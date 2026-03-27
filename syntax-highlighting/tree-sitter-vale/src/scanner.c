/**
 * Custom external scanner for Vale language.
 *
 * Handles:
 * - String literal delimiters: single " and triple """
 * - String content scanning (stops at {, \, or end delimiter)
 * - String interpolation: { inside a string (not followed by \n) opens interp
 *   Matching } closes it (tracking brace depth for nested braces)
 *
 * Token types MUST match the `externals` array in grammar.js:
 *   0  _string_start            (")
 *   1  _multiline_string_start  (""")
 *   2  _string_content          (chars inside a string)
 *   3  _string_end              (" or """ matching the opener)
 *   4  _interp_open             ({ inside string, not followed by \n)
 *   5  _interp_close            (} that ends an interpolation)
 */

#include "tree_sitter/parser.h"
#include <stdlib.h>
#include <string.h>

/* ── Token indices ──────────────────────────────────────────────────────── */
enum {
  TOK_STRING_START = 0,
  TOK_MULTILINE_STRING_START,
  TOK_STRING_CONTENT,
  TOK_STRING_END,
  TOK_INTERP_OPEN,
  TOK_INTERP_CLOSE,
  TOK__COUNT
};

/* ── Context stack ──────────────────────────────────────────────────────── */
/* Context types pushed onto the stack: */
#define CTX_STRING_SINGLE 0   /* inside "..."     */
#define CTX_STRING_TRIPLE 1   /* inside """..."""  */
#define CTX_INTERP        2   /* inside {interp}  */

#define MAX_DEPTH 64

typedef struct {
  uint8_t ctx_type[MAX_DEPTH];
  uint8_t brace_depth[MAX_DEPTH]; /* extra braces inside CTX_INTERP contexts */
  int     top;                    /* index of top context, -1 if empty       */
} Scanner;

/* ── Helpers ────────────────────────────────────────────────────────────── */

static bool in_string_ctx(const Scanner *s) {
  return s->top >= 0 &&
    (s->ctx_type[s->top] == CTX_STRING_SINGLE ||
     s->ctx_type[s->top] == CTX_STRING_TRIPLE);
}

static bool is_triple(const Scanner *s) {
  return s->top >= 0 && s->ctx_type[s->top] == CTX_STRING_TRIPLE;
}

static bool in_interp_ctx(const Scanner *s) {
  return s->top >= 0 && s->ctx_type[s->top] == CTX_INTERP;
}

static bool in_any_string(const Scanner *s) {
  return s->top >= 0;
}

static void push(Scanner *s, uint8_t ctx) {
  if (s->top + 1 < MAX_DEPTH) {
    s->top++;
    s->ctx_type[s->top]    = ctx;
    s->brace_depth[s->top] = 0;
  }
}

static void pop(Scanner *s) {
  if (s->top >= 0) s->top--;
}

/* ── Lifecycle ──────────────────────────────────────────────────────────── */

void *tree_sitter_vale_external_scanner_create(void) {
  Scanner *s = (Scanner *)calloc(1, sizeof(Scanner));
  s->top = -1;
  return s;
}

void tree_sitter_vale_external_scanner_destroy(void *payload) {
  free(payload);
}

unsigned tree_sitter_vale_external_scanner_serialize(void *payload,
                                                      char *buffer) {
  Scanner *s = (Scanner *)payload;
  /* depth is stored as (top+1) so -1 maps to 0 (fits in uint8) */
  unsigned n = 0;
  buffer[n++] = (char)(uint8_t)(s->top + 1);
  for (int i = 0; i <= s->top && i < MAX_DEPTH &&
                  n + 2 <= TREE_SITTER_SERIALIZATION_BUFFER_SIZE; i++) {
    buffer[n++] = (char)s->ctx_type[i];
    buffer[n++] = (char)s->brace_depth[i];
  }
  return n;
}

void tree_sitter_vale_external_scanner_deserialize(void *payload,
                                                    const char *buffer,
                                                    unsigned length) {
  Scanner *s = (Scanner *)payload;
  s->top = -1;
  if (length == 0) return;
  s->top = (int)(uint8_t)buffer[0] - 1;
  for (int i = 0; i <= s->top && i < MAX_DEPTH && 1 + 2*i + 1 < (int)length; i++) {
    s->ctx_type[i]    = (uint8_t)buffer[1 + 2*i];
    s->brace_depth[i] = (uint8_t)buffer[2 + 2*i];
  }
}

/* ── Main scan ──────────────────────────────────────────────────────────── */

bool tree_sitter_vale_external_scanner_scan(void *payload,
                                             TSLexer *lexer,
                                             const bool *valid_symbols) {
  Scanner *s = (Scanner *)payload;

  /* ── (1) STRING_START / MULTILINE_STRING_START ─────────────────────── */
  /* Attempt to match a string open delimiter.  We check for both tokens   */
  /* in a single pass since they share the initial " character.             */
  /* NOTE: We skip leading whitespace ourselves (advance skip=true) because */
  /* tree-sitter doesn't re-call the scanner after handling an extra when   */
  /* the scanner previously returned false for whitespace at that position.  */
  if (!in_any_string(s) &&
      (valid_symbols[TOK_STRING_START] ||
       valid_symbols[TOK_MULTILINE_STRING_START])) {
    /* Skip whitespace so we see the actual next character. */
    while (lexer->lookahead == ' '  || lexer->lookahead == '\t' ||
           lexer->lookahead == '\n' || lexer->lookahead == '\r') {
      lexer->advance(lexer, true);
    }
  }
  if (!in_any_string(s) &&
      (valid_symbols[TOK_STRING_START] ||
       valid_symbols[TOK_MULTILINE_STRING_START]) &&
      lexer->lookahead == '"') {

    lexer->advance(lexer, false);       /* consume first "  */
    lexer->mark_end(lexer);             /* record end after first " */

    if (lexer->lookahead == '"') {
      lexer->advance(lexer, false);     /* consume second "  */
      if (lexer->lookahead == '"') {
        lexer->advance(lexer, false);   /* consume third "   */
        lexer->mark_end(lexer);         /* end after """ */
        if (valid_symbols[TOK_MULTILINE_STRING_START]) {
          push(s, CTX_STRING_TRIPLE);
          lexer->result_symbol = TOK_MULTILINE_STRING_START;
          return true;
        }
        return false;
      }
      /* Saw "" (two quotes): treat as an empty single-quoted string.
       * Emit STRING_START for the first ", leaving the second " to be
       * matched as STRING_END on the next call.
       * mark_end is still after the first ", which is what we want. */
    }
    /* Either we saw " followed by non-", OR we saw "" (and fell through). */
    if (valid_symbols[TOK_STRING_START]) {
      push(s, CTX_STRING_SINGLE);
      lexer->result_symbol = TOK_STRING_START;
      return true;
    }
    return false;
  }

  /* ── (2) STRING_END ─────────────────────────────────────────────────── */
  if (in_string_ctx(s) && valid_symbols[TOK_STRING_END] &&
      lexer->lookahead == '"') {
    if (is_triple(s)) {
      lexer->advance(lexer, false);
      if (lexer->lookahead == '"') {
        lexer->advance(lexer, false);
        if (lexer->lookahead == '"') {
          lexer->advance(lexer, false);
          lexer->mark_end(lexer);
          pop(s);
          lexer->result_symbol = TOK_STRING_END;
          return true;
        }
      }
      /* Saw one or two " inside a triple-quoted string: not the end yet.
       * Fall through so STRING_CONTENT can handle these chars. */
    } else {
      lexer->advance(lexer, false);
      lexer->mark_end(lexer);
      pop(s);
      lexer->result_symbol = TOK_STRING_END;
      return true;
    }
  }

  /* ── (3) INTERP_OPEN ────────────────────────────────────────────────── */
  /* { inside a string (not in an interpolation) starts interpolation,     */
  /* UNLESS it is immediately followed by a newline (Vale rule).            */
  if (in_string_ctx(s) && valid_symbols[TOK_INTERP_OPEN] &&
      lexer->lookahead == '{') {
    lexer->advance(lexer, false);
    int32_t next = lexer->lookahead;
    if (next == '\n' || next == '\r') {
      /* { followed by newline is a literal { — treat as string content */
      lexer->mark_end(lexer);
      lexer->result_symbol = TOK_STRING_CONTENT;
      return true;
    }
    /* Valid interpolation start */
    lexer->mark_end(lexer);
    push(s, CTX_INTERP);
    lexer->result_symbol = TOK_INTERP_OPEN;
    return true;
  }

  /* ── (4) INTERP_CLOSE ───────────────────────────────────────────────── */
  /* } that matches the interpolation's opening {.  We track extra braces  */
  /* that appear inside the interpolated expression.                         */
  if (in_interp_ctx(s) && valid_symbols[TOK_INTERP_CLOSE]) {
    if (lexer->lookahead == '}') {
      if (s->brace_depth[s->top] > 0) {
        /* An extra { was opened inside the interpolation; this } closes it. */
        s->brace_depth[s->top]--;
        /* Don't emit INTERP_CLOSE; let the grammar consume this as '}'. */
      } else {
        lexer->advance(lexer, false);
        lexer->mark_end(lexer);
        pop(s);
        lexer->result_symbol = TOK_INTERP_CLOSE;
        return true;
      }
    }
    /* Track extra { inside the interpolated expression */
    if (lexer->lookahead == '{') {
      if (s->brace_depth[s->top] < 255) s->brace_depth[s->top]++;
      /* Don't advance; let the grammar parse it normally. */
    }
  }

  /* ── (5) STRING_CONTENT ─────────────────────────────────────────────── */
  /* Scan characters that are literal string content: stop at escape        */
  /* sequences (handled by the grammar's string_escape rule), at { which    */
  /* may start interpolation, and at the closing delimiter.                  */
  if (in_string_ctx(s) && valid_symbols[TOK_STRING_CONTENT]) {
    bool triple_mode = is_triple(s);
    bool has_content = false;

    while (true) {
      int32_t c = lexer->lookahead;

      if (c == 0) break;           /* EOF                                    */
      if (c == '\\') break;        /* escape: let grammar rule handle it      */
      if (c == '{') break;         /* potential interpolation start           */

      if (c == '"') {
        if (triple_mode) {
          /* We might be at """, but we might also be at a lone " in content. */
          /* Peek ahead: consume this " but record mark_end after it so we    */
          /* can include it in content if it's not the end delimiter.          */
          lexer->advance(lexer, false);
          if (lexer->lookahead != '"') {
            /* Lone " inside triple-quoted string: it's content */
            has_content = true;
            lexer->mark_end(lexer);
            continue;
          }
          /* Saw "" — peek one more */
          lexer->advance(lexer, false);
          if (lexer->lookahead != '"') {
            /* "" inside triple-quoted string: it's content */
            has_content = true;
            lexer->mark_end(lexer);
            continue;
          }
          /* Found """ — this is the end delimiter. Don't include it. */
          break;
        } else {
          /* Inside single-quoted string: " is the end delimiter */
          break;
        }
      }

      lexer->advance(lexer, false);
      has_content = true;
      lexer->mark_end(lexer);
    }

    if (has_content) {
      lexer->result_symbol = TOK_STRING_CONTENT;
      return true;
    }
  }

  return false;
}
