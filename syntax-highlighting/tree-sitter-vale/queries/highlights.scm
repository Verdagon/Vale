; Vale tree-sitter highlights
; Used by editors that consume tree-sitter grammars (Neovim, Zed, Helix, etc.)

; ── Keywords ──────────────────────────────────────────────────────────────────

(keyword_attribute) @keyword.modifier
(extern_attribute "extern" @keyword.modifier)
(macro_attribute) @attribute

["func" "struct" "interface" "impl" "import" "export"] @keyword

["if" "else" "while" "foreach" "in" "break" "return"
 "set" "destruct" "unlet" "as"] @keyword.control

["own" "borrow" "weak" "share"] @keyword.modifier
["mut" "imm"] @keyword.modifier
["and" "or" "not" "mod"] @keyword.operator
["virtual" "self" "this"] @keyword

; ── Types ─────────────────────────────────────────────────────────────────────

(primitive_type) @type.builtin
(metatype) @type.builtin
(named_type (identifier) @type)
(type_param (identifier) @type.parameter)
(region_param (region) @type.parameter)

; ── Functions ─────────────────────────────────────────────────────────────────

(function_definition name: (identifier) @function)
(function_definition name: (operator_name) @operator)

(call_expression function: (identifier) @function.call)
(method_call_expression method: (identifier) @function.method.call)

; ── Definitions ───────────────────────────────────────────────────────────────

(struct_definition name: (identifier) @type)
(interface_definition name: (identifier) @type)

(field_definition name: (identifier) @variable.member)
(named_parameter name: (identifier) @variable.parameter)

; ── Literals ──────────────────────────────────────────────────────────────────

(string_literal) @string
(multiline_string_literal) @string
(string_escape) @string.escape
(string_interpolation) @string.special

(integer_literal) @number
(float_literal) @number.float
(boolean_literal) @boolean

; ── Comments ──────────────────────────────────────────────────────────────────

(line_comment) @comment.line
(chevron_comment) @comment.block
(ellipsis) @comment

; ── Regions ───────────────────────────────────────────────────────────────────

(region) @type.parameter

; ── Operators ─────────────────────────────────────────────────────────────────

["+" "-" "*" "/" "==" "!=" "===" "<" ">" "<=" ">=" "<=>" ".." "="] @operator

; ── Punctuation ───────────────────────────────────────────────────────────────

["(" ")" "[" "]" "{" "}" "<" ">"] @punctuation.bracket
["," "." ";"] @punctuation.delimiter
