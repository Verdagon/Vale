#!/usr/bin/env node
/**
 * tokenize.js — tokenize a Vale source file using the TextMate grammar.
 *
 * Usage: node tokenize.js <path/to/file.vale>
 *
 * Output: one token per line, format:
 *   <line>:<start>-<end>  <scope1> <scope2> ...
 *
 * The output is deterministic and suitable for snapshot testing.
 */

'use strict';

const path = require('path');
const fs = require('fs');
const { createRequire } = require('module');

async function main() {
  const inputFile = process.argv[2];
  if (!inputFile) {
    console.error('Usage: node tokenize.js <file.vale>');
    process.exit(1);
  }

  const source = fs.readFileSync(inputFile, 'utf8');

  // Load vscode-oniguruma and vscode-textmate
  const vsctm = require('vscode-textmate');
  const oniguruma = require('vscode-oniguruma');

  // Path to the oniguruma WASM binary shipped with vscode-oniguruma
  const onigWasmPath = require.resolve('vscode-oniguruma/release/onig.wasm');
  const wasmBin = fs.readFileSync(onigWasmPath).buffer;

  await oniguruma.loadWASM(wasmBin);

  const registry = new vsctm.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (sources) => oniguruma.createOnigScanner(sources),
      createOnigString: (str) => oniguruma.createOnigString(str),
    }),
    loadGrammar: async (scopeName) => {
      if (scopeName === 'source.vale') {
        const grammarPath = path.join(__dirname, 'vale.tmLanguage.json');
        const content = fs.readFileSync(grammarPath, 'utf8');
        return vsctm.parseRawGrammar(content, grammarPath);
      }
      return null;
    },
  });

  const grammar = await registry.loadGrammar('source.vale');
  if (!grammar) {
    console.error('Failed to load Vale grammar');
    process.exit(1);
  }

  const lines = source.split('\n');
  let ruleStack = vsctm.INITIAL;

  for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
    const line = lines[lineIndex];
    const lineResult = grammar.tokenizeLine(line, ruleStack);

    for (const token of lineResult.tokens) {
      const scopes = token.scopes.join(' ');
      const col_start = token.startIndex;
      const col_end = token.endIndex;
      const lineNum = lineIndex + 1;
      console.log(`${lineNum}:${col_start}-${col_end}\t${scopes}`);
    }

    ruleStack = lineResult.ruleStack;
  }
}

main().catch(err => {
  console.error(err);
  process.exit(1);
});
