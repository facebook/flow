/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

'use strict';

// Phase D1 / task #13 sub-test 2: per-node AST diff against hermes-parser.
//
// Parse a small set of representative source fixtures with both parsers and
// verify the AST shapes agree on the load-bearing fields. The contract-test
// suite (port of upstream's per-fixture tests) already covers wide
// per-feature snapshot parity; this suite is the cross-parser sanity check
// that proves we are actually delivering on "drop-in replacement" — i.e. the
// AST a downstream consumer sees from `flow-parser-oxidized.parse(src)` is
// shape-equivalent to `hermes-parser.parse(src)` for these representative
// shapes.
//
// Deviations that are *known and tracked* are skipped from the diff via the
// `allowKeys` helper or the `tolerate` per-fixture map. Each entry references
// the tracking task (#NN) so the gap is visible. Adding a new tolerate entry
// requires a corresponding tracked task — silent tolerance is forbidden.

const {parse: ourParse} = require('flow-parser-oxidized');
const {parse: upstreamParse} = require('hermes-parser');

// Loc/range carry source-position bookkeeping that doesn't bear on the AST
// shape contract. The contract-test suite already enforces loc presence;
// here we strip it so the diff focuses on structural fields.
const POSITION_KEYS = new Set(['loc', 'range', 'start', 'end']);

// Round-trip ESTree comparison helper. Walks both nodes in lockstep.
// Returns a list of diff descriptors (path + message) — empty list ⇒ match.
function diffNodes(ours, upstream, path, tolerate) {
  const diffs = [];
  const ourType = typeof ours;
  const upstreamType = typeof upstream;

  // Both are arrays.
  if (Array.isArray(ours) && Array.isArray(upstream)) {
    if (ours.length !== upstream.length) {
      diffs.push({
        path,
        message: `array length: ours=${ours.length} upstream=${upstream.length}`,
      });
      return diffs;
    }
    for (let i = 0; i < ours.length; i++) {
      diffs.push(...diffNodes(ours[i], upstream[i], `${path}[${i}]`, tolerate));
    }
    return diffs;
  }

  // Both are plain objects (AST nodes or sub-records like loc / regex).
  if (
    ours != null &&
    upstream != null &&
    ourType === 'object' &&
    upstreamType === 'object' &&
    !Array.isArray(ours) &&
    !Array.isArray(upstream)
  ) {
    // Apply per-node tolerate entries by node `type`. The tolerate map is
    // keyed on the *upstream* node type — the diff is keyed off upstream as
    // ground truth so we can flag when our type doesn't match.
    const tolerateForNode =
      typeof upstream.type === 'string' ? tolerate[upstream.type] : null;

    const ourKeys = new Set(
      Object.keys(ours).filter(k => !POSITION_KEYS.has(k)),
    );
    const upstreamKeys = new Set(
      Object.keys(upstream).filter(k => !POSITION_KEYS.has(k)),
    );

    // Keys upstream has that we don't — these are real omissions on our end.
    for (const k of upstreamKeys) {
      if (!ourKeys.has(k)) {
        if (tolerateForNode != null && tolerateForNode.missing.has(k)) {
          continue;
        }
        diffs.push({
          path: `${path}.${k}`,
          message: `key only in upstream`,
        });
      }
    }
    // Keys we have that upstream doesn't — these are over-emits on our end.
    for (const k of ourKeys) {
      if (!upstreamKeys.has(k)) {
        if (tolerateForNode != null && tolerateForNode.extra.has(k)) {
          continue;
        }
        diffs.push({
          path: `${path}.${k}`,
          message: `key only in ours`,
        });
      }
    }
    // Keys present in both — recurse.
    for (const k of upstreamKeys) {
      if (!ourKeys.has(k)) {
        continue;
      }
      if (tolerateForNode != null && tolerateForNode.ignoreValueOf.has(k)) {
        continue;
      }
      diffs.push(...diffNodes(ours[k], upstream[k], `${path}.${k}`, tolerate));
    }
    return diffs;
  }

  // Both are RegExp. Compare source + flags as a structural identity.
  if (ours instanceof RegExp && upstream instanceof RegExp) {
    if (ours.source !== upstream.source || ours.flags !== upstream.flags) {
      diffs.push({
        path,
        message: `regexp: ours=/${ours.source}/${ours.flags} upstream=/${upstream.source}/${upstream.flags}`,
      });
    }
    return diffs;
  }

  // Both are BigInt — compare via toString to dodge `===` quirks.
  if (ourType === 'bigint' && upstreamType === 'bigint') {
    if (ours.toString() !== upstream.toString()) {
      diffs.push({
        path,
        message: `bigint: ours=${ours.toString()} upstream=${upstream.toString()}`,
      });
    }
    return diffs;
  }

  // Primitive compare.
  if (ours !== upstream) {
    diffs.push({
      path,
      message: `value: ours=${JSON.stringify(ours)} upstream=${JSON.stringify(upstream)}`,
    });
  }
  return diffs;
}

// Build a tolerate entry helper: lists of keys we expect to differ at the
// given node type. `missing` = key only in upstream (we don't emit). `extra`
// = key only in ours (we over-emit). `ignoreValueOf` = key is in both but
// the value diverges in a documented way and we accept it.
function tol({
  missing = [],
  extra = [],
  ignoreValueOf = [],
}: $ReadOnly<{
  missing?: $ReadOnlyArray<string>,
  extra?: $ReadOnlyArray<string>,
  ignoreValueOf?: $ReadOnlyArray<string>,
}>) {
  return {
    missing: new Set(missing),
    extra: new Set(extra),
    ignoreValueOf: new Set(ignoreValueOf),
  };
}

// Documented per-node tolerances. Each entry MUST cite its tracking task
// (#NN) — silent tolerance is not acceptable. New tolerances require
// updating ADAPTER_GAPS.md.
const KNOWN_TOLERANCES = {
  // #30: Program.interpreter is always emitted by upstream (`null` for
  // sources without a hashbang) but the Rust serializer omits the slot.
  // - docblock: upstream Hermes doesn't carry docblock at the Program
  //   level; we add it as part of the adapter fixup chain
  //   (HermesToESTreeAdapter.mapProgram parity).
  // - errors: the Rust serializer always emits `errors: []` on Program;
  //   upstream's HermesAST does not surface a Program-level errors array.
  //   This is an over-emit on our side — tracked as #53.
  Program: tol({
    missing: ['interpreter'],
    extra: ['docblock', 'errors'],
  }),
  // #35: literal nodes — we attach `literalType` (synthesized from value /
  // side-fields) for Babel adapter parity; upstream's HermesAST has
  // separate node types per literal kind, so the merged ESTree shape ends
  // up without it.
  Literal: tol({extra: ['literalType']}),
  // #38: Identifier always emits `optional` and `typeAnnotation` slots in
  // Hermes ESTree shape but our adapter doesn't strip when null/false.
  // Listed as `extra` here because upstream's strict ESTree omits when
  // null. Same for PrivateIdentifier (#38).
  Identifier: tol({extra: ['optional', 'typeAnnotation']}),
  PrivateIdentifier: tol({extra: ['optional', 'typeAnnotation']}),
  // #37: ExpressionStatement.directive — our adapter strips when null
  // (see applyPerNodeFixups in src/index.js), upstream keeps the slot.
  // The dispositional choice tracked in #37 is to strip; the cross-parser
  // delta surfaces it as `missing` (key only in upstream).
  ExpressionStatement: tol({missing: ['directive']}),
  // #34: PropertyDefinition / MethodDefinition — upstream emits
  // `tsModifiers` (TypeScript-only modifier set, null for non-TS classes);
  // the Rust serializer doesn't write the slot. Listed as `missing` — the
  // existing #34 tracking already covers this side of the gap (along with
  // the over-emits of `override` / `tsAccessibility` that the JS adapter
  // already strips).
  PropertyDefinition: tol({missing: ['tsModifiers']}),
  MethodDefinition: tol({missing: ['tsModifiers']}),
  // #52: ExportSpecifier — the Rust serializer emits `exportKind`
  // (defaulted 'value') on each specifier; upstream does not carry the
  // per-specifier kind (the parent ExportNamedDeclaration's `exportKind`
  // is the source of truth).
  ExportSpecifier: tol({extra: ['exportKind']}),
  // #51: ArrowFunctionExpression.generator — over-emit. Arrow functions
  // can never be generators; upstream omits the slot, our adapter doesn't
  // strip the false default.
  ArrowFunctionExpression: tol({extra: ['generator']}),
};

function deepEqualWithTolerances(
  ours: mixed,
  upstream: mixed,
  tolerate: typeof KNOWN_TOLERANCES,
) {
  return diffNodes(ours, upstream, '$', tolerate);
}

const FIXTURES = {
  // Representative fixture set. Picked to cover the most adapter-touched
  // node families in the public AST shape.
  literal: `
    null;
    1;
    "s";
    true;
    /foo/g;
    4321n;
  `,
  chainExpression: `
    a?.b?.c;
    a?.();
    (a?.b).c;
  `,
  classPrivateProperty: `
    class C {
      #x = 1;
      #m() { return this.#x; }
    }
  `,
  exportNamespace: `
    export * as foo from 'mod';
    export {default as bar} from 'other';
  `,
  arrowFunction: `
    const f = (x) => x + 1;
    const g = async (a, b) => { await a; return b; };
  `,
  thisType: `
    class C {
      m(): this { return this; }
    }
    type T = This;
  `,
};

describe('Cross-parser AST diff vs hermes-parser', () => {
  for (const [name, source] of Object.entries(FIXTURES)) {
    test(`fixture ${name}: AST shape agrees with hermes-parser (mod tracked tolerances)`, () => {
      // Pass `flow: 'all'` to both parsers so type-grammar fixtures
      // (thisType, classPrivateProperty's potential type annotations)
      // parse without requiring an `@flow` pragma in the fixture text.
      // The default `flow: 'detect'` would surface as a parse error on
      // type syntax and the cross-parser diff would never run.
      const oursAst = ourParse(source, {flow: 'all'});
      const upstreamAst = upstreamParse(source, {flow: 'all'});
      const diffs = deepEqualWithTolerances(
        oursAst,
        upstreamAst,
        KNOWN_TOLERANCES,
      );
      // Fail with the full diff list so the failure mode is debuggable
      // without re-running. Empty diff list = strict shape parity (modulo
      // tracked tolerances) and the test passes.
      if (diffs.length > 0) {
        const formatted = diffs
          .map(d => `  ${d.path} — ${d.message}`)
          .join('\n');
        throw new Error(
          `Cross-parser AST diff for fixture "${name}" (${diffs.length} diffs):\n${formatted}\n\n` +
            `Add a tracked tolerance to KNOWN_TOLERANCES (with #NN tracking task) ` +
            `or fix the underlying gap in src/index.js / FlowParserDeserializer.js / ` +
            `the Rust serializer.`,
        );
      }
    });
  }
});
