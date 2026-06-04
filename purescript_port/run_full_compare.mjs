/**
 * Full CoreFn comparison test using stdlib externs.
 *
 * For each test file in tests/purs/passing/ that has a reference CoreFn JSON
 * in /tmp/ps_full_compare/reference/, compile it using the PS port with stdlib
 * externs and compare the output (ignoring modulePath).
 */

import { init, loadAllExterns } from './decode_externs.mjs';
import fs from 'fs';
import path from 'path';

const OUTPUT = './output';

async function loadPS() {
  const [Make, Lexer, Parser, Convert, CoreFnJSON, Errors, Either, Tuple, Maybe] = await Promise.all([
    import(`${OUTPUT}/Language.PureScript.Make/index.js`),
    import(`${OUTPUT}/Language.PureScript.CST.Lexer/index.js`),
    import(`${OUTPUT}/Language.PureScript.CST.Parser/index.js`),
    import(`${OUTPUT}/Language.PureScript.CST.Convert/index.js`),
    import(`${OUTPUT}/Language.PureScript.CoreFn.ToJSON/index.js`),
    import(`${OUTPUT}/Language.PureScript.Errors/index.js`),
    import(`${OUTPUT}/Data.Either/index.js`),
    import(`${OUTPUT}/Data.Tuple/index.js`),
    import(`${OUTPUT}/Data.Maybe/index.js`),
  ]);
  // Set up the monad stack for rebuildModuleWithCoreFn
  const WriterT = await import(`${OUTPUT}/Control.Monad.Writer.Trans/index.js`);
  const ExceptT = await import(`${OUTPUT}/Control.Monad.Except.Trans/index.js`);
  const Except = await import(`${OUTPUT}/Control.Monad.Except/index.js`);
  const Identity = await import(`${OUTPUT}/Data.Identity/index.js`);
  const ListNE = await import(`${OUTPUT}/Data.List.NonEmpty/index.js`);
  const CST_Errors = await import(`${OUTPUT}/Language.PureScript.CST.Errors/index.js`);

  const monadErrorWriterT = WriterT.monadErrorWriterT(Errors.monoidMultipleErrors)(
    ExceptT.monadErrorExceptT(Identity.monadIdentity)
  );
  const monadWriterWriterT = WriterT.monadWriterWriterT(Errors.monoidMultipleErrors)(
    ExceptT.monadExceptT(Identity.monadIdentity)
  );
  const rebuildModuleWithCoreFn = Make.rebuildModuleWithCoreFn(monadErrorWriterT)(monadWriterWriterT);

  return {
    Make, Lexer, Parser, Convert, CoreFnJSON, Errors,
    Either, Tuple, Maybe, WriterT, ExceptT, Except, Identity, ListNE,
    CST_Errors, rebuildModuleWithCoreFn
  };
}

function compileModule(PS, externs, src, filename) {
  const tokens = PS.Lexer.lex(src);
  const parsed = PS.Parser.parseModule(tokens);

  if (parsed instanceof PS.Either.Left) {
    const err = PS.CST_Errors.prettyPrintError(PS.ListNE.head(parsed.value0));
    return { ok: false, error: 'parse: ' + err };
  }

  const cstMod = parsed.value0.value0.resFull.value1;
  if (cstMod instanceof PS.Either.Left) {
    const err = PS.CST_Errors.prettyPrintError(PS.ListNE.head(cstMod.value0));
    return { ok: false, error: 'parse-full: ' + err };
  }

  const astMod = PS.Convert.convertModule(filename)(cstMod.value0);
  const result = PS.Except.runExcept(
    PS.WriterT.runWriterT(PS.rebuildModuleWithCoreFn(externs)(astMod))
  );

  if (result instanceof PS.Either.Left) {
    const err = result.value0;
    let errMsg = 'compile error';
    try {
      // err is Array of ErrorMessage, each has value0=hints, value1=SimpleErrorMessage
      if (Array.isArray(err) && err.length > 0) {
        const em = err[0];
        const msgCtor = em.value1.constructor.name;
        const msgVal = JSON.stringify(em.value1).slice(0, 200);
        errMsg = `${msgCtor}: ${msgVal}`;
      } else {
        errMsg = JSON.stringify(err).slice(0, 200);
      }
    } catch (_) {
      errMsg = String(err).slice(0, 200);
    }
    return { ok: false, error: 'compile: ' + errMsg };
  }

  // result is Right (Tuple (Tuple _ corefnMod) _)
  const corefnMod = result.value0.value0.value1;
  const json = PS.CoreFnJSON.moduleToJSON('0.15.16')(corefnMod);
  return { ok: true, json };
}

function jsonEqual(a, b) {
  // Compare ignoring modulePath
  const cleanA = Object.fromEntries(Object.entries(a).filter(([k]) => k !== 'modulePath'));
  const cleanB = Object.fromEntries(Object.entries(b).filter(([k]) => k !== 'modulePath'));
  return JSON.stringify(cleanA) === JSON.stringify(cleanB);
}

function findTestFile(refName, testDir) {
  // refName could be "1110" (single file) or "1110" in a subdirectory
  const single = path.join(testDir, refName + '.purs');
  if (fs.existsSync(single)) return { type: 'single', file: single };
  const dir = path.join(testDir, refName);
  if (fs.existsSync(dir) && fs.statSync(dir).isDirectory()) {
    const files = fs.readdirSync(dir).filter(f => f.endsWith('.purs'));
    return { type: 'multi', dir, files };
  }
  return null;
}

async function main() {
  console.log('=== Full CoreFn Comparison Test (PS Port vs Haskell Reference) ===\n');

  console.log('Loading modules...');
  await init();
  const PS = await loadPS();
  console.log('Loading stdlib externs...');
  const externs = await loadAllExterns('/tmp/ps_full_compare/stdlib_cache');
  console.log(`Loaded ${externs.length} stdlib externs.\n`);

  const refDir = '/tmp/ps_full_compare/reference';
  const testDir = '/mnt/oldrog/home/jaten/go/src/github.com/purescript/purescript/tests/purs/passing';
  const refDirs = fs.readdirSync(refDir);

  let match = 0, diff = 0, noFile = 0, compFail = 0, parseRefFail = 0;
  const diffs = [];
  const failures = [];

  for (const refName of refDirs) {
    const refPath = path.join(refDir, refName, 'corefn.json');
    if (!fs.existsSync(refPath)) continue;

    const refRaw = fs.readFileSync(refPath, 'utf8');
    let refJson;
    try {
      refJson = JSON.parse(refRaw);
    } catch (e) {
      parseRefFail++;
      continue;
    }

    const testInfo = findTestFile(refName, testDir);
    if (!testInfo) {
      noFile++;
      continue;
    }

    if (testInfo.type === 'single') {
      const src = fs.readFileSync(testInfo.file, 'utf8');
      const filename = refName + '.purs';
      const result = compileModule(PS, externs, src, filename);
      if (!result.ok) {
        compFail++;
        failures.push({ name: refName, error: result.error });
        continue;
      }
      if (jsonEqual(refJson, result.json)) {
        match++;
      } else {
        diff++;
        diffs.push(refName);
      }
    } else {
      // Multi-file test: compile each file separately then compare
      // For now, skip multi-file tests
      noFile++;
    }
  }

  console.log('=== Results ===');
  console.log(`MATCH:   ${match}`);
  console.log(`DIFF:    ${diff}`);
  console.log(`FAIL:    ${compFail}`);
  console.log(`NO FILE: ${noFile}`);
  console.log(`Total:   ${match + diff + compFail + noFile}`);

  if (diffs.length > 0) {
    console.log('\nDIFF modules (first 20):');
    diffs.slice(0, 20).forEach(d => console.log('  ' + d));
  }

  if (failures.length > 0) {
    console.log('\nFAIL modules (first 20):');
    failures.slice(0, 20).forEach(f => console.log(`  ${f.name}: ${String(f.error).slice(0, 120)}`));
  }
}

main().catch(e => {
  console.error('Fatal:', e);
  process.exit(1);
});
