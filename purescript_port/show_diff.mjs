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
  const WriterT = await import(`${OUTPUT}/Control.Monad.Writer.Trans/index.js`);
  const ExceptT = await import(`${OUTPUT}/Control.Monad.Except.Trans/index.js`);
  const Except = await import(`${OUTPUT}/Control.Monad.Except/index.js`);
  const Identity = await import(`${OUTPUT}/Data.Identity/index.js`);
  const ListNE = await import(`${OUTPUT}/Data.List.NonEmpty/index.js`);
  const CST_Errors = await import(`${OUTPUT}/Language.PureScript.CST.Errors/index.js`);
  const monadErrorWriterT = WriterT.monadErrorWriterT(Errors.monoidMultipleErrors)(ExceptT.monadErrorExceptT(Identity.monadIdentity));
  const monadWriterWriterT = WriterT.monadWriterWriterT(Errors.monoidMultipleErrors)(ExceptT.monadExceptT(Identity.monadIdentity));
  const rebuildModuleWithCoreFn = Make.rebuildModuleWithCoreFn(monadErrorWriterT)(monadWriterWriterT);
  return { Make, Lexer, Parser, Convert, CoreFnJSON, Errors, Either, Tuple, Maybe, WriterT, ExceptT, Except, Identity, ListNE, CST_Errors, rebuildModuleWithCoreFn };
}

function compileModule(PS, externs, src, filename) {
  const tokens = PS.Lexer.lex(src);
  const parsed = PS.Parser.parseModule(tokens);
  if (parsed instanceof PS.Either.Left) return { ok: false, error: 'parse: ' + PS.CST_Errors.prettyPrintError(PS.ListNE.head(parsed.value0)) };
  const cstMod = parsed.value0.value0.resFull.value1;
  if (cstMod instanceof PS.Either.Left) return { ok: false, error: 'parse-full: ' + PS.CST_Errors.prettyPrintError(PS.ListNE.head(cstMod.value0)) };
  const astMod = PS.Convert.convertModule(filename)(cstMod.value0);
  const result = PS.Except.runExcept(PS.WriterT.runWriterT(PS.rebuildModuleWithCoreFn(externs)(astMod)));
  if (result instanceof PS.Either.Left) return { ok: false, error: 'compile: ' + JSON.stringify(result.value0).slice(0,200) };
  const corefnMod = result.value0.value0.value1;
  const json = PS.CoreFnJSON.moduleToJSON('0.15.16')(corefnMod);
  return { ok: true, json };
}

function sortKeys(v) {
  if (Array.isArray(v)) return v.map(sortKeys);
  if (v !== null && typeof v === 'object') {
    const sorted = {};
    for (const k of Object.keys(v).sort()) sorted[k] = sortKeys(v[k]);
    return sorted;
  }
  return v;
}

const testName = process.argv[2];
const refDir = '/tmp/ps_full_compare/reference';
const testDir = '/mnt/oldrog/home/jaten/go/src/github.com/purescript/purescript/tests/purs/passing';

await init();
const PS = await loadPS();
const externs = await loadAllExterns('/tmp/ps_full_compare/stdlib_cache');

const refPath = path.join(refDir, testName, 'corefn.json');
const refJson = JSON.parse(fs.readFileSync(refPath, 'utf8'));
const src = fs.readFileSync(path.join(testDir, testName + '.purs'), 'utf8');
const result = compileModule(PS, externs, src, testName + '.purs');
if (!result.ok) { console.log('FAIL:', result.error); process.exit(1); }

const clean = j => {
  const o = Object.fromEntries(Object.entries(j).filter(([k]) => k !== 'modulePath'));
  return JSON.stringify(sortKeys(o), null, 2);
};
fs.writeFileSync('/tmp/ref.json', clean(refJson));
fs.writeFileSync('/tmp/port.json', clean(result.json));
console.log('Written to /tmp/ref.json and /tmp/port.json');
