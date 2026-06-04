import { init, loadAllExterns } from './decode_externs.mjs';
import fs from 'fs';
const OUTPUT = './output';
const TESTDIR = '/mnt/oldrog/home/jaten/go/src/github.com/purescript/purescript/tests/purs/passing';
const REFDIR = '/tmp/ps_full_compare/reference';

async function main() {
  await init();
  const [Make, Lexer, Parser, Convert, CoreFnJSON, Errors, Either] = await Promise.all([
    import(`${OUTPUT}/Language.PureScript.Make/index.js`),
    import(`${OUTPUT}/Language.PureScript.CST.Lexer/index.js`),
    import(`${OUTPUT}/Language.PureScript.CST.Parser/index.js`),
    import(`${OUTPUT}/Language.PureScript.CST.Convert/index.js`),
    import(`${OUTPUT}/Language.PureScript.CoreFn.ToJSON/index.js`),
    import(`${OUTPUT}/Language.PureScript.Errors/index.js`),
    import(`${OUTPUT}/Data.Either/index.js`),
  ]);
  const WriterT = await import(`${OUTPUT}/Control.Monad.Writer.Trans/index.js`);
  const ExceptT = await import(`${OUTPUT}/Control.Monad.Except.Trans/index.js`);
  const Identity = await import(`${OUTPUT}/Data.Identity/index.js`);
  const Except = await import(`${OUTPUT}/Control.Monad.Except/index.js`);
  const monadErrorWriterT = WriterT.monadErrorWriterT(Errors.monoidMultipleErrors)(ExceptT.monadErrorExceptT(Identity.monadIdentity));
  const monadWriterWriterT = WriterT.monadWriterWriterT(Errors.monoidMultipleErrors)(ExceptT.monadExceptT(Identity.monadIdentity));
  const rebuildModuleWithCoreFn = Make.rebuildModuleWithCoreFn(monadErrorWriterT)(monadWriterWriterT);
  const externs = await loadAllExterns('/tmp/ps_full_compare/stdlib_cache');

  const diffs = ['2795','2958','3238','4229','CaseStatement','DuplicateProperties','LargeSumType','NakedConstraint','OneConstructor','ParseTypeInt','PartialFunction','PolykindGeneralization','RunFnInline','UnderscoreIdent','UnsafeCoerce'];

  function sortKeys(v) {
    if (Array.isArray(v)) return v.map(sortKeys);
    if (v !== null && typeof v === 'object') {
      const s = {};
      for (const k of Object.keys(v).sort()) s[k] = sortKeys(v[k]);
      return s;
    }
    return v;
  }

  for (const name of diffs) {
    const src = fs.readFileSync(`${TESTDIR}/${name}.purs`, 'utf8');
    const tokens = Lexer.lex(src);
    const parsed = Parser.parseModule(tokens);
    const cstMod = parsed.value0.value0.resFull.value1.value0;
    const astMod = Convert.convertModule(`${name}.purs`)(cstMod);
    const result = Except.runExcept(WriterT.runWriterT(rebuildModuleWithCoreFn(externs)(astMod)));
    const corefnMod = result.value0.value0.value1;
    const json = CoreFnJSON.moduleToJSON('0.15.16')(corefnMod);
    const ref = JSON.parse(fs.readFileSync(`${REFDIR}/${name}/corefn.json`, 'utf8'));
    
    const keys = ['builtWith','comments','decls','exports','foreign','imports','moduleName','version'];
    let diffKeys = [];
    for (const k of keys) {
      if (k === 'modulePath') continue;
      const a = JSON.stringify(sortKeys(ref[k]));
      const b = JSON.stringify(sortKeys(json[k]));
      if (a !== b) diffKeys.push(k);
    }
    console.log(`${name}: DIFF in ${diffKeys.join(', ')}`);
    if (diffKeys.includes('decls')) {
      const refIds = ref.decls.map(d => d.identifier || '(rec)');
      const portIds = json.decls.map(d => d.identifier || '(rec)');
      console.log(`  REF  decls: ${refIds.join(', ')}`);
      console.log(`  PORT decls: ${portIds.join(', ')}`);
    }
    if (diffKeys.includes('imports')) {
      const refImps = ref.imports.map(i => i.moduleName.join('.'));
      const portImps = json.imports.map(i => i.moduleName.join('.'));
      console.log(`  REF  imports: ${refImps.join(', ')}`);
      console.log(`  PORT imports: ${portImps.join(', ')}`);
    }
  }
}

main().catch(e => { console.error(e); process.exit(1); });
