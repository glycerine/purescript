import { init, loadAllExterns } from './decode_externs.mjs';
import fs from 'fs';
const OUTPUT = './output';
const TESTDIR = '/mnt/oldrog/home/jaten/go/src/github.com/purescript/purescript/tests/purs/passing';
const REFDIR = '/tmp/ps_full_compare/reference';

function sortKeys(v) {
  if (Array.isArray(v)) return v.map(sortKeys);
  if (v !== null && typeof v === 'object') {
    const s = {};
    for (const k of Object.keys(v).sort()) s[k] = sortKeys(v[k]);
    return s;
  }
  return v;
}

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

  const diffs = ['2795','2958','3238','4229','CaseStatement','DuplicateProperties','NakedConstraint','ParseTypeInt','PartialFunction','PolykindGeneralization','RunFnInline','UnsafeCoerce'];

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
    
    const cleanRef = Object.fromEntries(Object.entries(ref).filter(([k]) => k !== 'modulePath'));
    const cleanJson = Object.fromEntries(Object.entries(json).filter(([k]) => k !== 'modulePath'));
    const rs = JSON.stringify(sortKeys(cleanRef));
    const ps = JSON.stringify(sortKeys(cleanJson));
    
    // Find first char diff
    let pos = -1;
    for (let j = 0; j < Math.min(rs.length, ps.length); j++) {
      if (rs[j] !== ps[j]) { pos = j; break; }
    }
    if (pos >= 0) {
      console.log(`${name}: first diff at pos ${pos}`);
      console.log(`  REF:  ...${rs.slice(Math.max(0,pos-40), pos+60)}...`);
      console.log(`  PORT: ...${ps.slice(Math.max(0,pos-40), pos+60)}...`);
    } else if (rs.length !== ps.length) {
      console.log(`${name}: length diff ref=${rs.length} port=${ps.length}`);
      console.log(`  REF  end:  ...${rs.slice(-80)}`);
      console.log(`  PORT end:  ...${ps.slice(-80)}`);
    }
  }
}

main().catch(e => { console.error(e); process.exit(1); });
