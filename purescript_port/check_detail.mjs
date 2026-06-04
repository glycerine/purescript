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

  for (const name of ['CaseStatement', 'UnsafeCoerce', 'PolykindGeneralization']) {
    const src = fs.readFileSync(`${TESTDIR}/${name}.purs`, 'utf8');
    const tokens = Lexer.lex(src);
    const parsed = Parser.parseModule(tokens);
    const cstMod = parsed.value0.value0.resFull.value1.value0;
    const astMod = Convert.convertModule(`${name}.purs`)(cstMod);
    const result = Except.runExcept(WriterT.runWriterT(rebuildModuleWithCoreFn(externs)(astMod)));
    const corefnMod = result.value0.value0.value1;
    const json = CoreFnJSON.moduleToJSON('0.15.16')(corefnMod);
    const ref = JSON.parse(fs.readFileSync(`${REFDIR}/${name}/corefn.json`, 'utf8'));
    
    console.log(`\n=== ${name} ===`);
    // Find first differing decl
    const minLen = Math.min(ref.decls.length, json.decls.length);
    for (let i = 0; i < minLen; i++) {
      const rs = JSON.stringify(ref.decls[i]);
      const ps = JSON.stringify(json.decls[i]);
      if (rs !== ps) {
        const rid = ref.decls[i].identifier || ref.decls[i].bindType;
        const pid = json.decls[i].identifier || json.decls[i].bindType;
        console.log(`  decl[${i}] (ref:${rid}, port:${pid})`);
        // Find first character difference
        for (let j = 0; j < Math.min(rs.length, ps.length); j++) {
          if (rs[j] !== ps[j]) {
            console.log(`  REF  context: ...${rs.slice(Math.max(0,j-50), j+80)}...`);
            console.log(`  PORT context: ...${ps.slice(Math.max(0,j-50), j+80)}...`);
            break;
          }
        }
        break;
      }
    }
  }
}

main().catch(e => { console.error(e); process.exit(1); });
