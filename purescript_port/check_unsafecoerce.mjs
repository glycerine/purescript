import { init, loadAllExterns } from './decode_externs.mjs';
import fs from 'fs';
const OUTPUT = './output';
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
  
  const src = fs.readFileSync('/mnt/oldrog/home/jaten/go/src/github.com/purescript/purescript/tests/purs/passing/UnsafeCoerce.purs', 'utf8');
  const tokens = Lexer.lex(src);
  const parsed = Parser.parseModule(tokens);
  const cstMod = parsed.value0.value0.resFull.value1.value0;
  const astMod = Convert.convertModule('UnsafeCoerce.purs')(cstMod);
  const result = Except.runExcept(WriterT.runWriterT(rebuildModuleWithCoreFn(externs)(astMod)));
  const corefnMod = result.value0.value0.value1;
  const json = CoreFnJSON.moduleToJSON('0.15.16')(corefnMod);
  const ref = JSON.parse(fs.readFileSync('/tmp/ps_full_compare/reference/UnsafeCoerce/corefn.json', 'utf8'));
  
  console.log('REF decls:');
  for (const d of ref.decls) {
    const ann = d.annotation.sourceSpan;
    console.log(`  ${d.identifier}: span [${ann.start}]-[${ann.end}]`);
  }
  console.log('PORT decls:');
  for (const d of json.decls) {
    const ann = d.annotation.sourceSpan;
    console.log(`  ${d.identifier}: span [${ann.start}]-[${ann.end}]`);
  }
}
main().catch(e => { console.error(e); process.exit(1); });
