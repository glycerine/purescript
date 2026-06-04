import { init, loadAllExterns } from './decode_externs.mjs';
import fs from 'fs';

const OUTPUT = './output';

async function main() {
  await init();
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
  const Identity = await import(`${OUTPUT}/Data.Identity/index.js`);
  const ListNE = await import(`${OUTPUT}/Data.List.NonEmpty/index.js`);
  const CST_Errors = await import(`${OUTPUT}/Language.PureScript.CST.Errors/index.js`);
  const Except = await import(`${OUTPUT}/Control.Monad.Except/index.js`);

  const monadErrorWriterT = WriterT.monadErrorWriterT(Errors.monoidMultipleErrors)(
    ExceptT.monadErrorExceptT(Identity.monadIdentity)
  );
  const monadWriterWriterT = WriterT.monadWriterWriterT(Errors.monoidMultipleErrors)(
    ExceptT.monadExceptT(Identity.monadIdentity)
  );
  const rebuildModuleWithCoreFn = Make.rebuildModuleWithCoreFn(monadErrorWriterT)(monadWriterWriterT);

  const externs = await loadAllExterns('/tmp/ps_full_compare/stdlib_cache');
  console.log(`Loaded ${externs.length} externs`);

  const src = fs.readFileSync('/mnt/oldrog/home/jaten/go/src/github.com/purescript/purescript/tests/purs/passing/1570.purs', 'utf8');
  const tokens = Lexer.lex(src);
  const parsed = Parser.parseModule(tokens);
  const cstMod = parsed.value0.value0.resFull.value1;
  const astMod = Convert.convertModule('1570.purs')(cstMod.value0);
  const result = Except.runExcept(WriterT.runWriterT(rebuildModuleWithCoreFn(externs)(astMod)));
  
  if (result instanceof Either.Left) {
    console.log('compile error:', JSON.stringify(result.value0).slice(0, 200));
    return;
  }
  
  const corefnMod = result.value0.value0.value1;
  const json = CoreFnJSON.moduleToJSON('0.15.16')(corefnMod);
  
  const ref = JSON.parse(fs.readFileSync('/tmp/ps_full_compare/reference/1570/corefn.json', 'utf8'));
  
  function sortKeys(v) {
    if (Array.isArray(v)) return v.map(sortKeys);
    if (v !== null && typeof v === 'object') {
      const s = {};
      for (const k of Object.keys(v).sort()) s[k] = sortKeys(v[k]);
      return s;
    }
    return v;
  }
  
  const cleanRef = Object.fromEntries(Object.entries(ref).filter(([k]) => k !== 'modulePath'));
  const cleanJson = Object.fromEntries(Object.entries(json).filter(([k]) => k !== 'modulePath'));
  
  const sRef = JSON.stringify(sortKeys(cleanRef));
  const sJson = JSON.stringify(sortKeys(cleanJson));
  
  if (sRef === sJson) {
    console.log('MATCH!');
  } else {
    console.log('DIFF');
    // find first difference
    const refKeys = Object.keys(cleanRef);
    const jsonKeys = Object.keys(cleanJson);
    for (const k of refKeys) {
      const a = JSON.stringify(sortKeys(cleanRef[k]));
      const b = JSON.stringify(sortKeys(cleanJson[k]));
      if (a !== b) {
        console.log('  Different key:', k);
        if (k === 'decls') {
          console.log('  Ref decls count:', cleanRef.decls.length);
          console.log('  Port decls count:', cleanJson.decls.length);
          // show first differing decl
          for (let i = 0; i < Math.min(cleanRef.decls.length, cleanJson.decls.length); i++) {
            const ar = JSON.stringify(sortKeys(cleanRef.decls[i]));
            const aj = JSON.stringify(sortKeys(cleanJson.decls[i]));
            if (ar !== aj) {
              console.log(`  First diff at decl[${i}]:`);
              console.log('  REF:', ar.slice(0,200));
              console.log('  PORT:', aj.slice(0,200));
              break;
            }
          }
        }
      }
    }
  }
}

main().catch(e => { console.error(e); process.exit(1); });
