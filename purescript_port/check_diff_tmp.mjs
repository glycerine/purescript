import { init, loadAllExterns } from './decode_externs.mjs';
import fs from 'fs';
const OUTPUT = './output';
await init();
const [Make, Lexer, Parser, Convert, CoreFnJSON, Errors, Either] = await Promise.all([
  import(OUTPUT + '/Language.PureScript.Make/index.js'),
  import(OUTPUT + '/Language.PureScript.CST.Lexer/index.js'),
  import(OUTPUT + '/Language.PureScript.CST.Parser/index.js'),
  import(OUTPUT + '/Language.PureScript.CST.Convert/index.js'),
  import(OUTPUT + '/Language.PureScript.CoreFn.ToJSON/index.js'),
  import(OUTPUT + '/Language.PureScript.Errors/index.js'),
  import(OUTPUT + '/Data.Either/index.js'),
]);
const WriterT = await import(OUTPUT + '/Control.Monad.Writer.Trans/index.js');
const ExceptT = await import(OUTPUT + '/Control.Monad.Except.Trans/index.js');
const Except = await import(OUTPUT + '/Control.Monad.Except/index.js');
const Identity = await import(OUTPUT + '/Data.Identity/index.js');
const monadErrorWriterT = WriterT.monadErrorWriterT(Errors.monoidMultipleErrors)(ExceptT.monadErrorExceptT(Identity.monadIdentity));
const monadWriterWriterT = WriterT.monadWriterWriterT(Errors.monoidMultipleErrors)(ExceptT.monadExceptT(Identity.monadIdentity));
const rebuildModuleWithCoreFn = Make.rebuildModuleWithCoreFn(monadErrorWriterT)(monadWriterWriterT);
const externs = await loadAllExterns('/tmp/ps_full_compare/stdlib_cache');

function sortKeys(v) {
  if (Array.isArray(v)) return v.map(sortKeys);
  if (v !== null && typeof v === 'object') {
    const sorted = {};
    for (const k of Object.keys(v).sort()) sorted[k] = sortKeys(v[k]);
    return sorted;
  }
  return v;
}

const modName = process.argv[2] || '1570';
const testDir = '/mnt/oldrog/home/jaten/go/src/github.com/purescript/purescript/tests/purs/passing';
const src = fs.readFileSync(`${testDir}/${modName}.purs`, 'utf8');
const tokens = Lexer.lex(src);
const parsed = Parser.parseModule(tokens);
const cstMod = parsed.value0.value0.resFull.value1;
const astMod = Convert.convertModule(`${modName}.purs`)(cstMod.value0);
const result = Except.runExcept(WriterT.runWriterT(rebuildModuleWithCoreFn(externs)(astMod)));
if (result instanceof Either.Left) {
  const err = result.value0;
  console.log('Error:', err[0]?.value1?.constructor?.name, JSON.stringify(err[0]?.value1).slice(0,300));
} else {
  const corefnMod = result.value0.value0.value1;
  const portJson = CoreFnJSON.moduleToJSON('0.15.16')(corefnMod);
  const refJson = JSON.parse(fs.readFileSync(`/tmp/ps_full_compare/reference/${modName}/corefn.json`, 'utf8'));
  const cleanPort = sortKeys(Object.fromEntries(Object.entries(portJson).filter(([k]) => k !== 'modulePath')));
  const cleanRef = sortKeys(Object.fromEntries(Object.entries(refJson).filter(([k]) => k !== 'modulePath')));
  if (JSON.stringify(cleanPort) === JSON.stringify(cleanRef)) {
    console.log('EXACT MATCH!');
  } else {
    for (const k of Object.keys(cleanRef)) {
      const ps = JSON.stringify(cleanPort[k]);
      const rs = JSON.stringify(cleanRef[k]);
      if (ps !== rs) {
        console.log('DIFF in key:', k);
        if (k === 'decls') {
          for (let i = 0; i < Math.max(cleanPort[k]?.length || 0, cleanRef[k]?.length || 0); i++) {
            const pd = JSON.stringify(cleanPort[k]?.[i]);
            const rd = JSON.stringify(cleanRef[k]?.[i]);
            if (pd !== rd) {
              const pid = cleanPort[k]?.[i]?.identifier || cleanPort[k]?.[i]?.[0]?.identifier;
              const rid = cleanRef[k]?.[i]?.identifier || cleanRef[k]?.[i]?.[0]?.identifier;
              console.log(`  Decl ${i}: port-id=${pid} ref-id=${rid}`);
              // show first diff char
              for (let j = 0; j < Math.min(pd?.length || 0, rd?.length || 0); j++) {
                if (pd[j] !== rd[j]) {
                  console.log(`  First diff at char ${j}:`);
                  console.log(`  PORT[${j-20}..${j+80}]: `, pd?.slice(Math.max(0,j-20), j+80));
                  console.log(`  REF [${j-20}..${j+80}]: `, rd?.slice(Math.max(0,j-20), j+80));
                  break;
                }
              }
              if (i >= 2) { console.log('  ...'); break; }
            }
          }
        } else {
          console.log('  PORT:', ps.slice(0, 200));
          console.log('  REF:', rs.slice(0, 200));
        }
      }
    }
  }
}
