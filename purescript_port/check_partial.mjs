import { init, loadExternsFile } from './decode_externs.mjs';
async function main() {
  await init();
  const ef = await loadExternsFile('/tmp/ps_full_compare/stdlib_cache/Partial/externs.cbor');
  console.log('Module:', ef.value0.efModuleName);
  for (const decl of ef.value0.efDeclarations) {
    if (decl.constructor.name === 'EDClass') {
      console.log('EDClass:', decl.value0.edClassName, 'isEmpty:', decl.value0.edIsEmpty);
    }
  }
}
main().catch(e => { console.error(e); process.exit(1); });
