import { init, loadExternsFile } from './decode_externs.mjs';
async function main() {
  await init();
  const ef = await loadExternsFile('/tmp/ps_full_compare/stdlib_cache/Partial/externs.cbor');
  console.log('Module:', ef.value0.efModuleName);
  console.log('Declarations:', ef.value0.efDeclarations.length);
  for (const decl of ef.value0.efDeclarations) {
    console.log('  decl type:', decl.constructor.name, JSON.stringify(decl.value0).slice(0, 100));
  }
}
main().catch(e => { console.error(e); process.exit(1); });
