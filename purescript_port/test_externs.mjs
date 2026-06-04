/**
 * Test the CBOR externs decoder on a few stdlib modules.
 */

import { init, loadExternsFile, loadAllExterns } from './decode_externs.mjs';

async function main() {
  console.log('Initializing modules...');
  await init();
  console.log('Done.');

  // Test single file
  console.log('\nTesting Data.Ordering externs...');
  try {
    const ef = await loadExternsFile('/tmp/ps_full_compare/stdlib_cache/Data.Ordering/externs.cbor');
    console.log('  Module:', ef.value0.efModuleName);
    console.log('  Declarations:', ef.value0.efDeclarations.length);
    console.log('  Exports:', ef.value0.efExports.length);
    console.log('  Imports:', ef.value0.efImports.length);
    // Check first declaration
    const decl0 = ef.value0.efDeclarations[0];
    console.log('  First decl type:', decl0.constructor.name);
    if (decl0.constructor.name === 'EDType') {
      console.log('  EDType name:', decl0.value0.edTypeName);
    }
    console.log('  PASS');
  } catch (e) {
    console.log('  FAIL:', e.message);
    console.log(e.stack);
  }

  // Test Data.Maybe
  console.log('\nTesting Data.Maybe externs...');
  try {
    const ef = await loadExternsFile('/tmp/ps_full_compare/stdlib_cache/Data.Maybe/externs.cbor');
    console.log('  Module:', ef.value0.efModuleName);
    console.log('  Declarations:', ef.value0.efDeclarations.length);
    console.log('  PASS');
  } catch (e) {
    console.log('  FAIL:', e.message);
    console.log(e.stack.split('\n').slice(0,5).join('\n'));
  }

  // Test Effect.Console
  console.log('\nTesting Effect.Console externs...');
  try {
    const ef = await loadExternsFile('/tmp/ps_full_compare/stdlib_cache/Effect.Console/externs.cbor');
    console.log('  Module:', ef.value0.efModuleName);
    console.log('  Declarations:', ef.value0.efDeclarations.length);
    console.log('  PASS');
  } catch (e) {
    console.log('  FAIL:', e.message);
    console.log(e.stack.split('\n').slice(0,5).join('\n'));
  }

  // Load all externs
  console.log('\nLoading all externs from stdlib cache...');
  const allExterns = await loadAllExterns('/tmp/ps_full_compare/stdlib_cache');
  console.log('Total loaded:', allExterns.length);
}

main().catch(e => {
  console.error('Fatal:', e);
  process.exit(1);
});
