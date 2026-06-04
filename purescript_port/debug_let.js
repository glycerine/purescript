const { lex } = require('./output/Language.PureScript.CST.Lexer/index.js');
const { parseModule } = require('./output/Language.PureScript.CST.Parser/index.js');
const { convertModule } = require('./output/Language.PureScript.CST.Convert/index.js');

const src = `module LetTest where

let_ :: forall a b. a -> b -> a
let_ x y = let z = x in z`;

const tokens = lex(src);
const parsed = parseModule(tokens);

// Check the right branch - it should be Right (PartialResult _)
// In PS compiled to JS, Right is typically {value0: ...} with constructor check
function findDecls(obj, path, depth) {
  if (depth > 15 || !obj || typeof obj !== 'object') return;
  
  // Look for Let expressions
  if (obj.tag === 'Let' || (obj.constructor && obj.constructor.name && obj.constructor.name.includes('Let'))) {
    console.log("Found Let at path:", path, JSON.stringify(obj).substring(0, 200));
  }
  
  // Look for ValueDeclaration or BindingGroupDeclaration
  if (obj.tag === 'ValueDeclaration' || (obj.constructor && obj.constructor.name === 'ValueDeclaration')) {
    console.log("Found ValueDeclaration at path:", path);
  }
  
  for (const k of Object.keys(obj)) {
    findDecls(obj[k], path + '.' + k, depth + 1);
  }
}

// First get the AST module
let astMod;
try {
  // It's Either Left errs | Right (PartialResult r)
  // In purescript-compiled JS, Either is typically value0 = Left, value0 = Right
  const result = parsed;
  if (result && result.value0) {
    const pr = result.value0; // PartialResult
    if (pr.resFull && pr.resFull.value1) {
      // value1 is the Right side
      const cst = pr.resFull.value1.value0; // Right cstMod -> value0
      const modName = "LetTest";
      astMod = convertModule(modName + ".purs")(cst);
      console.log("Got AST module");
      console.log("Module type:", typeof astMod, Object.keys(astMod).slice(0, 5));
      
      // Find decls in AST
      findDecls(astMod, "astMod", 0);
    }
  }
} catch(e) {
  console.error("Error:", e.message);
  console.error("Stack:", e.stack);
}
