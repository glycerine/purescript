// Trace what renameDecl produces for the LetTest module
const {lex}=require('./output/Language.PureScript.CST.Lexer/index.js');
const {parseModule}=require('./output/Language.PureScript.CST.Parser/index.js');
const {convertModule}=require('./output/Language.PureScript.CST.Convert/index.js');
const Sugar=require('./output/Language.PureScript.Sugar/index.js');
const {runExcept}=require('./output/Control.Monad.Except/index.js');
const {runWriterT}=require('./output/Control.Monad.Writer/index.js');
const {rebuildModuleWithCoreFn}=require('./output/Language.PureScript.Make/index.js');
const {moduleToJSON}=require('./output/Language.PureScript.CoreFn.ToJSON/index.js');
const Json=require('./output/Data.Argonaut.Core/index.js');

const src = `module LetTest where

let_ :: forall a b. a -> b -> a
let_ x y = let z = x in z`;

const tokens=lex(src);
const r=parseModule(tokens);
const pr=r.value0.value0;
const cst=pr.resFull.value1.value0;
const ast=convertModule('LetTest.purs')(cst);

// Run the full compilation
const result = runExcept(runWriterT(rebuildModuleWithCoreFn([])(ast)));
if (result instanceof require('./output/Data.Either/index.js').Left) {
  console.log("ERROR:", JSON.stringify(result.value0).substring(0, 500));
} else {
  const corefn = result.value0.value0.value1;
  const json = moduleToJSON('0.15.16')(corefn);
  const jsonStr = Json.stringify(json);
  const parsed = JSON.parse(jsonStr);
  
  // Look for sourcePos in decls
  function findSourcePos(obj, path, depth) {
    if (depth > 20 || !obj || typeof obj !== 'object') return;
    if (Array.isArray(obj) && obj.length === 2 && typeof obj[0] === 'number' && typeof obj[1] === 'number') {
      // sourcePos
      if (path.includes('sourcePos') || path.includes('expression') || path.includes('value')) {
        console.log(`  sourcePos at ${path}: [${obj[0]}, ${obj[1]}]`);
      }
    }
    if (obj.sourcePos !== undefined) {
      console.log(`  ${path}.sourcePos = [${obj.sourcePos}]`);
    }
    for (const k of Object.keys(obj)) {
      findSourcePos(obj[k], path + '.' + k, depth + 1);
    }
  }
  
  findSourcePos(parsed, 'root', 0);
}
