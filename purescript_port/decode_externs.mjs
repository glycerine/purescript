/**
 * CBOR externs decoder for the PureScript port.
 *
 * Decodes externs.cbor files (produced by the Haskell purs compiler) into
 * ExternsFile JS objects that the PS port can use in rebuildModuleWithCoreFn.
 *
 * CBOR encoding conventions (Haskell codec-serialise DeriveAnyClass):
 *   - ADT: [constructorIndex, field1, field2, ...]
 *   - Haskell tuple (a,b): [a, b]  (no constructor index)
 *   - Maybe a: [] = Nothing, [a] = Just a
 *   - [T] list: plain CBOR array
 *   - Newtypes: [0, inner] in CBOR (but decoded to inner value in PS)
 *
 * Type constructor indices (from Haskell data Type a definition order):
 *   0=TUnknown, 1=TypeVar, 2=TypeLevelString, 3=TypeLevelInt, 4=TypeWildcard,
 *   5=TypeConstructor, 6=TypeOp, 7=TypeApp, 8=KindApp, 9=ForAll,
 *   10=ConstrainedType, 11=Skolem, 12=REmpty, 13=RCons, 14=KindedType,
 *   15=BinaryNoParensType, 16=ParensInType
 */

import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const cbor = require('cbor');

const OUTPUT = './output';

// Lazy module loading cache
const mods = {};
async function mod(name) {
  if (!mods[name]) {
    mods[name] = await import(`${OUTPUT}/${name}/index.js`);
  }
  return mods[name];
}

// Pre-load all needed modules
export async function loadModules() {
  const [Names, Types, Env, Externs, Decls, Tuple, Maybe, List, Roles, Operators, Either] = await Promise.all([
    mod('Language.PureScript.Names'),
    mod('Language.PureScript.Types'),
    mod('Language.PureScript.Environment'),
    mod('Language.PureScript.Externs'),
    mod('Language.PureScript.AST.Declarations'),
    mod('Data.Tuple'),
    mod('Data.Maybe'),
    mod('Data.List.Types'),
    mod('Language.PureScript.Roles'),
    mod('Language.PureScript.AST.Operators'),
    mod('Data.Either'),
  ]);
  return { Names, Types, Env, Externs, Decls, Tuple, Maybe, List, Roles, Operators, Either };
}

let M = null; // will be set by init()

export async function init() {
  M = await loadModules();
}

// ── Primitives ─────────────────────────────────────────────────────────────

function decodeSourcePos(arr) {
  // [0, line, col] in CBOR
  return { line: arr[1], column: arr[2] };
}

function decodeSourceSpan(arr) {
  // [0, name, startPos, endPos] in CBOR
  return { name: arr[1], start: decodeSourcePos(arr[2]), end: decodeSourcePos(arr[3]) };
}

// SourceType annotation: plain 2-tuple [sourceSpan_cbor, comments_cbor]
function decodeAnn(arr) {
  return new M.Tuple.Tuple(decodeSourceSpan(arr[0]), []);
}

function decodeNullAnn() {
  return new M.Tuple.Tuple({ name: '', start: { line: 0, column: 0 }, end: { line: 0, column: 0 } }, []);
}

// Maybe T: [] = Nothing, [t] = Just t
function decodeMaybe(arr, decodeT) {
  if (arr.length === 0) return M.Maybe.Nothing.value;
  return new M.Maybe.Just(decodeT(arr[0]));
}

// ── Names ──────────────────────────────────────────────────────────────────

// ProperName "x" = [0, "x"] in CBOR → "x" in PS (identity)
function decodeProperName(arr) {
  return arr[1]; // identity function
}

// ModuleName = plain string in CBOR → plain string in PS (identity)
function decodeModuleName(s) {
  return s;
}

// Ident "x" = [0, "x"] in CBOR → new Ident("x") in PS
function decodeIdent(arr) {
  return new M.Names.Ident(arr[1]);
}

// OpName = [0, "x"] in CBOR → "x" in PS (identity)
function decodeOpName(arr) {
  return arr[1];
}

// QualifiedBy: [0, pos] = BySourcePos, [1, mn] = ByModuleName
function decodeQualifiedBy(arr) {
  if (arr[0] === 0) return new M.Names.BySourcePos(decodeSourcePos(arr[1]));
  if (arr[0] === 1) return new M.Names.ByModuleName(decodeModuleName(arr[1]));
  throw new Error('Unknown QualifiedBy: ' + arr[0]);
}

// Qualified: [0, qb, name]
function decodeQualified(arr, decodeInner) {
  const qb = decodeQualifiedBy(arr[1]);
  const name = decodeInner(arr[2]);
  return new M.Names.Qualified(qb, name);
}

// ── Types ──────────────────────────────────────────────────────────────────

// TypeVarVisibility: [0] = TypeVarVisible, [1] = TypeVarInvisible
function decodeTypeVarVisibility(arr) {
  if (arr[0] === 0) return M.Types.TypeVarVisible.value;
  if (arr[0] === 1) return M.Types.TypeVarInvisible.value;
  throw new Error('Unknown TypeVarVisibility: ' + arr[0]);
}

// SkolemScope n = [0, n] in CBOR → n in PS (newtype = identity)
function decodeSkolemScope(arr) {
  return arr[1]; // SkolemScope is identity in PS
}

// Constraint: [0, { constraintAnn, constraintClass, constraintKindArgs, constraintArgs, constraintData }]
function decodeConstraint(arr) {
  // arr[0] = 0 (only one constructor)
  const ann = decodeAnn(arr[1]);
  const constraintClass = decodeQualified(arr[2], decodeProperName);
  const constraintKindArgs = arr[3].map(decodeType);
  const constraintArgs = arr[4].map(decodeType);
  const constraintData = M.Maybe.Nothing.value; // omit for now
  return new M.Types.Constraint({
    constraintAnn: ann,
    constraintClass,
    constraintKindArgs,
    constraintArgs,
    constraintData,
  });
}

// Type: [constructorIdx, ann, fields...]
function decodeType(arr) {
  const idx = arr[0];
  const ann = decodeAnn(arr[1]);
  switch (idx) {
    case 0: // TUnknown
      return new M.Types.TUnknown(ann, arr[2]);
    case 1: // TypeVar
      return new M.Types.TypeVar(ann, arr[2]);
    case 2: // TypeLevelString
      return new M.Types.TypeLevelString(ann, arr[2]);
    case 3: // TypeLevelInt
      return new M.Types.TypeLevelInt(ann, arr[2]);
    case 4: // TypeWildcard
      // TypeWildcard ann wildcardData - wildcardData can be opaque
      return new M.Types.TypeWildcard(ann, M.Maybe.Nothing.value);
    case 5: // TypeConstructor ann (Qualified ProperName)
      return new M.Types.TypeConstructor(ann, decodeQualified(arr[2], decodeProperName));
    case 6: // TypeOp ann (Qualified OpName)
      return new M.Types.TypeOp(ann, decodeQualified(arr[2], decodeOpName));
    case 7: // TypeApp ann t1 t2
      return new M.Types.TypeApp(ann, decodeType(arr[2]), decodeType(arr[3]));
    case 8: // KindApp ann t1 t2
      return new M.Types.KindApp(ann, decodeType(arr[2]), decodeType(arr[3]));
    case 9: { // ForAll ann visibility varName maybeKind body maybeSkolem
      const vis = decodeTypeVarVisibility(arr[2]);
      const varName = arr[3];
      const maybeKind = decodeMaybe(arr[4], decodeType);
      const body = decodeType(arr[5]);
      const maybeSkolem = decodeMaybe(arr[6], decodeSkolemScope);
      return new M.Types.ForAll(ann, vis, varName, maybeKind, body, maybeSkolem);
    }
    case 10: { // ConstrainedType ann constraint type
      return new M.Types.ConstrainedType(ann, decodeConstraint(arr[2]), decodeType(arr[3]));
    }
    case 11: { // Skolem ann text maybeKind int skolemScope
      const text = arr[2];
      const maybeKind = decodeMaybe(arr[3], decodeType);
      const skolemInt = arr[4];
      const skolemScope = arr[5][1]; // SkolemScope is identity
      return new M.Types.Skolem(ann, text, maybeKind, skolemInt, skolemScope);
    }
    case 12: // REmpty
      return new M.Types.REmpty(ann);
    case 13: { // RCons ann label type1 type2
      const label = arr[2]; // Label is a string
      return new M.Types.RCons(ann, label, decodeType(arr[3]), decodeType(arr[4]));
    }
    case 14: // KindedType ann t1 t2
      return new M.Types.KindedType(ann, decodeType(arr[2]), decodeType(arr[3]));
    case 15: // BinaryNoParensType ann t1 t2 t3
      return new M.Types.BinaryNoParensType(ann, decodeType(arr[2]), decodeType(arr[3]), decodeType(arr[4]));
    case 16: // ParensInType ann t
      return new M.Types.ParensInType(ann, decodeType(arr[2]));
    default:
      throw new Error('Unknown Type constructor: ' + idx + ' in ' + JSON.stringify(arr).slice(0, 100));
  }
}

// ── TypeKind ───────────────────────────────────────────────────────────────

// Role: [0]=Nominal [1]=Representational [2]=Phantom
function decodeRole(arr) {
  if (arr[0] === 0) return M.Roles.Nominal.value;
  if (arr[0] === 1) return M.Roles.Representational.value;
  if (arr[0] === 2) return M.Roles.Phantom.value;
  throw new Error('Unknown Role: ' + arr[0]);
}

// DataDeclType: [0]=Data [1]=Newtype
function decodeDataDeclType(arr) {
  if (arr[0] === 0) return M.Env.Data.value;
  if (arr[0] === 1) return M.Env.Newtype.value;
  throw new Error('Unknown DataDeclType: ' + arr[0]);
}

// TypeKind:
//   0 = DataType DataDeclType [(Text, Maybe SourceType, Role)] [(ProperName, [SourceType])]
//   1 = TypeSynonym
//   2 = ExternData [Role]
//   3 = LocalTypeVariable
//   4 = ScopedTypeVar
function decodeTypeKind(arr) {
  switch (arr[0]) {
    case 0: { // DataType
      const declType = decodeDataDeclType(arr[1]);
      // params: [(Text, Maybe SourceType, Role)] encoded as [[name, maybeKind, role], ...]
      const params = arr[2].map(p => new M.Tuple.Tuple(p[0], new M.Tuple.Tuple(decodeMaybe(p[1], decodeType), decodeRole(p[2]))));
      // ctors: [(ProperName, [SourceType])]
      const ctors = arr[3].map(c => new M.Tuple.Tuple(decodeProperName(c[0]), c[1].map(decodeType)));
      return new M.Env.DataType(declType, params, ctors);
    }
    case 1: return M.Env.TypeSynonym.value;
    case 2: { // ExternData [Role]
      const roles = arr[1].map(decodeRole);
      return new M.Env.ExternData(roles);
    }
    case 3: return M.Env.LocalTypeVariable.value;
    case 4: return M.Env.ScopedTypeVar.value;
    default:
      throw new Error('Unknown TypeKind: ' + arr[0]);
  }
}

// ── DeclarationRef ─────────────────────────────────────────────────────────

// DeclarationRef constructor indices (Haskell definition order):
//   0=TypeClassRef 1=TypeOpRef 2=TypeRef 3=ValueRef 4=ValueOpRef
//   5=TypeInstanceRef 6=ModuleRef 7=ReExportRef
function decodeDeclarationRef(arr) {
  const ss = decodeSourceSpan(arr[1]);
  switch (arr[0]) {
    case 0: // TypeClassRef ss name
      return new M.Decls.TypeClassRef(ss, decodeProperName(arr[2]));
    case 1: // TypeOpRef ss op
      return new M.Decls.TypeOpRef(ss, decodeOpName(arr[2]));
    case 2: { // TypeRef ss name maybeDctors
      const maybeDctors = decodeMaybe(arr[3], a => a.map(decodeProperName));
      return new M.Decls.TypeRef(ss, decodeProperName(arr[2]), maybeDctors);
    }
    case 3: // ValueRef ss ident
      return new M.Decls.ValueRef(ss, decodeIdent(arr[2]));
    case 4: // ValueOpRef ss op
      return new M.Decls.ValueOpRef(ss, decodeOpName(arr[2]));
    case 5: // TypeInstanceRef ss ident nameSource (ignored for externs purposes)
      return new M.Decls.TypeClassRef(ss, decodeProperName(arr[2])); // simplified
    case 6: // ModuleRef ss moduleName
      return new M.Decls.ModuleRef(ss, decodeModuleName(arr[2]));
    case 7: { // ReExportRef ss exportSource ref
      // ExportSource: [0, importedFrom, definedIn]
      // importedFrom = Maybe ModuleName = [] or [mn]
      // definedIn = ModuleName (plain string)
      const esSrc = arr[2];
      const exportSource = new M.Decls.ExportSource({
        exportSourceImportedFrom: decodeMaybe(esSrc[1], decodeModuleName),
        exportSourceDefinedIn: decodeModuleName(esSrc[2]),
      });
      return new M.Decls.ReExportRef(ss, exportSource, decodeDeclarationRef(arr[3]));
    }
    default:
      throw new Error('Unknown DeclarationRef: ' + arr[0]);
  }
}

// ── ExternsImport ──────────────────────────────────────────────────────────

// ImportDeclarationType: [0]=Implicit [1,[refs]]=Explicit [2,[refs]]=Hiding
function decodeImportDeclType(arr) {
  const Imps = mods['Language.PureScript.AST.Declarations'];
  if (arr[0] === 0) return M.Decls.Implicit.value;
  if (arr[0] === 1) return new M.Decls.Explicit(arr[1].map(decodeDeclarationRef));
  if (arr[0] === 2) return new M.Decls.Hiding(arr[1].map(decodeDeclarationRef));
  throw new Error('Unknown ImportDeclType: ' + arr[0]);
}

function decodeExternsImport(arr) {
  // [0, eiModule, eiImportType, eiImportedAs]
  return new M.Externs.ExternsImport({
    eiModule: decodeModuleName(arr[1]),
    eiImportType: decodeImportDeclType(arr[2]),
    eiImportedAs: decodeMaybe(arr[3], decodeModuleName),
  });
}

// ── ExternsFixity ──────────────────────────────────────────────────────────

// Associativity: [0]=Infixl [1]=Infixr [2]=Infix
function decodeAssociativity(arr) {
  if (arr[0] === 0) return M.Operators.Infixl.value;
  if (arr[0] === 1) return M.Operators.Infixr.value;
  if (arr[0] === 2) return M.Operators.Infix.value;
  throw new Error('Unknown Associativity: ' + arr[0]);
}

// efAlias: Qualified (Either Ident (ProperName ConstructorName))
// Either: [0, left] = Left, [1, right] = Right
function decodeFixityAlias(arr) {
  const qb = decodeQualifiedBy(arr[1]);
  const inner = arr[2];
  const aliasValue = inner[0] === 0
    ? new M.Either.Left(decodeIdent(inner[1]))
    : new M.Either.Right(decodeProperName(inner[1]));
  return new M.Names.Qualified(qb, aliasValue);
}

function decodeExternsFixity(arr) {
  return new M.Externs.ExternsFixity({
    efAssociativity: decodeAssociativity(arr[1]),
    efPrecedence: arr[2],
    efOperator: decodeOpName(arr[3]),
    efAlias: decodeFixityAlias(arr[4]),
  });
}

function decodeExternsTypeFixity(arr) {
  return new M.Externs.ExternsTypeFixity({
    efTypeAssociativity: decodeAssociativity(arr[1]),
    efTypePrecedence: arr[2],
    efTypeOperator: decodeOpName(arr[3]),
    efTypeAlias: decodeQualified(arr[4], decodeProperName),
  });
}

// ── ExternsDeclaration ─────────────────────────────────────────────────────

// NameSource: [0]=UserNamed [1]=CompilerNamed
function decodeNameSource(arr) {
  if (arr[0] === 0) return M.Decls.UserNamed.value;
  if (arr[0] === 1) return M.Decls.CompilerNamed.value;
  throw new Error('Unknown NameSource: ' + arr[0]);
}

// FunctionalDependency: [determiners, determined] (both arrays of Text)
function decodeFunctionalDependency(arr) {
  // Just pass through as a record
  return { fdDetermined: arr[1], fdDeterminers: arr[0] };
}

function decodeExternsDeclaration(arr) {
  switch (arr[0]) {
    case 0: { // EDType name kind typeKind
      return new M.Externs.EDType({
        edTypeName: decodeProperName(arr[1]),
        edTypeKind: decodeType(arr[2]),
        edTypeDeclarationKind: decodeTypeKind(arr[3]),
      });
    }
    case 1: { // EDTypeSynonym name args type
      const args = arr[2].map(p => new M.Tuple.Tuple(p[0], decodeMaybe(p[1], decodeType)));
      return new M.Externs.EDTypeSynonym({
        edTypeSynonymName: decodeProperName(arr[1]),
        edTypeSynonymArguments: args,
        edTypeSynonymType: decodeType(arr[3]),
      });
    }
    case 2: { // EDDataConstructor name origin typeCtor type fields
      return new M.Externs.EDDataConstructor({
        edDataCtorName: decodeProperName(arr[1]),
        edDataCtorOrigin: decodeDataDeclType(arr[2]),
        edDataCtorTypeCtor: decodeProperName(arr[3]),
        edDataCtorType: decodeType(arr[4]),
        edDataCtorFields: arr[5].map(decodeIdent),
      });
    }
    case 3: { // EDValue name type
      return new M.Externs.EDValue({
        edValueName: decodeIdent(arr[1]),
        edValueType: decodeType(arr[2]),
      });
    }
    case 4: { // EDClass name args members constraints deps isEmpty
      const args = arr[2].map(p => new M.Tuple.Tuple(p[0], decodeMaybe(p[1], decodeType)));
      const members = arr[3].map(m => new M.Tuple.Tuple(decodeIdent(m[0]), decodeType(m[1])));
      const constraints = arr[4].map(decodeConstraint);
      const deps = arr[5].map(decodeFunctionalDependency);
      const isEmpty = arr[6] === true || arr[6] === 1;
      return new M.Externs.EDClass({
        edClassName: decodeProperName(arr[1]),
        edClassTypeArguments: args,
        edClassMembers: members,
        edClassConstraints: constraints,
        edFunctionalDependencies: deps,
        edIsEmpty: isEmpty,
      });
    }
    case 5: { // EDInstance className name forAll kinds types constraints chain chainIdx nameSource ss
      const className = decodeQualified(arr[1], decodeProperName);
      const instanceName = decodeIdent(arr[2]);
      const forAll = arr[3].map(p => new M.Tuple.Tuple(p[0], decodeType(p[1])));
      const kinds = arr[4].map(decodeType);
      const types = arr[5].map(decodeType);
      const constraints = decodeMaybe(arr[6], a => a.map(decodeConstraint));
      // chain: Maybe ChainId where ChainId = Tuple String SourcePos
      const chain = decodeMaybe(arr[7], c => new M.Tuple.Tuple(c[0], decodeSourcePos(c[1])));
      const chainIndex = arr[8];
      const nameSource = decodeNameSource(arr[9]);
      const sourceSpan = decodeSourceSpan(arr[10]);
      return new M.Externs.EDInstance({
        edInstanceClassName: className,
        edInstanceName: instanceName,
        edInstanceForAll: forAll,
        edInstanceKinds: kinds,
        edInstanceTypes: types,
        edInstanceConstraints: constraints,
        edInstanceChain: chain,
        edInstanceChainIndex: chainIndex,
        edInstanceNameSource: nameSource,
        edInstanceSourceSpan: sourceSpan,
      });
    }
    default:
      throw new Error('Unknown ExternsDeclaration: ' + arr[0]);
  }
}

// ── ExternsFile ────────────────────────────────────────────────────────────

export function decodeExternsFile(buf) {
  const raw = cbor.decodeFirstSync(buf);
  // raw = [0, version, modName, exports, imports, fixities, typeFixities, declarations, sourceSpan]
  const version = raw[1];
  const modName = decodeModuleName(raw[2]);
  const exports = raw[3].map(decodeDeclarationRef);
  const imports = raw[4].map(decodeExternsImport);
  const fixities = raw[5].map(decodeExternsFixity);
  const typeFixities = raw[6].map(decodeExternsTypeFixity);
  const declarations = raw[7].map(decodeExternsDeclaration);
  const sourceSpan = decodeSourceSpan(raw[8]);

  return new M.Externs.ExternsFile({
    efVersion: version,
    efModuleName: modName,
    efExports: exports,
    efImports: imports,
    efFixities: fixities,
    efTypeFixities: typeFixities,
    efDeclarations: declarations,
    efSourceSpan: sourceSpan,
  });
}

export async function loadExternsFile(path) {
  const fs = await import('fs');
  const buf = fs.readFileSync(path);
  return decodeExternsFile(buf);
}

// Load all externs from a stdlib cache directory, sorted in topological order
export async function loadAllExterns(cacheDir) {
  const fs = await import('fs');
  const path = await import('path');
  const dirs = fs.readdirSync(cacheDir);
  const externs = [];
  let loaded = 0;
  let failed = 0;
  for (const dir of dirs) {
    if (dir === 'cache-db.json') continue;
    const externPath = path.join(cacheDir, dir, 'externs.cbor');
    if (!fs.existsSync(externPath)) continue;
    try {
      const ef = await loadExternsFile(externPath);
      externs.push(ef);
      loaded++;
    } catch (e) {
      failed++;
      // console.error(`  FAILED ${dir}: ${e.message}`);
    }
  }
  console.log(`Loaded ${loaded} externs, ${failed} failed`);
  // Sort in topological order (dependencies first)
  return topoSort(externs);
}

// Topological sort of externs based on module imports
function topoSort(externs) {
  const modMap = new Map();
  for (const ef of externs) {
    modMap.set(ef.value0.efModuleName, ef);
  }

  const visited = new Set();
  const result = [];

  function visit(modName) {
    if (visited.has(modName)) return;
    visited.add(modName);
    const ef = modMap.get(modName);
    if (!ef) return; // not in our set
    // Visit all imported modules first
    for (const imp of ef.value0.efImports) {
      visit(imp.value0.eiModule);
    }
    result.push(ef);
  }

  for (const ef of externs) {
    visit(ef.value0.efModuleName);
  }

  return result;
}
