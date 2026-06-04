module Language.PureScript.Externs
  ( ExternsFile(..)
  , ExternsImport(..)
  , ExternsFixity(..)
  , ExternsTypeFixity(..)
  , ExternsDeclaration(..)
  , applyExternsFileToEnvironment
  , moduleToExternsFile
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)

import Language.PureScript.AST.Declarations
  ( DeclarationRef(..)
  , ImportDeclarationType
  , NameSource(..)
  , Declaration(..)
  , ValueFixity(..)
  , TypeFixity(..)
  , getValueOpRef
  , getTypeOpRef
  )
import Language.PureScript.AST.Operators (Associativity, Fixity(..), Precedence)
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.AST.Declarations (Module(..))
import Language.PureScript.Environment
  ( DataDeclType
  , Environment(..)
  , FunctionalDependency
  , NameKind(..)
  , NameVisibility(..)
  , TypeClassData(..)
  , TypeKind(..)
  , makeTypeClassData
  , dictTypeName
  )
import Language.PureScript.Names
  ( ClassName
  , ConstructorName
  , Ident
  , ModuleName
  , OpName
  , ProperName(..)
  , Qualified(..)
  , QualifiedBy(..)
  , TypeName
  , TypeOpName
  , ValueOpName
  , coerceProperName
  , isPlainIdent
  )
import Language.PureScript.TypeClassDictionaries (TypeClassDictionaryInScope(..))
import Language.PureScript.Types (SourceConstraint, SourceType, srcInstanceType)
import Language.PureScript.Roles (Role)
import Partial.Unsafe (unsafeCrashWith)

data ExternsFile = ExternsFile
  { efVersion      :: String
  , efModuleName   :: ModuleName
  , efExports      :: Array DeclarationRef
  , efImports      :: Array ExternsImport
  , efFixities     :: Array ExternsFixity
  , efTypeFixities :: Array ExternsTypeFixity
  , efDeclarations :: Array ExternsDeclaration
  , efSourceSpan   :: SourceSpan
  }

data ExternsImport = ExternsImport
  { eiModule     :: ModuleName
  , eiImportType :: ImportDeclarationType
  , eiImportedAs :: Maybe ModuleName
  }

data ExternsFixity = ExternsFixity
  { efAssociativity :: Associativity
  , efPrecedence    :: Precedence
  , efOperator      :: OpName ValueOpName
  , efAlias         :: Qualified (Either Ident (ProperName ConstructorName))
  }

data ExternsTypeFixity = ExternsTypeFixity
  { efTypeAssociativity :: Associativity
  , efTypePrecedence    :: Precedence
  , efTypeOperator      :: OpName TypeOpName
  , efTypeAlias         :: Qualified (ProperName TypeName)
  }

data ExternsDeclaration
  = EDType
      { edTypeName             :: ProperName TypeName
      , edTypeKind             :: SourceType
      , edTypeDeclarationKind  :: TypeKind
      }
  | EDTypeSynonym
      { edTypeSynonymName      :: ProperName TypeName
      , edTypeSynonymArguments :: Array (Tuple String (Maybe SourceType))
      , edTypeSynonymType      :: SourceType
      }
  | EDDataConstructor
      { edDataCtorName    :: ProperName ConstructorName
      , edDataCtorOrigin  :: DataDeclType
      , edDataCtorTypeCtor :: ProperName TypeName
      , edDataCtorType    :: SourceType
      , edDataCtorFields  :: Array Ident
      }
  | EDValue
      { edValueName :: Ident
      , edValueType :: SourceType
      }
  | EDClass
      { edClassName            :: ProperName ClassName
      , edClassTypeArguments   :: Array (Tuple String (Maybe SourceType))
      , edClassMembers         :: Array (Tuple Ident SourceType)
      , edClassConstraints     :: Array SourceConstraint
      , edFunctionalDependencies :: Array FunctionalDependency
      , edIsEmpty              :: Boolean
      }
  | EDInstance
      { edInstanceClassName   :: Qualified (ProperName ClassName)
      , edInstanceName        :: Ident
      , edInstanceForAll      :: Array (Tuple String SourceType)
      , edInstanceKinds       :: Array SourceType
      , edInstanceTypes       :: Array SourceType
      , edInstanceConstraints :: Maybe (Array SourceConstraint)
      , edInstanceSourceSpan  :: SourceSpan
      }

-- | Apply an externs file to an environment, extending it with its declarations.
applyExternsFileToEnvironment :: ExternsFile -> Environment -> Environment
applyExternsFileToEnvironment (ExternsFile ef) env =
  Array.foldl applyDecl env ef.efDeclarations
  where
  qual :: forall a. a -> Qualified a
  qual = Qualified (ByModuleName ef.efModuleName)

  applyDecl :: Environment -> ExternsDeclaration -> Environment
  applyDecl (Environment e) (EDType { edTypeName: pn, edTypeKind: kind, edTypeDeclarationKind: tyKind }) =
    Environment e { types = Map.insert (qual pn) (Tuple kind tyKind) e.types }
  applyDecl (Environment e) (EDTypeSynonym { edTypeSynonymName: pn, edTypeSynonymArguments: args, edTypeSynonymType: ty }) =
    Environment e { typeSynonyms = Map.insert (qual pn) (Tuple args ty) e.typeSynonyms }
  applyDecl (Environment e) (EDDataConstructor { edDataCtorName: pn, edDataCtorOrigin: dTy, edDataCtorTypeCtor: tNm, edDataCtorType: ty, edDataCtorFields: nms }) =
    Environment e { dataConstructors = Map.insert (qual pn) (Tuple (Tuple (Tuple dTy tNm) ty) nms) e.dataConstructors }
  applyDecl (Environment e) (EDValue { edValueName: ident, edValueType: ty }) =
    Environment e { names = Map.insert (Qualified (ByModuleName ef.efModuleName) ident) (Tuple (Tuple ty External) Defined) e.names }
  applyDecl (Environment e) (EDClass { edClassName: pn, edClassTypeArguments: args, edClassMembers: members, edClassConstraints: cs, edFunctionalDependencies: deps, edIsEmpty: tcIsEmpty }) =
    Environment e { typeClasses = Map.insert (qual pn) (makeTypeClassData args members cs deps tcIsEmpty) e.typeClasses }
  applyDecl (Environment e) (EDInstance { edInstanceClassName: className, edInstanceName: ident, edInstanceForAll: vars, edInstanceKinds: kinds, edInstanceTypes: tys, edInstanceConstraints: cs, edInstanceSourceSpan: ss }) =
    Environment e
      { typeClassDictionaries =
          updateMap
            (updateMap (Map.insertWith (<>) (qual ident) [dict]) className)
            (ByModuleName ef.efModuleName)
            e.typeClassDictionaries
      }
    where
    dict :: TypeClassDictionaryInScope (Qualified Ident)
    dict = TypeClassDictionaryInScope
      { tcdChain: Nothing
      , tcdIndex: 0
      , tcdValue: qual ident
      , tcdPath: []
      , tcdClassName: className
      , tcdForAll: vars
      , tcdInstanceKinds: kinds
      , tcdInstanceTypes: tys
      , tcdDependencies: cs
      , tcdDescription: Just (srcInstanceType ss vars className tys)
      }

    updateMap :: forall k a. Ord k => Monoid a => (a -> a) -> k -> Map k a -> Map k a
    updateMap f = Map.alter (Just <<< f <<< fromMaybe mempty)

-- | Generate an externs file for all declarations in a module.
moduleToExternsFile
  :: Module
  -> Environment
  -> Map Ident Ident
  -> ExternsFile
moduleToExternsFile (Module _ _ _ _ Nothing) _ _ =
  -- This case shouldn't happen post-elaboration; return a minimal dummy
  -- (mirrors internalError in Haskell)
  unsafeCrashWith "moduleToExternsFile: module exports were not elaborated"
moduleToExternsFile (Module ss _ mn ds (Just exps)) (Environment env) renamedIdents =
  ExternsFile
    { efVersion:      "0.15.16"
    , efModuleName:   mn
    , efExports:      map renameRef exps
    , efImports:      Array.mapMaybe importDecl ds
    , efFixities:     Array.mapMaybe fixityDecl ds
    , efTypeFixities: Array.mapMaybe typeFixityDecl ds
    , efDeclarations: Array.concatMap toExternsDeclaration exps
    , efSourceSpan:   ss
    }
  where
  qual :: forall a. a -> Qualified a
  qual = Qualified (ByModuleName mn)

  lookupRenamedIdent :: Ident -> Ident
  lookupRenamedIdent ident = fromMaybe ident (Map.lookup ident renamedIdents)

  fixityDecl :: Declaration -> Maybe ExternsFixity
  fixityDecl (FixityDeclaration _ (Left (ValueFixity (Fixity assoc prec) name op))) =
    const (ExternsFixity { efAssociativity: assoc, efPrecedence: prec, efOperator: op, efAlias: name })
      <$> Array.find (\ref -> getValueOpRef ref == Just op) exps
  fixityDecl _ = Nothing

  typeFixityDecl :: Declaration -> Maybe ExternsTypeFixity
  typeFixityDecl (FixityDeclaration _ (Right (TypeFixity (Fixity assoc prec) name op))) =
    const (ExternsTypeFixity { efTypeAssociativity: assoc, efTypePrecedence: prec, efTypeOperator: op, efTypeAlias: name })
      <$> Array.find (\ref -> getTypeOpRef ref == Just op) exps
  typeFixityDecl _ = Nothing

  importDecl :: Declaration -> Maybe ExternsImport
  importDecl (ImportDeclaration _ m mt qmn) = Just (ExternsImport { eiModule: m, eiImportType: mt, eiImportedAs: qmn })
  importDecl _ = Nothing

  toExternsDeclaration :: DeclarationRef -> Array ExternsDeclaration
  toExternsDeclaration (TypeRef _ pn dctors) =
    case Map.lookup (qual pn) env.types of
      Nothing ->
        unsafeCrashWith "toExternsDeclaration: no kind in toExternsDeclaration"
      Just (Tuple kind TypeSynonym) ->
        case Map.lookup (qual pn) env.typeSynonyms of
          Just (Tuple args synTy) ->
            [ EDType { edTypeName: pn, edTypeKind: kind, edTypeDeclarationKind: TypeSynonym }
            , EDTypeSynonym { edTypeSynonymName: pn, edTypeSynonymArguments: args, edTypeSynonymType: synTy }
            ]
          Nothing ->
            unsafeCrashWith "toExternsDeclaration: TypeSynonym without synonym entry"
      Just (Tuple kind (ExternData rs)) ->
        [ EDType { edTypeName: pn, edTypeKind: kind, edTypeDeclarationKind: ExternData rs } ]
      Just (Tuple kind tk@(DataType _ _ tys)) ->
        let ctorNames = fromMaybe (map fst tys) dctors
            ctorDecls = Array.concatMap (\dctor ->
              case Map.lookup (qual dctor) env.dataConstructors of
                Just (Tuple (Tuple (Tuple dty _) ty) args) ->
                  [ EDDataConstructor
                      { edDataCtorName: dctor
                      , edDataCtorOrigin: dty
                      , edDataCtorTypeCtor: pn
                      , edDataCtorType: ty
                      , edDataCtorFields: args
                      }
                  ]
                Nothing -> []
              ) ctorNames
        in [ EDType { edTypeName: pn, edTypeKind: kind, edTypeDeclarationKind: tk } ] <> ctorDecls
      _ ->
        unsafeCrashWith "toExternsDeclaration: Invalid input"

  toExternsDeclaration (ValueRef _ ident) =
    case Map.lookup (qual ident) env.names of
      Just (Tuple (Tuple ty _) _) ->
        [ EDValue { edValueName: lookupRenamedIdent ident, edValueType: ty } ]
      Nothing -> []

  toExternsDeclaration (TypeClassRef _ className) =
    let dictName = dictTypeName (coerceProperName className :: ProperName TypeName)
    in case Map.lookup (qual className) env.typeClasses of
      Just (TypeClassData tcd) ->
        case Map.lookup (qual (coerceProperName className :: ProperName TypeName)) env.types of
          Just (Tuple kind tk) ->
            case Map.lookup (qual dictName) env.types of
              Just (Tuple dictKind dictData@(DataType _ _ [(Tuple dctor _)])) ->
                case Map.lookup (qual dctor) env.dataConstructors of
                  Just (Tuple (Tuple (Tuple dty _) ty) args) ->
                    let members = map (\(Tuple (Tuple i t) _) -> Tuple i t) tcd.typeClassMembers
                    in [ EDType { edTypeName: coerceProperName className, edTypeKind: kind, edTypeDeclarationKind: tk }
                       , EDType { edTypeName: dictName, edTypeKind: dictKind, edTypeDeclarationKind: dictData }
                       , EDDataConstructor { edDataCtorName: dctor, edDataCtorOrigin: dty, edDataCtorTypeCtor: dictName, edDataCtorType: ty, edDataCtorFields: args }
                       , EDClass
                           { edClassName: className
                           , edClassTypeArguments: tcd.typeClassArguments
                           , edClassMembers: members
                           , edClassConstraints: tcd.typeClassSuperclasses
                           , edFunctionalDependencies: tcd.typeClassDependencies
                           , edIsEmpty: tcd.typeClassIsEmpty
                           }
                       ]
                  Nothing -> []
              _ -> []
          Nothing -> []
      Nothing -> []

  toExternsDeclaration (TypeInstanceRef ss' ident ns) =
    case Map.lookup (ByModuleName mn) env.typeClassDictionaries of
      Nothing -> []
      Just m1 ->
        let allDicts = Array.concatMap (\m2 ->
              case Map.lookup (qual ident) m2 of
                Nothing -> []
                Just dictArr -> dictArr
              ) (Array.fromFoldable (Map.values m1))
        in map (\(TypeClassDictionaryInScope tcd) ->
              EDInstance
                { edInstanceClassName: tcd.tcdClassName
                , edInstanceName: lookupRenamedIdent ident
                , edInstanceForAll: tcd.tcdForAll
                , edInstanceKinds: tcd.tcdInstanceKinds
                , edInstanceTypes: tcd.tcdInstanceTypes
                , edInstanceConstraints: tcd.tcdDependencies
                , edInstanceSourceSpan: ss'
                }
            ) allDicts

  toExternsDeclaration _ = []

  renameRef :: DeclarationRef -> DeclarationRef
  renameRef (ValueRef ss' ident) = ValueRef ss' (lookupRenamedIdent ident)
  renameRef (TypeInstanceRef ss' ident _)
    | not (isPlainIdent ident) = TypeInstanceRef ss' (lookupRenamedIdent ident) CompilerNamed
  renameRef other = other
