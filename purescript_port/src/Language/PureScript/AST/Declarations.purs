module Language.PureScript.AST.Declarations
  ( Context
  , NameSource(..)
  , ExportSource(..)
  , DeclarationRef(..)
  , ImportDeclarationType(..)
  , RoleDeclarationData(..)
  , TypeDeclarationData(..)
  , ValueDeclarationData(..)
  , DataConstructorDeclaration(..)
  , Declaration(..)
  , ValueFixity(..)
  , TypeFixity(..)
  , InstanceDerivationStrategy(..)
  , TypeInstanceBody(..)
  , KindSignatureFor(..)
  , Guard(..)
  , GuardedExpr(..)
  , Expr(..)
  , WhereProvenance(..)
  , CaseAlternative(..)
  , DoNotationElement(..)
  , PathTree(..)
  , PathNode(..)
  , AssocList(..)
  , ErrorMessageHint(..)
  , HintCategory(..)
  , UnknownsHint(..)
  , TypeSearch(..)
  , Module(..)
  , getModuleName
  , getModuleSourceSpan
  , getModuleDeclarations
  , addDefaultImport
  , importPrim
  , getTypeDeclaration
  , unwrapTypeDeclaration
  , getValueDeclaration
  , mapTypeInstanceBody
  , traverseTypeInstanceBody
  , declSourceAnn
  , declSourceSpan
  , declName
  , declRefName
  , declRefSourceSpan
  , getTypeRef
  , getValueRef
  , getTypeClassRef
  , isModuleRef
  , isExplicit
  , isValueDecl
  , isDataDecl
  , isTypeSynonymDecl
  , isImportDecl
  , isRoleDecl
  , isExternDataDecl
  , isFixityDecl
  , isExternDecl
  , isTypeClassInstanceDecl
  , isTypeClassDecl
  , isKindDecl
  , flattenDecls
  , isTrueExpr
  , isAnonymousArgument
  , mapDataCtorFields
  , traverseDataCtorFields
  , getFixityDecl
  , getValueOpRef
  , getTypeOpRef
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable, any)
import Data.List.NonEmpty (NonEmptyList)
import Data.Traversable (class Traversable)
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Language.PureScript.AST.Binders (Binder)
import Language.PureScript.AST.Declarations.ChainId (ChainId)
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.Operators (Fixity)
import Language.PureScript.AST.SourcePos (SourceAnn, SourcePos(..), SourceSpan)
import Data.Tuple as Tuple
import Language.PureScript.Comments (Comment)
import Language.PureScript.Environment (DataDeclType, Environment, FunctionalDependency, NameKind)
import Language.PureScript.Label (Label)
import Language.PureScript.Names (ClassName, ConstructorName, Ident(..), ModuleName(..), Name(..), OpName, ProperName, Qualified(..), QualifiedBy(..), TypeName, TypeOpName, ValueOpName, toMaybeModuleName)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Roles (Role)
import Language.PureScript.TypeClassDictionaries (NamedDict)
import Language.PureScript.Types (SourceConstraint, SourceType)

type Context = Array (Tuple Ident SourceType)

-- | Type-search state, used during type-checking for hole completion.
data TypeSearch
  = TSBefore Environment
  | TSAfter
      { tsAfterIdentifiers   :: Array (Tuple (Qualified String) SourceType)
      , tsAfterRecordFields  :: Maybe (Array (Tuple Label SourceType))
      }

instance showTypeSearch :: Show TypeSearch where
  show (TSBefore _) = "TSBefore"
  show (TSAfter _)  = "TSAfter"

-- | Error message hints.
data ErrorMessageHint
  = ErrorUnifyingTypes SourceType SourceType
  | ErrorInExpression Expr
  | ErrorInModule ModuleName
  | ErrorInInstance (Qualified (ProperName ClassName)) (Array SourceType)
  | ErrorInSubsumption SourceType SourceType
  | ErrorInRowLabel Label
  | ErrorCheckingAccessor Expr PSString
  | ErrorCheckingType Expr SourceType
  | ErrorCheckingKind SourceType SourceType
  | ErrorCheckingGuard
  | ErrorInferringType Expr
  | ErrorInferringKind SourceType
  | ErrorInApplication Expr SourceType Expr
  | ErrorInDataConstructor (ProperName ConstructorName)
  | ErrorInTypeConstructor (ProperName TypeName)
  | ErrorInBindingGroup (NonEmptyList Ident)
  | ErrorInDataBindingGroup (Array (ProperName TypeName))
  | ErrorInTypeSynonym (ProperName TypeName)
  | ErrorInValueDeclaration Ident
  | ErrorInTypeDeclaration Ident
  | ErrorInTypeClassDeclaration (ProperName ClassName)
  | ErrorInKindDeclaration (ProperName TypeName)
  | ErrorInRoleDeclaration (ProperName TypeName)
  | ErrorInForeignImport Ident
  | ErrorInForeignImportData (ProperName TypeName)
  | ErrorSolvingConstraint SourceConstraint
  | MissingConstructorImportForCoercible (Qualified (ProperName ConstructorName))
  | PositionedError (NonEmptyList SourceSpan)
  | RelatedPositions (NonEmptyList SourceSpan)

instance showErrorMessageHint :: Show ErrorMessageHint where
  show _ = "<ErrorMessageHint>"

data HintCategory
  = ExprHint | KindHint | CheckHint | PositionHint | SolverHint | DeclarationHint | OtherHint

derive instance eqHintCategory :: Eq HintCategory

instance showHintCategory :: Show HintCategory where
  show ExprHint       = "ExprHint"
  show KindHint       = "KindHint"
  show CheckHint      = "CheckHint"
  show PositionHint   = "PositionHint"
  show SolverHint     = "SolverHint"
  show DeclarationHint = "DeclarationHint"
  show OtherHint      = "OtherHint"

data UnknownsHint
  = NoUnknowns
  | Unknowns
  | UnknownsWithVtaRequiringArgs (NonEmptyList (Tuple (Qualified Ident) (Array (Array String))))

instance showUnknownsHint :: Show UnknownsHint where
  show NoUnknowns = "NoUnknowns"
  show Unknowns   = "Unknowns"
  show _          = "UnknownsWithVtaRequiringArgs"

-- | A PureScript module.
data Module = Module SourceSpan (Array Comment) ModuleName (Array Declaration) (Maybe (Array DeclarationRef))

instance showModule :: Show Module where
  show (Module _ _ mn _ _) = "(Module " <> show mn <> ")"

getModuleName :: Module -> ModuleName
getModuleName (Module _ _ name _ _) = name

getModuleSourceSpan :: Module -> SourceSpan
getModuleSourceSpan (Module ss _ _ _ _) = ss

getModuleDeclarations :: Module -> Array Declaration
getModuleDeclarations (Module _ _ _ decls _) = decls

addDefaultImport :: Qualified ModuleName -> Module -> Module
addDefaultImport (Qualified toImportAs toImport) m@(Module ss coms mn decls exps) =
  if any isExistingImport decls || mn == toImport then m
  else Module ss coms mn
    (Array.cons
      (ImportDeclaration (Tuple ss []) toImport Implicit toImportAs')
      decls)
    exps
  where
  toImportAs' = toMaybeModuleName toImportAs
  isExistingImport (ImportDeclaration _ mn' _ as')
    | mn' == toImport = case toImportAs' of
        Nothing -> true
        _ -> as' == toImportAs'
  isExistingImport _ = false

importPrim :: Module -> Module
importPrim =
  let primModName = ModuleName "Prim"
  in addDefaultImport (Qualified (ByModuleName primModName) primModName)
     <<< addDefaultImport (Qualified (BySourcePos (SourcePos { line: 0, column: 0 })) primModName)

data NameSource = UserNamed | CompilerNamed

derive instance eqNameSource :: Eq NameSource
derive instance ordNameSource :: Ord NameSource

instance showNameSource :: Show NameSource where
  show UserNamed    = "UserNamed"
  show CompilerNamed = "CompilerNamed"

data ExportSource = ExportSource
  { exportSourceImportedFrom :: Maybe ModuleName
  , exportSourceDefinedIn    :: ModuleName
  }

derive instance eqExportSource :: Eq ExportSource
derive instance ordExportSource :: Ord ExportSource

instance showExportSource :: Show ExportSource where
  show (ExportSource { exportSourceDefinedIn }) =
    "(ExportSource { definedIn: " <> show exportSourceDefinedIn <> " })"

data DeclarationRef
  = TypeClassRef SourceSpan (ProperName ClassName)
  | TypeOpRef SourceSpan (OpName TypeOpName)
  | TypeRef SourceSpan (ProperName TypeName) (Maybe (Array (ProperName ConstructorName)))
  | ValueRef SourceSpan Ident
  | ValueOpRef SourceSpan (OpName ValueOpName)
  | TypeInstanceRef SourceSpan Ident NameSource
  | ModuleRef SourceSpan ModuleName
  | ReExportRef SourceSpan ExportSource DeclarationRef

instance eqDeclarationRef :: Eq DeclarationRef where
  eq (TypeClassRef _ n) (TypeClassRef _ n')         = n == n'
  eq (TypeOpRef _ n) (TypeOpRef _ n')               = n == n'
  eq (TypeRef _ n d) (TypeRef _ n' d')              = n == n' && d == d'
  eq (ValueRef _ n) (ValueRef _ n')                 = n == n'
  eq (ValueOpRef _ n) (ValueOpRef _ n')             = n == n'
  eq (TypeInstanceRef _ n _) (TypeInstanceRef _ n' _) = n == n'
  eq (ModuleRef _ n) (ModuleRef _ n')               = n == n'
  eq (ReExportRef _ mn r) (ReExportRef _ mn' r')    = mn == mn' && r == r'
  eq _ _                                            = false

instance ordDeclarationRef :: Ord DeclarationRef where
  compare (TypeClassRef _ n) (TypeClassRef _ n')   = compare n n'
  compare (TypeOpRef _ n) (TypeOpRef _ n')         = compare n n'
  compare (TypeRef _ n d) (TypeRef _ n' d')        = compare n n' <> compare d d'
  compare (ValueRef _ n) (ValueRef _ n')           = compare n n'
  compare (ValueOpRef _ n) (ValueOpRef _ n')       = compare n n'
  compare (TypeInstanceRef _ n _) (TypeInstanceRef _ n' _) = compare n n'
  compare (ModuleRef _ n) (ModuleRef _ n')         = compare n n'
  compare (ReExportRef _ mn r) (ReExportRef _ mn' r') = compare mn mn' <> compare r r'
  compare x y = compare (orderOf x) (orderOf y)
    where
    orderOf (TypeClassRef _ _)     = 0
    orderOf (TypeOpRef _ _)        = 1
    orderOf (TypeRef _ _ _)        = 2
    orderOf (ValueRef _ _)         = 3
    orderOf (ValueOpRef _ _)       = 4
    orderOf (TypeInstanceRef _ _ _) = 5
    orderOf (ModuleRef _ _)        = 6
    orderOf (ReExportRef _ _ _)    = 7

instance showDeclarationRef :: Show DeclarationRef where
  show (TypeClassRef _ n) = "(TypeClassRef " <> show n <> ")"
  show (TypeOpRef _ n)    = "(TypeOpRef " <> show n <> ")"
  show (TypeRef _ n d)    = "(TypeRef " <> show n <> " " <> show d <> ")"
  show (ValueRef _ n)     = "(ValueRef " <> show n <> ")"
  show (ValueOpRef _ n)   = "(ValueOpRef " <> show n <> ")"
  show (TypeInstanceRef _ n _) = "(TypeInstanceRef " <> show n <> ")"
  show (ModuleRef _ n)    = "(ModuleRef " <> show n <> ")"
  show (ReExportRef _ _ r) = "(ReExportRef " <> show r <> ")"

declRefSourceSpan :: DeclarationRef -> SourceSpan
declRefSourceSpan (TypeRef ss _ _)     = ss
declRefSourceSpan (TypeOpRef ss _)     = ss
declRefSourceSpan (ValueRef ss _)      = ss
declRefSourceSpan (ValueOpRef ss _)    = ss
declRefSourceSpan (TypeClassRef ss _)  = ss
declRefSourceSpan (TypeInstanceRef ss _ _) = ss
declRefSourceSpan (ModuleRef ss _)     = ss
declRefSourceSpan (ReExportRef ss _ _) = ss

declRefName :: DeclarationRef -> Name
declRefName (TypeRef _ n _)            = TyName n
declRefName (TypeOpRef _ n)            = TyOpName n
declRefName (ValueRef _ n)             = IdentName n
declRefName (ValueOpRef _ n)           = ValOpName n
declRefName (TypeClassRef _ n)         = TyClassName n
declRefName (TypeInstanceRef _ n _)    = IdentName n
declRefName (ModuleRef _ n)            = ModName n
declRefName (ReExportRef _ _ ref)      = declRefName ref

getTypeRef :: DeclarationRef -> Maybe (Tuple (ProperName TypeName) (Maybe (Array (ProperName ConstructorName))))
getTypeRef (TypeRef _ name dctors) = Just (Tuple name dctors)
getTypeRef _ = Nothing

getValueRef :: DeclarationRef -> Maybe Ident
getValueRef (ValueRef _ name) = Just name
getValueRef _ = Nothing

getTypeClassRef :: DeclarationRef -> Maybe (ProperName ClassName)
getTypeClassRef (TypeClassRef _ name) = Just name
getTypeClassRef _ = Nothing

isModuleRef :: DeclarationRef -> Boolean
isModuleRef (ModuleRef _ _) = true
isModuleRef _ = false

data ImportDeclarationType
  = Implicit
  | Explicit (Array DeclarationRef)
  | Hiding (Array DeclarationRef)

derive instance eqImportDeclarationType :: Eq ImportDeclarationType

instance showImportDeclarationType :: Show ImportDeclarationType where
  show Implicit      = "Implicit"
  show (Explicit rs) = "(Explicit " <> show rs <> ")"
  show (Hiding rs)   = "(Hiding " <> show rs <> ")"

isExplicit :: ImportDeclarationType -> Boolean
isExplicit (Explicit _) = true
isExplicit _ = false

data RoleDeclarationData = RoleDeclarationData
  { rdeclSourceAnn :: SourceAnn
  , rdeclIdent     :: ProperName TypeName
  , rdeclRoles     :: Array Role
  }

derive instance eqRoleDeclarationData :: Eq RoleDeclarationData

instance showRoleDeclarationData :: Show RoleDeclarationData where
  show (RoleDeclarationData d) = "(RoleDeclarationData " <> show d.rdeclIdent <> ")"

data TypeDeclarationData = TypeDeclarationData
  { tydeclSourceAnn :: SourceAnn
  , tydeclIdent     :: Ident
  , tydeclType      :: SourceType
  }

derive instance eqTypeDeclarationData :: Eq TypeDeclarationData

instance showTypeDeclarationData :: Show TypeDeclarationData where
  show (TypeDeclarationData d) = "(TypeDeclarationData " <> show d.tydeclIdent <> ")"

getTypeDeclaration :: Declaration -> Maybe TypeDeclarationData
getTypeDeclaration (TypeDeclaration d) = Just d
getTypeDeclaration _ = Nothing

unwrapTypeDeclaration :: TypeDeclarationData -> Tuple Ident SourceType
unwrapTypeDeclaration (TypeDeclarationData td) = Tuple td.tydeclIdent td.tydeclType

data ValueDeclarationData a = ValueDeclarationData
  { valdeclSourceAnn  :: SourceAnn
  , valdeclIdent      :: Ident
  , valdeclName       :: NameKind
  , valdeclBinders    :: Array Binder
  , valdeclExpression :: a
  }

derive instance functorValueDeclarationData :: Functor ValueDeclarationData

instance showValueDeclarationData :: Show a => Show (ValueDeclarationData a) where
  show (ValueDeclarationData d) = "(ValueDeclarationData " <> show d.valdeclIdent <> ")"

getValueDeclaration :: Declaration -> Maybe (ValueDeclarationData (Array GuardedExpr))
getValueDeclaration (ValueDeclaration d) = Just d
getValueDeclaration _ = Nothing

data DataConstructorDeclaration = DataConstructorDeclaration
  { dataCtorAnn    :: SourceAnn
  , dataCtorName   :: ProperName ConstructorName
  , dataCtorFields :: Array (Tuple Ident SourceType)
  }

derive instance eqDataConstructorDeclaration :: Eq DataConstructorDeclaration

instance showDataConstructorDeclaration :: Show DataConstructorDeclaration where
  show (DataConstructorDeclaration d) = "(DataConstructorDeclaration " <> show d.dataCtorName <> ")"

mapDataCtorFields
  :: (Array (Tuple Ident SourceType) -> Array (Tuple Ident SourceType))
  -> DataConstructorDeclaration
  -> DataConstructorDeclaration
mapDataCtorFields f (DataConstructorDeclaration d) =
  DataConstructorDeclaration d { dataCtorFields = f d.dataCtorFields }

traverseDataCtorFields
  :: forall m
   . Monad m
  => (Array (Tuple Ident SourceType) -> m (Array (Tuple Ident SourceType)))
  -> DataConstructorDeclaration
  -> m DataConstructorDeclaration
traverseDataCtorFields f (DataConstructorDeclaration d) =
  (\fields -> DataConstructorDeclaration d { dataCtorFields = fields }) <$> f d.dataCtorFields

data InstanceDerivationStrategy = KnownClassStrategy | NewtypeStrategy

instance showInstanceDerivationStrategy :: Show InstanceDerivationStrategy where
  show KnownClassStrategy = "KnownClassStrategy"
  show NewtypeStrategy    = "NewtypeStrategy"

data TypeInstanceBody
  = DerivedInstance
  | NewtypeInstance
  | ExplicitInstance (Array Declaration)

instance showTypeInstanceBody :: Show TypeInstanceBody where
  show DerivedInstance       = "DerivedInstance"
  show NewtypeInstance       = "NewtypeInstance"
  show (ExplicitInstance ds) = "(ExplicitInstance [" <> show (Array.length ds) <> " decls])"

mapTypeInstanceBody :: (Array Declaration -> Array Declaration) -> TypeInstanceBody -> TypeInstanceBody
mapTypeInstanceBody f (ExplicitInstance ds) = ExplicitInstance (f ds)
mapTypeInstanceBody _ other = other

traverseTypeInstanceBody
  :: forall f. Applicative f
  => (Array Declaration -> f (Array Declaration))
  -> TypeInstanceBody
  -> f TypeInstanceBody
traverseTypeInstanceBody f (ExplicitInstance ds) = ExplicitInstance <$> f ds
traverseTypeInstanceBody _ other = pure other

data KindSignatureFor = DataSig | NewtypeSig | TypeSynonymSig | ClassSig

derive instance eqKindSignatureFor :: Eq KindSignatureFor
derive instance ordKindSignatureFor :: Ord KindSignatureFor

instance showKindSignatureFor :: Show KindSignatureFor where
  show DataSig       = "DataSig"
  show NewtypeSig    = "NewtypeSig"
  show TypeSynonymSig = "TypeSynonymSig"
  show ClassSig      = "ClassSig"

data ValueFixity = ValueFixity Fixity (Qualified (Either Ident (ProperName ConstructorName))) (OpName ValueOpName)

derive instance eqValueFixity :: Eq ValueFixity
derive instance ordValueFixity :: Ord ValueFixity

instance showValueFixity :: Show ValueFixity where
  show (ValueFixity f _ op) = "(ValueFixity " <> show f <> " " <> show op <> ")"

data TypeFixity = TypeFixity Fixity (Qualified (ProperName TypeName)) (OpName TypeOpName)

derive instance eqTypeFixity :: Eq TypeFixity
derive instance ordTypeFixity :: Ord TypeFixity

instance showTypeFixity :: Show TypeFixity where
  show (TypeFixity f _ op) = "(TypeFixity " <> show f <> " " <> show op <> ")"

-- | The main declaration type.
data Declaration
  = DataDeclaration SourceAnn DataDeclType (ProperName TypeName)
      (Array (Tuple String (Maybe SourceType)))
      (Array DataConstructorDeclaration)
  | DataBindingGroupDeclaration (NonEmptyList Declaration)
  | TypeSynonymDeclaration SourceAnn (ProperName TypeName)
      (Array (Tuple String (Maybe SourceType)))
      SourceType
  | KindDeclaration SourceAnn KindSignatureFor (ProperName TypeName) SourceType
  | RoleDeclaration RoleDeclarationData
  | TypeDeclaration TypeDeclarationData
  | ValueDeclaration (ValueDeclarationData (Array GuardedExpr))
  | BoundValueDeclaration SourceAnn Binder Expr
  | BindingGroupDeclaration (NonEmptyList (Tuple (Tuple SourceAnn Ident) (Tuple NameKind Expr)))
  | ExternDeclaration SourceAnn Ident SourceType
  | ExternDataDeclaration SourceAnn (ProperName TypeName) SourceType
  | FixityDeclaration SourceAnn (Either ValueFixity TypeFixity)
  | ImportDeclaration SourceAnn ModuleName ImportDeclarationType (Maybe ModuleName)
  | TypeClassDeclaration SourceAnn (ProperName ClassName)
      (Array (Tuple String (Maybe SourceType)))
      (Array SourceConstraint)
      (Array FunctionalDependency)
      (Array Declaration)
  | TypeInstanceDeclaration SourceAnn SourceAnn ChainId Int
      (Either String Ident)
      (Array SourceConstraint)
      (Qualified (ProperName ClassName))
      (Array SourceType)
      TypeInstanceBody

instance showDeclaration :: Show Declaration where
  show d = "(Declaration " <> show (declName d) <> ")"

declSourceAnn :: Declaration -> SourceAnn
declSourceAnn (DataDeclaration sa _ _ _ _) = sa
declSourceAnn (DataBindingGroupDeclaration ds) = declSourceAnn (NEL.head ds)
declSourceAnn (TypeSynonymDeclaration sa _ _ _) = sa
declSourceAnn (KindDeclaration sa _ _ _) = sa
declSourceAnn (RoleDeclaration (RoleDeclarationData rd)) = rd.rdeclSourceAnn
declSourceAnn (TypeDeclaration (TypeDeclarationData td)) = td.tydeclSourceAnn
declSourceAnn (ValueDeclaration (ValueDeclarationData vd)) = vd.valdeclSourceAnn
declSourceAnn (BoundValueDeclaration sa _ _) = sa
declSourceAnn (BindingGroupDeclaration ds) =
  case NEL.head ds of
    Tuple (Tuple sa _) _ -> sa
declSourceAnn (ExternDeclaration sa _ _) = sa
declSourceAnn (ExternDataDeclaration sa _ _) = sa
declSourceAnn (FixityDeclaration sa _) = sa
declSourceAnn (ImportDeclaration sa _ _ _) = sa
declSourceAnn (TypeClassDeclaration sa _ _ _ _ _) = sa
declSourceAnn (TypeInstanceDeclaration sa _ _ _ _ _ _ _ _) = sa

declSourceSpan :: Declaration -> SourceSpan
declSourceSpan = Tuple.fst <<< declSourceAnn

declName :: Declaration -> Maybe Name
declName (DataDeclaration _ _ n _ _)            = Just (TyName n)
declName (TypeSynonymDeclaration _ n _ _)        = Just (TyName n)
declName (ValueDeclaration (ValueDeclarationData vd)) = Just (IdentName vd.valdeclIdent)
declName (ExternDeclaration _ n _)              = Just (IdentName n)
declName (ExternDataDeclaration _ n _)          = Just (TyName n)
declName (FixityDeclaration _ (Left (ValueFixity _ _ n))) = Just (ValOpName n)
declName (FixityDeclaration _ (Right (TypeFixity _ _ n))) = Just (TyOpName n)
declName (TypeClassDeclaration _ n _ _ _ _)     = Just (TyClassName n)
declName (TypeInstanceDeclaration _ _ _ _ n _ _ _ _) =
  case n of
    Right ident -> Just (IdentName ident)
    Left _      -> Nothing
declName (RoleDeclaration (RoleDeclarationData rd)) = Just (TyName rd.rdeclIdent)
declName (ImportDeclaration _ _ _ _)            = Nothing
declName (BindingGroupDeclaration _)            = Nothing
declName (DataBindingGroupDeclaration _)        = Nothing
declName (BoundValueDeclaration _ _ _)          = Nothing
declName (KindDeclaration _ _ _ _)              = Nothing
declName (TypeDeclaration _)                    = Nothing

isValueDecl :: Declaration -> Boolean
isValueDecl (ValueDeclaration _) = true
isValueDecl _ = false

isDataDecl :: Declaration -> Boolean
isDataDecl (DataDeclaration _ _ _ _ _) = true
isDataDecl _ = false

isTypeSynonymDecl :: Declaration -> Boolean
isTypeSynonymDecl (TypeSynonymDeclaration _ _ _ _) = true
isTypeSynonymDecl _ = false

isImportDecl :: Declaration -> Boolean
isImportDecl (ImportDeclaration _ _ _ _) = true
isImportDecl _ = false

isRoleDecl :: Declaration -> Boolean
isRoleDecl (RoleDeclaration _) = true
isRoleDecl _ = false

isExternDataDecl :: Declaration -> Boolean
isExternDataDecl (ExternDataDeclaration _ _ _) = true
isExternDataDecl _ = false

isFixityDecl :: Declaration -> Boolean
isFixityDecl (FixityDeclaration _ _) = true
isFixityDecl _ = false

isExternDecl :: Declaration -> Boolean
isExternDecl (ExternDeclaration _ _ _) = true
isExternDecl _ = false

isTypeClassInstanceDecl :: Declaration -> Boolean
isTypeClassInstanceDecl (TypeInstanceDeclaration _ _ _ _ _ _ _ _ _) = true
isTypeClassInstanceDecl _ = false

isTypeClassDecl :: Declaration -> Boolean
isTypeClassDecl (TypeClassDeclaration _ _ _ _ _ _) = true
isTypeClassDecl _ = false

isKindDecl :: Declaration -> Boolean
isKindDecl (KindDeclaration _ _ _ _) = true
isKindDecl _ = false

flattenDecls :: Array Declaration -> Array Declaration
flattenDecls = Array.concatMap flattenOne
  where
  flattenOne (DataBindingGroupDeclaration decls) =
    Array.concatMap flattenOne (Array.fromFoldable decls)
  flattenOne d = [ d ]

data Guard = ConditionGuard Expr | PatternGuard Binder Expr

instance showGuard :: Show Guard where
  show (ConditionGuard e) = "(ConditionGuard " <> show e <> ")"
  show (PatternGuard b e) = "(PatternGuard " <> show b <> " " <> show e <> ")"

data GuardedExpr = GuardedExpr (Array Guard) Expr

instance showGuardedExpr :: Show GuardedExpr where
  show (GuardedExpr gs e) = "(GuardedExpr " <> show gs <> " " <> show e <> ")"

data WhereProvenance = FromWhere | FromLet

instance showWhereProvenance :: Show WhereProvenance where
  show FromWhere = "FromWhere"
  show FromLet   = "FromLet"

data CaseAlternative = CaseAlternative
  { caseAlternativeBinders :: Array Binder
  , caseAlternativeResult  :: Array GuardedExpr
  }

instance showCaseAlternative :: Show CaseAlternative where
  show _ = "<CaseAlternative>"

data DoNotationElement
  = DoNotationValue Expr
  | DoNotationBind Binder Expr
  | DoNotationLet (Array Declaration)
  | PositionedDoNotationElement SourceSpan (Array Comment) DoNotationElement

instance showDoNotationElement :: Show DoNotationElement where
  show (DoNotationValue e)  = "(DoNotationValue " <> show e <> ")"
  show (DoNotationBind b e) = "(DoNotationBind " <> show b <> " " <> show e <> ")"
  show (DoNotationLet _)    = "DoNotationLet"
  show (PositionedDoNotationElement _ _ e) = "(PositionedDoNotationElement " <> show e <> ")"

newtype AssocList k v = AssocList (Array (Tuple k v))

derive instance functorAssocList :: Functor (AssocList k)
derive instance foldableAssocList :: Foldable (AssocList k)
derive instance traversableAssocList :: Traversable (AssocList k)
derive instance eqAssocList :: (Eq k, Eq v) => Eq (AssocList k v)
derive instance ordAssocList :: (Ord k, Ord v) => Ord (AssocList k v)

instance showAssocList :: (Show k, Show v) => Show (AssocList k v) where
  show (AssocList xs) = "(AssocList " <> show xs <> ")"

newtype PathTree t = PathTree (AssocList PSString (PathNode t))

derive instance functorPathTree :: Functor PathTree
derive instance foldablePathTree :: Foldable PathTree
derive instance traversablePathTree :: Traversable PathTree
derive instance eqPathTree :: Eq t => Eq (PathTree t)
derive instance ordPathTree :: Ord t => Ord (PathTree t)

instance showPathTree :: Show t => Show (PathTree t) where
  show (PathTree al) = "(PathTree " <> show al <> ")"

data PathNode t = Leaf t | Branch (PathTree t)

derive instance functorPathNode :: Functor PathNode
derive instance foldablePathNode :: Foldable PathNode
derive instance traversablePathNode :: Traversable PathNode
derive instance eqPathNode :: Eq t => Eq (PathNode t)
derive instance ordPathNode :: Ord t => Ord (PathNode t)

instance showPathNode :: Show t => Show (PathNode t) where
  show (Leaf t)   = "(Leaf " <> show t <> ")"
  show (Branch pt) = "(Branch " <> show pt <> ")"

-- | The expression type. Core of the AST.
data Expr
  = Literal SourceSpan (Literal Expr)
  | UnaryMinus SourceSpan Expr
  | BinaryNoParens Expr Expr Expr
  | Parens Expr
  | Accessor PSString Expr
  | ObjectUpdate Expr (Array (Tuple PSString Expr))
  | ObjectUpdateNested Expr (PathTree Expr)
  | Abs Binder Expr
  | App Expr Expr
  | VisibleTypeApp Expr SourceType
  | Unused Expr
  | Var SourceSpan (Qualified Ident)
  | Op SourceSpan (Qualified (OpName ValueOpName))
  | IfThenElse Expr Expr Expr
  | Constructor SourceSpan (Qualified (ProperName ConstructorName))
  | Case (Array Expr) (Array CaseAlternative)
  | TypedValue Boolean Expr SourceType
  | Let WhereProvenance (Array Declaration) Expr
  | Do (Maybe ModuleName) (Array DoNotationElement)
  | Ado (Maybe ModuleName) (Array DoNotationElement) Expr
  | TypeClassDictionary SourceConstraint
      (Map QualifiedBy (Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array NamedDict))))
      (Array ErrorMessageHint)
  | DeferredDictionary (Qualified (ProperName ClassName)) (Array SourceType)
  | DerivedInstancePlaceholder (Qualified (ProperName ClassName)) InstanceDerivationStrategy
  | AnonymousArgument
  | Hole String
  | PositionedValue SourceSpan (Array Comment) Expr

instance showExpr :: Show Expr where
  show (Literal _ lit)       = "(Literal " <> show lit <> ")"
  show (UnaryMinus _ e)      = "(UnaryMinus " <> show e <> ")"
  show (BinaryNoParens f a b) = "(BinaryNoParens " <> show f <> " " <> show a <> " " <> show b <> ")"
  show (Parens e)            = "(Parens " <> show e <> ")"
  show (Accessor ps e)       = "(Accessor " <> show ps <> " " <> show e <> ")"
  show (ObjectUpdate e _)    = "(ObjectUpdate " <> show e <> ")"
  show (ObjectUpdateNested e _) = "(ObjectUpdateNested " <> show e <> ")"
  show (Abs b e)             = "(Abs " <> show b <> " " <> show e <> ")"
  show (App f a)             = "(App " <> show f <> " " <> show a <> ")"
  show (VisibleTypeApp e t)  = "(VisibleTypeApp " <> show e <> " " <> show t <> ")"
  show (Unused e)            = "(Unused " <> show e <> ")"
  show (Var _ q)             = "(Var " <> show q <> ")"
  show (Op _ q)              = "(Op " <> show q <> ")"
  show (IfThenElse c t e)    = "(IfThenElse " <> show c <> " " <> show t <> " " <> show e <> ")"
  show (Constructor _ q)     = "(Constructor " <> show q <> ")"
  show (Case es _)           = "(Case " <> show es <> ")"
  show (TypedValue b e t)    = "(TypedValue " <> show b <> " " <> show e <> " " <> show t <> ")"
  show (Let _ _ e)           = "(Let ... " <> show e <> ")"
  show (Do _ _)              = "Do"
  show (Ado _ _ e)           = "(Ado " <> show e <> ")"
  show (TypeClassDictionary _ _ _) = "TypeClassDictionary"
  show (DeferredDictionary q _) = "(DeferredDictionary " <> show q <> ")"
  show (DerivedInstancePlaceholder q _) = "(DerivedInstancePlaceholder " <> show q <> ")"
  show AnonymousArgument     = "AnonymousArgument"
  show (Hole h)              = "(Hole " <> show h <> ")"
  show (PositionedValue _ _ e) = "(PositionedValue " <> show e <> ")"

isTrueExpr :: Expr -> Boolean
isTrueExpr (Literal _ (BooleanLiteral true)) = true
isTrueExpr (Var _ (Qualified (ByModuleName (ModuleName "Prelude")) (Ident "otherwise"))) = true
isTrueExpr (Var _ (Qualified (ByModuleName (ModuleName "Data.Boolean")) (Ident "otherwise"))) = true
isTrueExpr (TypedValue _ e _) = isTrueExpr e
isTrueExpr (PositionedValue _ _ e) = isTrueExpr e
isTrueExpr _ = false

isAnonymousArgument :: Expr -> Boolean
isAnonymousArgument AnonymousArgument = true
isAnonymousArgument (PositionedValue _ _ e) = isAnonymousArgument e
isAnonymousArgument _ = false

getFixityDecl :: Declaration -> Maybe (Either ValueFixity TypeFixity)
getFixityDecl (FixityDeclaration _ fixity) = Just fixity
getFixityDecl _ = Nothing

getValueOpRef :: DeclarationRef -> Maybe (OpName ValueOpName)
getValueOpRef (ValueOpRef _ op) = Just op
getValueOpRef _ = Nothing

getTypeOpRef :: DeclarationRef -> Maybe (OpName TypeOpName)
getTypeOpRef (TypeOpRef _ op) = Just op
getTypeOpRef _ = Nothing
