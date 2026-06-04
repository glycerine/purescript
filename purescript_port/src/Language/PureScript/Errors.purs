module Language.PureScript.Errors
  ( SimpleErrorMessage(..)
  , ErrorMessage(..)
  , MultipleErrors(..)
  , Level(..)
  , errorMessage
  , errorMessage'
  , errorMessage''
  , singleError
  , onErrorMessages
  , addHint
  , addHints
  , rethrow
  , withPosition
  , rethrowWithPosition
  , positionedError
  , parU
  , nonEmpty
  , unwrapErrorMessage
  , internalCompilerError
  , warnWithPosition
  , warnAndRethrow
  , warnAndRethrowWithPosition
  , withoutPosition
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError, catchError)
import Control.Monad.Writer.Class (class MonadWriter, censor)
import Data.Traversable (traverse)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)

import Language.PureScript.AST.Declarations
  ( CaseAlternative
  , Context
  , DeclarationRef
  , ErrorMessageHint(..)
  , Expr
  , ImportDeclarationType
  , KindSignatureFor
  , TypeSearch
  , UnknownsHint
  )
import Language.PureScript.AST.SourcePos (SourceSpan(..), nullSourceSpan)
import Language.PureScript.Names
  ( Ident
  , ModuleName
  , Name
  , OpName
  , ProperName
  , Qualified
  , AnyOpName
  , ValueOpName
  , TypeOpName
  , ClassName
  , ConstructorName
  , TypeName
  )
import Language.PureScript.AST.Operators (Associativity)
import Language.PureScript.AST.Binders (Binder)
import Language.PureScript.Label (Label)
import Language.PureScript.Roles (Role)
import Language.PureScript.Types (SourceConstraint, SourceType)

data SimpleErrorMessage
  = InternalCompilerError String String
  | ModuleNotFound ModuleName
  | ErrorParsingCSTModule String
  | WarningParsingCSTModule String
  | MissingFFIModule ModuleName
  | UnnecessaryFFIModule ModuleName String
  | MissingFFIImplementations ModuleName (Array Ident)
  | UnusedFFIImplementations ModuleName (Array Ident)
  | InvalidFFIIdentifier ModuleName String
  | DeprecatedFFIPrime ModuleName String
  | DeprecatedFFICommonJSModule ModuleName String
  | UnsupportedFFICommonJSExports ModuleName (Array String)
  | UnsupportedFFICommonJSImports ModuleName (Array String)
  | FileIOError String String
  | InfiniteType SourceType
  | InfiniteKind SourceType
  | MultipleValueOpFixities (OpName ValueOpName)
  | MultipleTypeOpFixities (OpName TypeOpName)
  | OrphanTypeDeclaration Ident
  | OrphanKindDeclaration (ProperName TypeName)
  | OrphanRoleDeclaration (ProperName TypeName)
  | RedefinedIdent Ident
  | OverlappingNamesInLet Ident
  | UnknownName (Qualified Name)
  | UnknownImport ModuleName Name
  | UnknownImportDataConstructor ModuleName (ProperName TypeName) (ProperName ConstructorName)
  | UnknownExport Name
  | UnknownExportDataConstructor (ProperName TypeName) (ProperName ConstructorName)
  | ScopeConflict Name (Array ModuleName)
  | ScopeShadowing Name (Maybe ModuleName) (Array ModuleName)
  | DeclConflict Name Name
  | ExportConflict (Qualified Name) (Qualified Name)
  | DuplicateModule ModuleName
  | DuplicateTypeClass (ProperName ClassName) SourceSpan
  | DuplicateInstance Ident SourceSpan
  | DuplicateTypeArgument String
  | InvalidDoBind
  | InvalidDoLet
  | CycleInDeclaration Ident
  | CycleInTypeSynonym (NonEmptyList (ProperName TypeName))
  | CycleInTypeClassDeclaration (NonEmptyList (Qualified (ProperName ClassName)))
  | CycleInKindDeclaration (NonEmptyList (Qualified (ProperName TypeName)))
  | CycleInModules (NonEmptyList ModuleName)
  | NameIsUndefined Ident
  | UndefinedTypeVariable (ProperName TypeName)
  | PartiallyAppliedSynonym (Qualified (ProperName TypeName))
  | EscapedSkolem String (Maybe SourceSpan) SourceType
  | TypesDoNotUnify SourceType SourceType
  | KindsDoNotUnify SourceType SourceType
  | ConstrainedTypeUnified SourceType SourceType
  | OverlappingInstances (Qualified (ProperName ClassName)) (Array SourceType) (Array (Qualified (Either SourceType Ident)))
  | NoInstanceFound SourceConstraint (Array (Qualified (Either SourceType Ident))) UnknownsHint
  | AmbiguousTypeVariables SourceType (Array (Tuple String Int))
  | UnknownClass (Qualified (ProperName ClassName))
  | PossiblyInfiniteInstance (Qualified (ProperName ClassName)) (Array SourceType)
  | PossiblyInfiniteCoercibleInstance
  | CannotDerive (Qualified (ProperName ClassName)) (Array SourceType)
  | InvalidDerivedInstance (Qualified (ProperName ClassName)) (Array SourceType) Int
  | ExpectedTypeConstructor (Qualified (ProperName ClassName)) (Array SourceType) SourceType
  | InvalidNewtypeInstance (Qualified (ProperName ClassName)) (Array SourceType)
  | MissingNewtypeSuperclassInstance (Qualified (ProperName ClassName)) (Qualified (ProperName ClassName)) (Array SourceType)
  | UnverifiableSuperclassInstance (Qualified (ProperName ClassName)) (Qualified (ProperName ClassName)) (Array SourceType)
  | CannotFindDerivingType (ProperName TypeName)
  | DuplicateLabel Label (Maybe Expr)
  | DuplicateValueDeclaration Ident
  | ArgListLengthsDiffer Ident
  | OverlappingArgNames (Maybe Ident)
  | MissingClassMember (NonEmptyList (Tuple Ident SourceType))
  | ExtraneousClassMember Ident (Qualified (ProperName ClassName))
  | ExpectedType SourceType SourceType
  | IncorrectConstructorArity (Qualified (ProperName ConstructorName)) Int Int
  | ExprDoesNotHaveType Expr SourceType
  | PropertyIsMissing Label
  | AdditionalProperty Label
  | OrphanInstance Ident (Qualified (ProperName ClassName)) (Array ModuleName) (Array SourceType)
  | InvalidNewtype (ProperName TypeName)
  | InvalidInstanceHead SourceType
  | TransitiveExportError DeclarationRef (Array DeclarationRef)
  | TransitiveDctorExportError DeclarationRef (Array (ProperName ConstructorName))
  | HiddenConstructors DeclarationRef (Qualified (ProperName ClassName))
  | ShadowedName Ident
  | ShadowedTypeVar String
  | UnusedTypeVar String
  | UnusedName Ident
  | UnusedDeclaration Ident
  | WildcardInferredType SourceType Context
  | HoleInferredType String SourceType Context (Maybe TypeSearch)
  | MissingTypeDeclaration Ident SourceType
  | MissingKindDeclaration KindSignatureFor (ProperName TypeName) SourceType
  | OverlappingPattern (Array (Array Binder)) Boolean
  | IncompleteExhaustivityCheck
  | ImportHidingModule ModuleName
  | UnusedImport ModuleName (Maybe ModuleName)
  | UnusedExplicitImport ModuleName (Array Name) (Maybe ModuleName) (Array DeclarationRef)
  | UnusedDctorImport ModuleName (ProperName TypeName) (Maybe ModuleName) (Array DeclarationRef)
  | UnusedDctorExplicitImport ModuleName (ProperName TypeName) (Array (ProperName ConstructorName)) (Maybe ModuleName) (Array DeclarationRef)
  | DuplicateSelectiveImport ModuleName
  | DuplicateImport ModuleName ImportDeclarationType (Maybe ModuleName)
  | DuplicateImportRef Name
  | DuplicateExportRef Name
  | IntOutOfRange Int String Int Int
  | ImplicitQualifiedImport ModuleName ModuleName (Array DeclarationRef)
  | ImplicitQualifiedImportReExport ModuleName ModuleName (Array DeclarationRef)
  | ImplicitImport ModuleName (Array DeclarationRef)
  | HidingImport ModuleName (Array DeclarationRef)
  | CaseBinderLengthDiffers Int (Array Binder)
  | IncorrectAnonymousArgument
  | InvalidOperatorInBinder (Qualified (OpName ValueOpName)) (Qualified Ident)
  | CannotGeneralizeRecursiveFunction Ident SourceType
  | CannotDeriveNewtypeForData (ProperName TypeName)
  | ExpectedWildcard (ProperName TypeName)
  | CannotUseBindWithDo Ident
  | ClassInstanceArityMismatch Ident (Qualified (ProperName ClassName)) Int Int
  | UserDefinedWarning SourceType
  | CannotDefinePrimModules ModuleName
  | MixedAssociativityError (NonEmptyList (Tuple (Qualified (OpName AnyOpName)) Associativity))
  | NonAssociativeError (NonEmptyList (Qualified (OpName AnyOpName)))
  | QuantificationCheckFailureInKind String
  | QuantificationCheckFailureInType (Array Int) SourceType
  | VisibleQuantificationCheckFailureInType String
  | UnsupportedTypeInKind SourceType
  | RoleMismatch String Role Role
  | InvalidCoercibleInstanceDeclaration (Array SourceType)
  | UnsupportedRoleDeclaration
  | RoleDeclarationArityMismatch (ProperName TypeName) Int Int
  | DuplicateRoleDeclaration (ProperName TypeName)
  | CannotDeriveInvalidConstructorArg (Qualified (ProperName ClassName)) (Array (Qualified (ProperName ClassName))) Boolean
  | CannotSkipTypeApplication SourceType
  | CannotApplyExpressionOfTypeOnType SourceType SourceType

instance showSimpleErrorMessage :: Show SimpleErrorMessage where
  show _ = "<SimpleErrorMessage>"

data ErrorMessage = ErrorMessage (Array ErrorMessageHint) SimpleErrorMessage

instance showErrorMessage :: Show ErrorMessage where
  show _ = "<ErrorMessage>"

newtype MultipleErrors = MultipleErrors (Array ErrorMessage)

instance semigroupMultipleErrors :: Semigroup MultipleErrors where
  append (MultipleErrors xs) (MultipleErrors ys) = MultipleErrors (xs <> ys)

instance monoidMultipleErrors :: Monoid MultipleErrors where
  mempty = MultipleErrors []

instance showMultipleErrors :: Show MultipleErrors where
  show _ = "<MultipleErrors>"

data Level = Error | Warning

nonEmpty :: MultipleErrors -> Boolean
nonEmpty (MultipleErrors errs) = not (Array.null errs)

errorMessage :: SimpleErrorMessage -> MultipleErrors
errorMessage err = MultipleErrors [ErrorMessage [] err]

errorMessage' :: SourceSpan -> SimpleErrorMessage -> MultipleErrors
errorMessage' ss err = MultipleErrors [ErrorMessage [positionedError ss] err]

errorMessage'' :: NonEmptyList SourceSpan -> SimpleErrorMessage -> MultipleErrors
errorMessage'' sss err = MultipleErrors [ErrorMessage [PositionedError sss] err]

singleError :: ErrorMessage -> MultipleErrors
singleError err = MultipleErrors [err]

onErrorMessages :: (ErrorMessage -> ErrorMessage) -> MultipleErrors -> MultipleErrors
onErrorMessages f (MultipleErrors errs) = MultipleErrors (map f errs)

addHint :: ErrorMessageHint -> MultipleErrors -> MultipleErrors
addHint hint = addHints [hint]

addHints :: Array ErrorMessageHint -> MultipleErrors -> MultipleErrors
addHints hints = onErrorMessages (\(ErrorMessage hints' se) -> ErrorMessage (hints <> hints') se)

rethrow :: forall e m a. MonadError e m => (e -> e) -> m a -> m a
rethrow f m = catchError m (throwError <<< f)

withPosition :: SourceSpan -> ErrorMessage -> ErrorMessage
withPosition (SourceSpan ss) err
  | ss.name == "" && ss.start == ss.end = err
withPosition pos (ErrorMessage hints se) = ErrorMessage (Array.cons (positionedError pos) hints) se

rethrowWithPosition :: forall m a. MonadError MultipleErrors m => SourceSpan -> m a -> m a
rethrowWithPosition pos = rethrow (onErrorMessages (withPosition pos))

positionedError :: SourceSpan -> ErrorMessageHint
positionedError ss = PositionedError (NEL.singleton ss)

unwrapErrorMessage :: ErrorMessage -> SimpleErrorMessage
unwrapErrorMessage (ErrorMessage _ se) = se

parU :: forall m a b. MonadError MultipleErrors m => Array a -> (a -> m b) -> m (Array b)
parU xs f = do
  results <- traverse withError xs
  let errs = Array.mapMaybe getLeft results
  let rs   = Array.mapMaybe getRight results
  case errs of
    [] -> pure rs
    _  -> throwError (Array.foldl (<>) mempty errs)
  where
  withError :: a -> m (Either MultipleErrors b)
  withError x = catchError (map Right (f x)) (pure <<< Left)

  getLeft :: Either MultipleErrors b -> Maybe MultipleErrors
  getLeft (Left e)  = Just e
  getLeft (Right _) = Nothing

  getRight :: Either MultipleErrors b -> Maybe b
  getRight (Left _)  = Nothing
  getRight (Right b) = Just b

internalCompilerError :: forall m a. MonadError MultipleErrors m => String -> m a
internalCompilerError msg = throwError (errorMessage (InternalCompilerError "callstack" msg))

warnWithPosition :: forall m a. MonadWriter MultipleErrors m => SourceSpan -> m a -> m a
warnWithPosition pos = censor (onErrorMessages (withPosition pos))

warnAndRethrow :: forall e m a. MonadError e m => MonadWriter e m => (e -> e) -> m a -> m a
warnAndRethrow f = rethrow f <<< censor f

warnAndRethrowWithPosition :: forall m a. MonadError MultipleErrors m => MonadWriter MultipleErrors m => SourceSpan -> m a -> m a
warnAndRethrowWithPosition pos = rethrowWithPosition pos <<< warnWithPosition pos

withoutPosition :: ErrorMessage -> ErrorMessage
withoutPosition (ErrorMessage hints se) = ErrorMessage (Array.filter go hints) se
  where
  go (PositionedError _) = false
  go _ = true
