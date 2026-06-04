module Language.PureScript.TypeChecker.Monad
  ( UnkLevel(..)
  , Substitution(..)
  , CheckState(..)
  , Unknown
  , emptySubstitution
  , emptyCheckState
  , insertUnkName
  , lookupUnkName
  , bindNames
  , bindTypes
  , withScopedTypeVars
  , withErrorMessageHint
  , getHints
  , rethrowWithPositionTC
  , warnAndRethrowWithPositionTC
  , withTypeClassDictionaries
  , getTypeClassDictionaries
  , getTypeClassDictionariesForModule
  , makeBindingGroupVisible
  , setVisible
  , getVisibility
  , checkVisibility
  , lookupTypeVariable
  , getEnv
  , getLocalContext
  , putEnv
  , modifyEnv
  , runCheck
  , guardWith
  , capturingSubstitution
  , withFreshSubstitution
  , withoutWarnings
  , unsafeCheckCurrentModule
  , bindLocalVariables
  , withBindingGroupVisible
  , lookupVariable
  , preservingNames
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Control.Monad.State (StateT, gets, modify_, runStateT)
import Control.Monad.State.Class (class MonadState, get, modify)
import Control.Monad.Writer.Class (class MonadWriter, censor, listen, tell)
import Data.Array as Array
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)

import Language.PureScript.AST.SourcePos (SourceAnn, SourcePos(..), SourceSpan(..), spanStart)
import Language.PureScript.Environment
  ( Environment(..)
  , NameKind(..)
  , NameVisibility(..)
  , TypeClassData(..)
  , TypeKind(..)
  )
import Language.PureScript.AST.Declarations (Context, ErrorMessageHint(..), ExportSource, ImportDeclarationType)
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , addHint
  , errorMessage
  , positionedError
  , rethrow
  , warnWithPosition
  )
import Language.PureScript.Names
  ( ClassName
  , ConstructorName
  , Ident(..)
  , ModuleName
  , Name(..)
  , ProperName(..)
  , Qualified(..)
  , QualifiedBy(..)
  , TypeName
  , byNullSourcePos
  , isBySourcePos
  )
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope(..))
import Language.PureScript.Types (SourceType, Type(..), srcTypeVar)

-- | Unification variable nesting level — a non-empty list of unknowns forming a path.
-- | Longer (more nested) paths sort before their root.
newtype UnkLevel = UnkLevel (NonEmptyList Int)

derive instance eqUnkLevel :: Eq UnkLevel

instance ordUnkLevel :: Ord UnkLevel where
  compare (UnkLevel a) (UnkLevel b) = go (NEL.toList a) (NEL.toList b)
    where
    go Nil Nil = EQ
    go _   Nil = LT
    go Nil _   = GT
    go (Cons x xs) (Cons y ys) = compare x y <> go xs ys

-- | Type substitution state
data Substitution = Substitution
  { substType     :: Map Int SourceType
  , substUnsolved :: Map Int (Tuple UnkLevel SourceType)
  , substNames    :: Map Int String
  }

emptySubstitution :: Substitution
emptySubstitution = Substitution { substType: Map.empty, substUnsolved: Map.empty, substNames: Map.empty }

-- | Unification variables (fresh integers)
type Unknown = Int

-- | Full type-checker state
data CheckState = CheckState
  { checkEnv :: Environment
  , checkNextType :: Int
  , checkNextSkolem :: Int
  , checkNextSkolemScope :: Int
  , checkCurrentModule :: Maybe ModuleName
  , checkCurrentModuleImports ::
      Array
        ( Tuple SourceAnn
          ( Tuple ModuleName
            ( Tuple ImportDeclarationType
              ( Tuple (Maybe ModuleName)
                (Map (ProperName TypeName) (Tuple (Array (ProperName ConstructorName)) ExportSource))
              )
            )
          )
        )
  , checkSubstitution :: Substitution
  , checkHints :: Array ErrorMessageHint
  , checkConstructorImportsForCoercible :: Set (Tuple ModuleName (Qualified (ProperName ConstructorName)))
  }

emptyCheckState :: Environment -> CheckState
emptyCheckState env = CheckState
  { checkEnv: env
  , checkNextType: 0
  , checkNextSkolem: 0
  , checkNextSkolemScope: 0
  , checkCurrentModule: Nothing
  , checkCurrentModuleImports: []
  , checkSubstitution: emptySubstitution
  , checkHints: []
  , checkConstructorImportsForCoercible: Set.empty
  }

insertUnkName :: forall m. MonadState CheckState m => Unknown -> String -> m Unit
insertUnkName u t = modify_ \(CheckState s) ->
  let Substitution sub = s.checkSubstitution
  in CheckState s { checkSubstitution = Substitution sub { substNames = Map.insert u t sub.substNames } }

lookupUnkName :: forall m. MonadState CheckState m => Unknown -> m (Maybe String)
lookupUnkName u = gets \(CheckState s) ->
  let Substitution sub = s.checkSubstitution
  in Map.lookup u sub.substNames

bindNames
  :: forall m a
   . MonadState CheckState m
  => Map (Qualified Ident) (Tuple (Tuple SourceType NameKind) NameVisibility)
  -> m a
  -> m a
bindNames newNames action = do
  CheckState orig <- get
  let Environment env = orig.checkEnv
  modify_ \(CheckState s) ->
    let Environment e = s.checkEnv
    in CheckState s { checkEnv = Environment e { names = Map.union newNames e.names } }
  a <- action
  modify_ \(CheckState s) ->
    let Environment e = s.checkEnv
    in CheckState s { checkEnv = Environment e { names = env.names } }
  pure a

bindTypes
  :: forall m a
   . MonadState CheckState m
  => Map (Qualified (ProperName TypeName)) (Tuple SourceType TypeKind)
  -> m a
  -> m a
bindTypes newTypes action = do
  CheckState orig <- get
  let Environment env = orig.checkEnv
  modify_ \(CheckState s) ->
    let Environment e = s.checkEnv
    in CheckState s { checkEnv = Environment e { types = Map.union newTypes e.types } }
  a <- action
  modify_ \(CheckState s) ->
    let Environment e = s.checkEnv
    in CheckState s { checkEnv = Environment e { types = env.types } }
  pure a

withScopedTypeVars
  :: forall m a
   . MonadState CheckState m
  => MonadWriter MultipleErrors m
  => ModuleName
  -> Array (Tuple String SourceType)
  -> m a
  -> m a
withScopedTypeVars mn ks ma = do
  CheckState orig <- get
  let Environment env = orig.checkEnv
  Array.foldl (\acc (Tuple name _) ->
    acc *> when (Map.member (Qualified (ByModuleName mn) (ProperName name)) env.types)
            (tell (errorMessage (ShadowedTypeVar name)))) (pure unit) ks
  bindTypes
    (Map.fromFoldable (map (\(Tuple name k) ->
      Tuple (Qualified (ByModuleName mn) (ProperName name)) (Tuple k ScopedTypeVar)) ks))
    ma

withErrorMessageHint
  :: forall m a
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => ErrorMessageHint
  -> m a
  -> m a
withErrorMessageHint hint action = do
  CheckState orig <- get
  modify_ \(CheckState s) -> CheckState s { checkHints = Array.cons hint s.checkHints }
  a <- rethrow (addHint hint) action
  modify_ \(CheckState s) -> CheckState s { checkHints = orig.checkHints }
  pure a

getHints :: forall m. MonadState CheckState m => m (Array ErrorMessageHint)
getHints = gets \(CheckState s) -> Array.reverse s.checkHints

rethrowWithPositionTC
  :: forall m a
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => SourceSpan
  -> m a
  -> m a
rethrowWithPositionTC pos = withErrorMessageHint (positionedError pos)

warnAndRethrowWithPositionTC
  :: forall m a
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => MonadWriter MultipleErrors m
  => SourceSpan
  -> m a
  -> m a
warnAndRethrowWithPositionTC pos = rethrowWithPositionTC pos <<< warnWithPosition pos

withTypeClassDictionaries
  :: forall m a
   . MonadState CheckState m
  => Array NamedDict
  -> m a
  -> m a
withTypeClassDictionaries entries action = do
  CheckState orig <- get
  let mentries :: Map QualifiedBy (Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array NamedDict)))
      mentries = Map.fromFoldableWith (Map.unionWith (Map.unionWith (<>)))
        (map (\nd ->
          let TypeClassDictionaryInScope d = nd
              Qualified qb _ = d.tcdValue
          in Tuple qb
               (Map.singleton d.tcdClassName
                 (Map.singleton d.tcdValue [nd]))) entries)
      Environment env = orig.checkEnv
  modify_ \(CheckState s) ->
    let Environment e = s.checkEnv
    in CheckState s
         { checkEnv = Environment e
             { typeClassDictionaries =
                 Map.unionWith (Map.unionWith (Map.unionWith (<>))) mentries e.typeClassDictionaries
             }
         }
  a <- action
  modify_ \(CheckState s) ->
    let Environment e = s.checkEnv
    in CheckState s { checkEnv = Environment e { typeClassDictionaries = env.typeClassDictionaries } }
  pure a

getTypeClassDictionaries
  :: forall m
   . MonadState CheckState m
  => m (Map QualifiedBy (Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array NamedDict))))
getTypeClassDictionaries = gets \(CheckState s) ->
  let Environment e = s.checkEnv in e.typeClassDictionaries

getTypeClassDictionariesForModule
  :: forall m
   . MonadState CheckState m
  => QualifiedBy
  -> m (Map (Qualified (ProperName ClassName)) (Map (Qualified Ident) (Array NamedDict)))
getTypeClassDictionariesForModule qb = do
  dicts <- getTypeClassDictionaries
  pure (case Map.lookup qb dicts of
    Nothing -> Map.empty
    Just m  -> m)

makeBindingGroupVisible :: forall m. MonadState CheckState m => m Unit
makeBindingGroupVisible = modifyEnv \(Environment e) ->
  Environment e { names = map makeVisible e.names }
  where
  makeVisible :: Tuple (Tuple SourceType NameKind) NameVisibility -> Tuple (Tuple SourceType NameKind) NameVisibility
  makeVisible (Tuple tk Undefined) = Tuple tk Defined
  makeVisible other = other

setVisible :: forall m. MonadState CheckState m => Qualified Ident -> m Unit
setVisible name = modifyEnv \(Environment e) ->
  Environment e { names = Map.update (\(Tuple tk _) -> Just (Tuple tk Defined)) name e.names }

getVisibility
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => Qualified Ident
  -> m NameVisibility
getVisibility name = do
  env <- getEnv
  let Environment e = env
  case Map.lookup name e.names of
    Nothing -> throwError (errorMessage (UnknownName (map (\i -> IdentName i) name)))
    Just (Tuple _ vis) -> pure vis

checkVisibility
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => Qualified Ident
  -> m Unit
checkVisibility name@(Qualified _ var) = do
  vis <- getVisibility name
  case vis of
    Undefined -> throwError (errorMessage (CycleInDeclaration var))
    _ -> pure unit

lookupTypeVariable
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => ModuleName
  -> Qualified (ProperName TypeName)
  -> m SourceType
lookupTypeVariable currentModule (Qualified qb name) = do
  env <- getEnv
  let Environment e = env
      qb' = ByModuleName (case qb of
              ByModuleName m -> m
              BySourcePos _ -> currentModule)
  case Map.lookup (Qualified qb' name) e.types of
    Nothing -> throwError (errorMessage (UndefinedTypeVariable name))
    Just (Tuple k _) -> pure k

getEnv :: forall m. MonadState CheckState m => m Environment
getEnv = gets \(CheckState s) -> s.checkEnv

getLocalContext :: forall m. MonadState CheckState m => m Context
getLocalContext = do
  env <- getEnv
  let Environment e = env
  pure (Array.mapMaybe getLocal (Map.toUnfoldable e.names))
  where
  getLocal :: Tuple (Qualified Ident) (Tuple (Tuple SourceType NameKind) NameVisibility) -> Maybe (Tuple Ident SourceType)
  getLocal (Tuple (Qualified qb ident@(Ident _)) (Tuple (Tuple ty _) Defined))
    | isBySourcePos qb = Just (Tuple ident ty)
  getLocal _ = Nothing

putEnv :: forall m. MonadState CheckState m => Environment -> m Unit
putEnv env = modify_ \(CheckState s) -> CheckState s { checkEnv = env }

modifyEnv :: forall m. MonadState CheckState m => (Environment -> Environment) -> m Unit
modifyEnv f = modify_ \(CheckState s) -> CheckState s { checkEnv = f s.checkEnv }

runCheck :: forall m a. Functor m => CheckState -> StateT CheckState m a -> m (Tuple a Environment)
runCheck st check = map (\(Tuple a (CheckState s)) -> Tuple a s.checkEnv) (runStateT check st)

guardWith :: forall e m. MonadError e m => e -> Boolean -> m Unit
guardWith _ true  = pure unit
guardWith e false = throwError e

capturingSubstitution
  :: forall m a b
   . MonadState CheckState m
  => (a -> Substitution -> b)
  -> m a
  -> m b
capturingSubstitution f ma = do
  a <- ma
  subst <- gets \(CheckState s) -> s.checkSubstitution
  pure (f a subst)

withFreshSubstitution :: forall m a. MonadState CheckState m => m a -> m a
withFreshSubstitution ma = do
  CheckState orig <- get
  modify_ \(CheckState s) -> CheckState s { checkSubstitution = emptySubstitution }
  a <- ma
  modify_ \(CheckState s) -> CheckState s { checkSubstitution = orig.checkSubstitution }
  pure a

withoutWarnings :: forall w m a. MonadWriter w m => m a -> m (Tuple a w)
withoutWarnings = censor (const mempty) <<< listen

unsafeCheckCurrentModule
  :: forall m
   . MonadError MultipleErrors m
  => MonadState CheckState m
  => m ModuleName
unsafeCheckCurrentModule = gets (\(CheckState s) -> s.checkCurrentModule) >>= case _ of
  Nothing -> throwError (errorMessage (InternalCompilerError "callstack" "No module name set in scope"))
  Just name -> pure name

-- | Temporarily bind a collection of names to local variables
bindLocalVariables
  :: forall m a
   . MonadState CheckState m
  => Array (Tuple (Tuple SourceSpan Ident) (Tuple SourceType NameVisibility))
  -> m a
  -> m a
bindLocalVariables bindings =
  bindNames (Map.fromFoldable (map toEntry bindings))
  where
  toEntry (Tuple (Tuple ss name) (Tuple ty visibility)) =
    Tuple (Qualified (BySourcePos (spanStart ss)) name) (Tuple (Tuple ty Private) visibility)

-- | Perform an action while preserving the names from the Environment
preservingNames :: forall m a. MonadState CheckState m => m a -> m a
preservingNames action = do
  CheckState orig <- get
  let Environment env = orig.checkEnv
  a <- action
  modify_ \(CheckState s) ->
    let Environment e = s.checkEnv
    in CheckState s { checkEnv = Environment e { names = env.names } }
  pure a

-- | Update the visibility of all names to Defined in the scope of the provided action
withBindingGroupVisible :: forall m a. MonadState CheckState m => m a -> m a
withBindingGroupVisible action = preservingNames (makeBindingGroupVisible *> action)

-- | Lookup the type of a value by name in the Environment
lookupVariable
  :: forall m
   . MonadState CheckState m
  => MonadError MultipleErrors m
  => Qualified Ident
  -> m SourceType
lookupVariable qual = do
  env <- getEnv
  let Environment e = env
  case Map.lookup qual e.names of
    Nothing -> throwError (errorMessage (NameIsUndefined (disqualify qual)))
    Just (Tuple (Tuple ty _) _) -> pure ty
  where
  disqualify :: Qualified Ident -> Ident
  disqualify (Qualified _ i) = i
