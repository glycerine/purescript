-- | Role inference and checking (stub — full implementation deferred).
module Language.PureScript.TypeChecker.Roles
  ( lookupRoles
  , checkRoles
  , checkRoleDeclarationArity
  , inferRoles
  , inferDataBindingGroupRoles
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..), fst, snd)

import Language.PureScript.AST.Declarations
  ( DataConstructorDeclaration
  , RoleDeclarationData(..)
  )
import Language.PureScript.Environment
  ( Environment(..)
  , TypeKind(..)
  )
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage(..)
  , errorMessage
  )
import Language.PureScript.Names
  ( ModuleName
  , ProperName
  , Qualified(..)
  , QualifiedBy(..)
  , TypeName
  )
import Language.PureScript.Roles (Role(..))
import Language.PureScript.Types (SourceType)

-- | Look up the roles for a type in the environment.
lookupRoles
  :: Environment
  -> Qualified (ProperName TypeName)
  -> Array Role
lookupRoles (Environment env) tyName =
  fromMaybe [] do
    Tuple _ tk <- Map.lookup tyName env.types
    typeKindRoles tk
  where
  typeKindRoles (DataType _ _ _) = Nothing
  typeKindRoles _ = Nothing

-- | Check that declared roles are not more permissive than inferred roles.
checkRoles
  :: forall m
   . MonadError MultipleErrors m
  => Array (Tuple String (Tuple (Maybe SourceType) Role))
  -> Array Role
  -> m Unit
checkRoles tyArgs declaredRoles = do
  let pairs = Array.zip tyArgs declaredRoles
  traverse_ (\(Tuple (Tuple var (Tuple _ inf)) dec) ->
    if inf < dec
      then throwError (errorMessage (RoleMismatch var inf dec))
      else pure unit
  ) pairs

-- | Check that a role declaration has the correct arity.
checkRoleDeclarationArity
  :: forall m
   . MonadError MultipleErrors m
  => ProperName TypeName
  -> Array Role
  -> Int
  -> m Unit
checkRoleDeclarationArity tyName roles expected = do
  let actual = Array.length roles
  if expected /= actual
    then throwError (errorMessage (RoleDeclarationArityMismatch tyName expected actual))
    else pure unit

-- | Infer roles for a data type (stub — returns Phantom for all type parameters).
inferRoles
  :: Environment
  -> ModuleName
  -> ProperName TypeName
  -> Array (Tuple String (Maybe SourceType))
  -> Array DataConstructorDeclaration
  -> Array Role
inferRoles env mn tyName tyArgs _ctors =
  map (const Phantom) tyArgs

-- | Infer roles for a binding group of data type declarations (stub).
inferDataBindingGroupRoles
  :: Environment
  -> ModuleName
  -> Array RoleDeclarationData
  -> Array (Tuple (ProperName TypeName) (Tuple (Array (Tuple String (Maybe SourceType))) (Array DataConstructorDeclaration)))
  -> Qualified (ProperName TypeName)
  -> Array (Tuple String (Maybe SourceType))
  -> Array Role
inferDataBindingGroupRoles env mn _roleDecls _group _tyName tyArgs =
  map (const Phantom) tyArgs
