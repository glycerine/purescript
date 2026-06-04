module Language.PureScript.Sugar.Names.Common
  ( warnDuplicateRefs
  ) where

import Prelude

import Control.Monad.Writer.Class (class MonadWriter, tell)
import Data.Array as Array
import Data.Array (mapMaybe, nub, (\\))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Language.PureScript.AST.Declarations (DeclarationRef(..))
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Errors
  ( MultipleErrors
  , SimpleErrorMessage
  , errorMessage
  , warnWithPosition
  )
import Language.PureScript.Names (Name(..))

warnDuplicateRefs
  :: forall m
   . MonadWriter MultipleErrors m
  => SourceSpan
  -> (Name -> SimpleErrorMessage)
  -> Array DeclarationRef
  -> m Unit
warnDuplicateRefs pos toError refs = do
  let withoutCtors = map deleteCtors refs
      dupeRefs     = mapMaybe (refToName pos) (removeUnique withoutCtors)
      dupeCtors    = Array.concatMap (extractCtors pos) refs

  for_ (dupeRefs <> dupeCtors) \(Tuple pos' name) ->
    warnWithPosition pos' (tell (errorMessage (toError name)))

  where

  removeUnique :: forall a. Ord a => Array a -> Array a
  removeUnique arr =
    let sorted  = Array.sort arr
        grouped = groupConsecutive sorted
    in Array.concatMap (Array.drop 1) grouped

  groupConsecutive :: forall a. Eq a => Array a -> Array (Array a)
  groupConsecutive [] = []
  groupConsecutive arr = case Array.uncons arr of
    Nothing -> []
    Just { head: x, tail: xs } ->
      let same = Array.takeWhile (_ == x) xs
          rest = Array.dropWhile (_ == x) xs
      in Array.cons (Array.cons x same) (groupConsecutive rest)

  deleteCtors :: DeclarationRef -> DeclarationRef
  deleteCtors (TypeRef sa pn _) = TypeRef sa pn Nothing
  deleteCtors other = other

  extractCtors :: SourceSpan -> DeclarationRef -> Array (Tuple SourceSpan Name)
  extractCtors pos' (TypeRef _ _ (Just dctors)) =
    let dupes = dctors \\ nub dctors
    in map (\ctor -> Tuple pos' (DctorName ctor)) dupes
  extractCtors _ _ = []

  refToName :: SourceSpan -> DeclarationRef -> Maybe (Tuple SourceSpan Name)
  refToName pos' (TypeRef _ name _)      = Just (Tuple pos' (TyName name))
  refToName pos' (TypeOpRef _ op)        = Just (Tuple pos' (TyOpName op))
  refToName pos' (ValueRef _ name)       = Just (Tuple pos' (IdentName name))
  refToName pos' (ValueOpRef _ op)       = Just (Tuple pos' (ValOpName op))
  refToName pos' (TypeClassRef _ name)   = Just (Tuple pos' (TyClassName name))
  refToName pos' (ModuleRef _ name)      = Just (Tuple pos' (ModName name))
  refToName _ _                          = Nothing
