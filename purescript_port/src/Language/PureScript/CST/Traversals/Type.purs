module Language.PureScript.CST.Traversals.Type
  ( everythingOnTypes
  ) where

import Prelude

import Data.Array (foldr, null) as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Language.PureScript.CST.Traversals (everythingOnSeparated)
import Language.PureScript.CST.Types
  ( Constraint(..)
  , Labeled(..)
  , Row(..)
  , Type(..)
  , Wrapped(..)
  )

everythingOnTypes :: forall a r. (r -> r -> r) -> (Type a -> r) -> Type a -> r
everythingOnTypes op k = goTy
  where
  goTy ty = case ty of
    TypeVar _ _           -> k ty
    TypeConstructor _ _   -> k ty
    TypeWildcard _ _      -> k ty
    TypeHole _ _          -> k ty
    TypeString _ _ _      -> k ty
    TypeInt _ _ _ _       -> k ty
    TypeRow _ (Wrapped w)    -> goRow ty w.wrpValue
    TypeRecord _ (Wrapped w) -> goRow ty w.wrpValue
    TypeForall _ _ _ _ ty2   -> k ty `op` goTy ty2
    TypeKinded _ ty2 _ ty3   -> k ty `op` (goTy ty2 `op` goTy ty3)
    TypeApp _ ty2 ty3        -> k ty `op` (goTy ty2 `op` goTy ty3)
    TypeOp _ ty2 _ ty3       -> k ty `op` (goTy ty2 `op` goTy ty3)
    TypeOpName _ _           -> k ty
    TypeArr _ ty2 _ ty3      -> k ty `op` (goTy ty2 `op` goTy ty3)
    TypeArrName _ _          -> k ty
    TypeConstrained _ c _ ty2 ->
      let tys = constraintTys c
      in if Array.null tys
         then k ty `op` goTy ty2
         else k ty `op` (Array.foldr (\t acc -> k t `op` acc) (goTy ty2) tys)
    TypeParens _ (Wrapped w) -> k ty `op` goTy w.wrpValue
    TypeUnaryRow _ _ ty2     -> k ty `op` goTy ty2

  goRow :: Type a -> Row a -> r
  goRow ty (Row { rowLabels, rowTail }) = case Tuple rowLabels rowTail of
    Tuple Nothing Nothing      -> k ty
    Tuple Nothing (Just (Tuple _ ty2)) -> k ty `op` goTy ty2
    Tuple (Just lbls) Nothing  ->
      k ty `op` everythingOnSeparated op (\(Labeled { lblValue }) -> goTy lblValue) lbls
    Tuple (Just lbls) (Just (Tuple _ ty2)) ->
      k ty `op` (everythingOnSeparated op (\(Labeled { lblValue }) -> goTy lblValue) lbls `op` goTy ty2)

  constraintTys :: Constraint a -> Array (Type a)
  constraintTys (Constraint _ _ tys)         = tys
  constraintTys (ConstraintParens _ (Wrapped w)) = constraintTys w.wrpValue
