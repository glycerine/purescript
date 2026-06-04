-- | Metadata annotations for core functional representation.
module Language.PureScript.CoreFn.Meta
  ( ConstructorType(..)
  , Meta(..)
  ) where

import Prelude

import Language.PureScript.Names (Ident)

data ConstructorType
  = ProductType
  | SumType

derive instance eqConstructorType :: Eq ConstructorType
derive instance ordConstructorType :: Ord ConstructorType

instance showConstructorType :: Show ConstructorType where
  show ProductType = "ProductType"
  show SumType = "SumType"

data Meta
  = IsConstructor ConstructorType (Array Ident)
  | IsNewtype
  | IsTypeClassConstructor
  | IsForeign
  | IsWhere
  | IsSynthetic

derive instance eqMeta :: Eq Meta
derive instance ordMeta :: Ord Meta

instance showMeta :: Show Meta where
  show (IsConstructor ct fields) = "(IsConstructor " <> show ct <> " " <> show fields <> ")"
  show IsNewtype = "IsNewtype"
  show IsTypeClassConstructor = "IsTypeClassConstructor"
  show IsForeign = "IsForeign"
  show IsWhere = "IsWhere"
  show IsSynthetic = "IsSynthetic"
