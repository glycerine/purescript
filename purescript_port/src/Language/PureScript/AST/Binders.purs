module Language.PureScript.AST.Binders
  ( Binder(..)
  , binderNames
  , binderNamesWithSpans
  , isIrrefutable
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.Names (ConstructorName, Ident, ValueOpName, OpName, ProperName, Qualified)
import Language.PureScript.Types (SourceType)

data Binder
  = NullBinder
  | LiteralBinder SourceSpan (Literal Binder)
  | VarBinder SourceSpan Ident
  | ConstructorBinder SourceSpan (Qualified (ProperName ConstructorName)) (Array Binder)
  | OpBinder SourceSpan (Qualified (OpName ValueOpName))
  | BinaryNoParensBinder Binder Binder Binder
  | ParensInBinder Binder
  | NamedBinder SourceSpan Ident Binder
  | PositionedBinder SourceSpan (Array Comment) Binder
  | TypedBinder SourceType Binder

-- Custom Eq: skip SourceSpan in comparison (for performance)
instance eqBinder :: Eq Binder where
  eq NullBinder NullBinder = true
  eq (LiteralBinder _ lb) (LiteralBinder _ lb') = lb == lb'
  eq (VarBinder _ i) (VarBinder _ i') = i == i'
  eq (ConstructorBinder _ q bs) (ConstructorBinder _ q' bs') = q == q' && bs == bs'
  eq (OpBinder _ q) (OpBinder _ q') = q == q'
  eq (BinaryNoParensBinder b1 b2 b3) (BinaryNoParensBinder b1' b2' b3') =
    b1 == b1' && b2 == b2' && b3 == b3'
  eq (ParensInBinder b) (ParensInBinder b') = b == b'
  eq (NamedBinder _ i b) (NamedBinder _ i' b') = i == i' && b == b'
  eq (PositionedBinder _ cs b) (PositionedBinder _ cs' b') = cs == cs' && b == b'
  eq (TypedBinder ty b) (TypedBinder ty' b') = ty == ty' && b == b'
  eq _ _ = false

instance ordBinder :: Ord Binder where
  compare NullBinder NullBinder = EQ
  compare (LiteralBinder _ lb) (LiteralBinder _ lb') = compare lb lb'
  compare (VarBinder _ i) (VarBinder _ i') = compare i i'
  compare (ConstructorBinder _ q bs) (ConstructorBinder _ q' bs') =
    compare q q' <> compare bs bs'
  compare (OpBinder _ q) (OpBinder _ q') = compare q q'
  compare (BinaryNoParensBinder b1 b2 b3) (BinaryNoParensBinder b1' b2' b3') =
    compare b1 b1' <> compare b2 b2' <> compare b3 b3'
  compare (ParensInBinder b) (ParensInBinder b') = compare b b'
  compare (NamedBinder _ i b) (NamedBinder _ i' b') = compare i i' <> compare b b'
  compare (PositionedBinder _ cs b) (PositionedBinder _ cs' b') =
    compare cs cs' <> compare b b'
  compare (TypedBinder ty b) (TypedBinder ty' b') = compare ty ty' <> compare b b'
  compare x y = compare (orderOf x) (orderOf y)
    where
    orderOf NullBinder              = 0
    orderOf (LiteralBinder _ _)     = 1
    orderOf (VarBinder _ _)         = 2
    orderOf (ConstructorBinder _ _ _) = 3
    orderOf (OpBinder _ _)          = 4
    orderOf (BinaryNoParensBinder _ _ _) = 5
    orderOf (ParensInBinder _)      = 6
    orderOf (NamedBinder _ _ _)     = 7
    orderOf (PositionedBinder _ _ _) = 8
    orderOf (TypedBinder _ _)       = 9

instance showBinder :: Show Binder where
  show NullBinder = "NullBinder"
  show (LiteralBinder _ lb) = "(LiteralBinder " <> show lb <> ")"
  show (VarBinder _ i) = "(VarBinder " <> show i <> ")"
  show (ConstructorBinder _ q bs) = "(ConstructorBinder " <> show q <> " " <> show bs <> ")"
  show (OpBinder _ q) = "(OpBinder " <> show q <> ")"
  show (BinaryNoParensBinder b1 b2 b3) = "(BinaryNoParensBinder " <> show b1 <> " " <> show b2 <> " " <> show b3 <> ")"
  show (ParensInBinder b) = "(ParensInBinder " <> show b <> ")"
  show (NamedBinder _ i b) = "(NamedBinder " <> show i <> " " <> show b <> ")"
  show (PositionedBinder _ _ b) = "(PositionedBinder " <> show b <> ")"
  show (TypedBinder ty b) = "(TypedBinder " <> show ty <> " " <> show b <> ")"

binderNamesWithSpans :: Binder -> Array (Tuple SourceSpan Ident)
binderNamesWithSpans = go []
  where
  go ns (LiteralBinder _ lb) = litGo ns lb
  go ns (VarBinder ss name) = ns <> [ Tuple ss name ]
  go ns (ConstructorBinder _ _ bs) = foldl go ns bs
  go ns (BinaryNoParensBinder b1 b2 b3) = foldl go ns [ b1, b2, b3 ]
  go ns (ParensInBinder b) = go ns b
  go ns (NamedBinder ss name b) = go (ns <> [ Tuple ss name ]) b
  go ns (PositionedBinder _ _ b) = go ns b
  go ns (TypedBinder _ b) = go ns b
  go ns _ = ns
  litGo ns (ObjectLiteral bs) = foldl go ns (map snd bs)
  litGo ns (ArrayLiteral bs) = foldl go ns bs
  litGo ns _ = ns
  snd (Tuple _ b) = b

binderNames :: Binder -> Array Ident
binderNames = map snd' <<< binderNamesWithSpans
  where snd' (Tuple _ b) = b

isIrrefutable :: Binder -> Boolean
isIrrefutable NullBinder = true
isIrrefutable (VarBinder _ _) = true
isIrrefutable (PositionedBinder _ _ b) = isIrrefutable b
isIrrefutable (TypedBinder _ b) = isIrrefutable b
isIrrefutable _ = false
