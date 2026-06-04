-- | Data types for the imperative core AST.
module Language.PureScript.CoreImp.AST
  ( UnaryOperator(..)
  , BinaryOperator(..)
  , CIComments(..)
  , InitializerEffects(..)
  , AST(..)
  , withSourceSpan
  , getSourceSpan
  , everywhere
  , everywhereTopDown
  , everywhereTopDownM
  , everything
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.Names (ModuleName)
import Language.PureScript.PSString (PSString)

data UnaryOperator
  = Negate
  | Not
  | BitwiseNot
  | Positive
  | New

derive instance eqUnaryOperator :: Eq UnaryOperator
instance showUnaryOperator :: Show UnaryOperator where
  show Negate = "Negate"
  show Not = "Not"
  show BitwiseNot = "BitwiseNot"
  show Positive = "Positive"
  show New = "New"

data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulus
  | EqualTo
  | NotEqualTo
  | LessThan
  | LessThanOrEqualTo
  | GreaterThan
  | GreaterThanOrEqualTo
  | And
  | Or
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  | ZeroFillShiftRight

derive instance eqBinaryOperator :: Eq BinaryOperator
instance showBinaryOperator :: Show BinaryOperator where
  show Add = "Add"
  show Subtract = "Subtract"
  show Multiply = "Multiply"
  show Divide = "Divide"
  show Modulus = "Modulus"
  show EqualTo = "EqualTo"
  show NotEqualTo = "NotEqualTo"
  show LessThan = "LessThan"
  show LessThanOrEqualTo = "LessThanOrEqualTo"
  show GreaterThan = "GreaterThan"
  show GreaterThanOrEqualTo = "GreaterThanOrEqualTo"
  show And = "And"
  show Or = "Or"
  show BitwiseAnd = "BitwiseAnd"
  show BitwiseOr = "BitwiseOr"
  show BitwiseXor = "BitwiseXor"
  show ShiftLeft = "ShiftLeft"
  show ShiftRight = "ShiftRight"
  show ZeroFillShiftRight = "ZeroFillShiftRight"

data CIComments
  = SourceComments (Array Comment)
  | PureAnnotation

derive instance eqCIComments :: Eq CIComments
instance showCIComments :: Show CIComments where
  show (SourceComments _) = "SourceComments"
  show PureAnnotation = "PureAnnotation"

data InitializerEffects = NoEffects | UnknownEffects

derive instance eqInitializerEffects :: Eq InitializerEffects
instance showInitializerEffects :: Show InitializerEffects where
  show NoEffects = "NoEffects"
  show UnknownEffects = "UnknownEffects"

data AST
  = NumericLiteral (Maybe SourceSpan) (Either Int Number)
  | StringLiteral (Maybe SourceSpan) PSString
  | BooleanLiteral (Maybe SourceSpan) Boolean
  | Unary (Maybe SourceSpan) UnaryOperator AST
  | Binary (Maybe SourceSpan) BinaryOperator AST AST
  | ArrayLiteral (Maybe SourceSpan) (Array AST)
  | Indexer (Maybe SourceSpan) AST AST
  | ObjectLiteral (Maybe SourceSpan) (Array (Tuple PSString AST))
  | Function (Maybe SourceSpan) (Maybe String) (Array String) AST
  | App (Maybe SourceSpan) AST (Array AST)
  | Var (Maybe SourceSpan) String
  | ModuleAccessor (Maybe SourceSpan) ModuleName PSString
  | Block (Maybe SourceSpan) (Array AST)
  | VariableIntroduction (Maybe SourceSpan) String (Maybe (Tuple InitializerEffects AST))
  | Assignment (Maybe SourceSpan) AST AST
  | While (Maybe SourceSpan) AST AST
  | For (Maybe SourceSpan) String AST AST AST
  | ForIn (Maybe SourceSpan) String AST AST
  | IfElse (Maybe SourceSpan) AST AST (Maybe AST)
  | Return (Maybe SourceSpan) AST
  | ReturnNoResult (Maybe SourceSpan)
  | Throw (Maybe SourceSpan) AST
  | InstanceOf (Maybe SourceSpan) AST AST
  | Comment CIComments AST

derive instance eqAST :: Eq AST
instance showAST :: Show AST where
  show (NumericLiteral _ _) = "NumericLiteral"
  show (StringLiteral _ _) = "StringLiteral"
  show (BooleanLiteral _ _) = "BooleanLiteral"
  show (Var _ s) = "(Var " <> s <> ")"
  show _ = "AST"

withSourceSpan :: SourceSpan -> AST -> AST
withSourceSpan withSpan = go
  where
  ss :: Maybe SourceSpan
  ss = Just withSpan

  go (NumericLiteral _ n) = NumericLiteral ss n
  go (StringLiteral _ s) = StringLiteral ss s
  go (BooleanLiteral _ b) = BooleanLiteral ss b
  go (Unary _ op j) = Unary ss op j
  go (Binary _ op j1 j2) = Binary ss op j1 j2
  go (ArrayLiteral _ js) = ArrayLiteral ss js
  go (Indexer _ j1 j2) = Indexer ss j1 j2
  go (ObjectLiteral _ js) = ObjectLiteral ss js
  go (Function _ name args j) = Function ss name args j
  go (App _ j js) = App ss j js
  go (Var _ s) = Var ss s
  go (ModuleAccessor _ s1 s2) = ModuleAccessor ss s1 s2
  go (Block _ js) = Block ss js
  go (VariableIntroduction _ name j) = VariableIntroduction ss name j
  go (Assignment _ j1 j2) = Assignment ss j1 j2
  go (While _ j1 j2) = While ss j1 j2
  go (For _ name j1 j2 j3) = For ss name j1 j2 j3
  go (ForIn _ name j1 j2) = ForIn ss name j1 j2
  go (IfElse _ j1 j2 j3) = IfElse ss j1 j2 j3
  go (Return _ js) = Return ss js
  go (ReturnNoResult _) = ReturnNoResult ss
  go (Throw _ js) = Throw ss js
  go (InstanceOf _ j1 j2) = InstanceOf ss j1 j2
  go c@(Comment _ _) = c

getSourceSpan :: AST -> Maybe SourceSpan
getSourceSpan (NumericLiteral ss _) = ss
getSourceSpan (StringLiteral ss _) = ss
getSourceSpan (BooleanLiteral ss _) = ss
getSourceSpan (Unary ss _ _) = ss
getSourceSpan (Binary ss _ _ _) = ss
getSourceSpan (ArrayLiteral ss _) = ss
getSourceSpan (Indexer ss _ _) = ss
getSourceSpan (ObjectLiteral ss _) = ss
getSourceSpan (Function ss _ _ _) = ss
getSourceSpan (App ss _ _) = ss
getSourceSpan (Var ss _) = ss
getSourceSpan (ModuleAccessor ss _ _) = ss
getSourceSpan (Block ss _) = ss
getSourceSpan (VariableIntroduction ss _ _) = ss
getSourceSpan (Assignment ss _ _) = ss
getSourceSpan (While ss _ _) = ss
getSourceSpan (For ss _ _ _ _) = ss
getSourceSpan (ForIn ss _ _ _) = ss
getSourceSpan (IfElse ss _ _ _) = ss
getSourceSpan (Return ss _) = ss
getSourceSpan (ReturnNoResult ss) = ss
getSourceSpan (Throw ss _) = ss
getSourceSpan (InstanceOf ss _ _) = ss
getSourceSpan (Comment _ _) = Nothing

everywhere :: (AST -> AST) -> AST -> AST
everywhere f = go
  where
  go (Unary ss op j) = f (Unary ss op (go j))
  go (Binary ss op j1 j2) = f (Binary ss op (go j1) (go j2))
  go (ArrayLiteral ss js) = f (ArrayLiteral ss (map go js))
  go (Indexer ss j1 j2) = f (Indexer ss (go j1) (go j2))
  go (ObjectLiteral ss js) = f (ObjectLiteral ss (map (\(Tuple k v) -> Tuple k (go v)) js))
  go (Function ss name args j) = f (Function ss name args (go j))
  go (App ss j js) = f (App ss (go j) (map go js))
  go (Block ss js) = f (Block ss (map go js))
  go (VariableIntroduction ss name j) = f (VariableIntroduction ss name (map (\(Tuple ie a) -> Tuple ie (go a)) j))
  go (Assignment ss j1 j2) = f (Assignment ss (go j1) (go j2))
  go (While ss j1 j2) = f (While ss (go j1) (go j2))
  go (For ss name j1 j2 j3) = f (For ss name (go j1) (go j2) (go j3))
  go (ForIn ss name j1 j2) = f (ForIn ss name (go j1) (go j2))
  go (IfElse ss j1 j2 j3) = f (IfElse ss (go j1) (go j2) (map go j3))
  go (Return ss js) = f (Return ss (go js))
  go (Throw ss js) = f (Throw ss (go js))
  go (InstanceOf ss j1 j2) = f (InstanceOf ss (go j1) (go j2))
  go (Comment com j) = f (Comment com (go j))
  go other = f other

everywhereTopDown :: (AST -> AST) -> AST -> AST
everywhereTopDown f = go <<< f
  where
  go (Unary ss op j) = Unary ss op (everywhereTopDown f j)
  go (Binary ss op j1 j2) = Binary ss op (everywhereTopDown f j1) (everywhereTopDown f j2)
  go (ArrayLiteral ss js) = ArrayLiteral ss (map (everywhereTopDown f) js)
  go (Indexer ss j1 j2) = Indexer ss (everywhereTopDown f j1) (everywhereTopDown f j2)
  go (ObjectLiteral ss js) = ObjectLiteral ss (map (\(Tuple k v) -> Tuple k (everywhereTopDown f v)) js)
  go (Function ss name args j) = Function ss name args (everywhereTopDown f j)
  go (App ss j js) = App ss (everywhereTopDown f j) (map (everywhereTopDown f) js)
  go (Block ss js) = Block ss (map (everywhereTopDown f) js)
  go (VariableIntroduction ss name j) = VariableIntroduction ss name (map (\(Tuple ie a) -> Tuple ie (everywhereTopDown f a)) j)
  go (Assignment ss j1 j2) = Assignment ss (everywhereTopDown f j1) (everywhereTopDown f j2)
  go (While ss j1 j2) = While ss (everywhereTopDown f j1) (everywhereTopDown f j2)
  go (For ss name j1 j2 j3) = For ss name (everywhereTopDown f j1) (everywhereTopDown f j2) (everywhereTopDown f j3)
  go (ForIn ss name j1 j2) = ForIn ss name (everywhereTopDown f j1) (everywhereTopDown f j2)
  go (IfElse ss j1 j2 j3) = IfElse ss (everywhereTopDown f j1) (everywhereTopDown f j2) (map (everywhereTopDown f) j3)
  go (Return ss j) = Return ss (everywhereTopDown f j)
  go (Throw ss j) = Throw ss (everywhereTopDown f j)
  go (InstanceOf ss j1 j2) = InstanceOf ss (everywhereTopDown f j1) (everywhereTopDown f j2)
  go (Comment com j) = Comment com (everywhereTopDown f j)
  go other = other

everywhereTopDownM :: forall m. Monad m => (AST -> m AST) -> AST -> m AST
everywhereTopDownM f = f >=> go
  where
  f' = everywhereTopDownM f
  go (Unary ss op j) = Unary ss op <$> f' j
  go (Binary ss op j1 j2) = Binary ss op <$> f' j1 <*> f' j2
  go (ArrayLiteral ss js) = ArrayLiteral ss <$> traverse f' js
  go (Indexer ss j1 j2) = Indexer ss <$> f' j1 <*> f' j2
  go (ObjectLiteral ss js) = ObjectLiteral ss <$> traverse (\(Tuple k v) -> Tuple k <$> f' v) js
  go (Function ss name args j) = Function ss name args <$> f' j
  go (App ss j js) = App ss <$> f' j <*> traverse f' js
  go (Block ss js) = Block ss <$> traverse f' js
  go (VariableIntroduction ss name j) = VariableIntroduction ss name <$> traverse (\(Tuple ie a) -> Tuple ie <$> f' a) j
  go (Assignment ss j1 j2) = Assignment ss <$> f' j1 <*> f' j2
  go (While ss j1 j2) = While ss <$> f' j1 <*> f' j2
  go (For ss name j1 j2 j3) = For ss name <$> f' j1 <*> f' j2 <*> f' j3
  go (ForIn ss name j1 j2) = ForIn ss name <$> f' j1 <*> f' j2
  go (IfElse ss j1 j2 j3) = IfElse ss <$> f' j1 <*> f' j2 <*> traverse f' j3
  go (Return ss j) = Return ss <$> f' j
  go (Throw ss j) = Throw ss <$> f' j
  go (InstanceOf ss j1 j2) = InstanceOf ss <$> f' j1 <*> f' j2
  go (Comment com j) = Comment com <$> f' j
  go other = f other

everything :: forall r. (r -> r -> r) -> (AST -> r) -> AST -> r
everything merge f = go
  where
  go j@(Unary _ _ j1) = f j `merge` go j1
  go j@(Binary _ _ j1 j2) = f j `merge` go j1 `merge` go j2
  go j@(ArrayLiteral _ js) = foldl merge (f j) (map go js)
  go j@(Indexer _ j1 j2) = f j `merge` go j1 `merge` go j2
  go j@(ObjectLiteral _ js) = foldl merge (f j) (map (\(Tuple _ v) -> go v) js)
  go j@(Function _ _ _ j1) = f j `merge` go j1
  go j@(App _ j1 js) = foldl merge (f j `merge` go j1) (map go js)
  go j@(Block _ js) = foldl merge (f j) (map go js)
  go j@(VariableIntroduction _ _ (Just (Tuple _ j1))) = f j `merge` go j1
  go j@(Assignment _ j1 j2) = f j `merge` go j1 `merge` go j2
  go j@(While _ j1 j2) = f j `merge` go j1 `merge` go j2
  go j@(For _ _ j1 j2 j3) = f j `merge` go j1 `merge` go j2 `merge` go j3
  go j@(ForIn _ _ j1 j2) = f j `merge` go j1 `merge` go j2
  go j@(IfElse _ j1 j2 Nothing) = f j `merge` go j1 `merge` go j2
  go j@(IfElse _ j1 j2 (Just j3)) = f j `merge` go j1 `merge` go j2 `merge` go j3
  go j@(Return _ j1) = f j `merge` go j1
  go j@(Throw _ j1) = f j `merge` go j1
  go j@(InstanceOf _ j1 j2) = f j `merge` go j1 `merge` go j2
  go j@(Comment _ j1) = f j `merge` go j1
  go other = f other
