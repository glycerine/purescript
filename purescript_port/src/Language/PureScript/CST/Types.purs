-- | CST data types for the PureScript surface language.
-- | Every token is represented, and every token is annotated with
-- | whitespace and comments. This allows an exact printer such that
-- | `print . parse = id`. Every constructor is laid out with tokens
-- | in left-to-right order.
module Language.PureScript.CST.Types
  ( CSTSourcePos(..)
  , SourceRange(..)
  , Comment(..)
  , LineFeed(..)
  , TokenAnn(..)
  , SourceStyle(..)
  , Token(..)
  , SourceToken(..)
  , Ident(..)
  , Name(..)
  , QualifiedName(..)
  , Label(..)
  , Wrapped(..)
  , Separated(..)
  , Labeled(..)
  , Delimited
  , DelimitedNonEmpty
  , OneOrDelimited(..)
  , Type(..)
  , TypeVarBinding(..)
  , Constraint(..)
  , Row(..)
  , Module(..)
  , Export(..)
  , DataMembers(..)
  , Declaration(..)
  , Instance(..)
  , InstanceBinding(..)
  , ImportDecl(..)
  , Import(..)
  , DataHead(..)
  , DataCtor(..)
  , ClassHead(..)
  , ClassFundep(..)
  , InstanceHead(..)
  , Fixity(..)
  , FixityOp(..)
  , FixityFields(..)
  , ValueBindingFields(..)
  , Guarded(..)
  , GuardedExpr(..)
  , PatternGuard(..)
  , Foreign(..)
  , Role(..)
  , Expr(..)
  , RecordLabeled(..)
  , RecordUpdate(..)
  , RecordAccessor(..)
  , Lambda(..)
  , IfThenElse(..)
  , CaseOf(..)
  , LetIn(..)
  , Where(..)
  , LetBinding(..)
  , DoBlock(..)
  , DoStatement(..)
  , AdoBlock(..)
  , Binder(..)
  ) where

import Prelude

import Data.Either (Either)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Tuple (Tuple(..))
import Data.Void (Void)

import Language.PureScript.Names (ClassName, ConstructorName, ModuleName, OpName, ProperName, TypeName, ValueOpName, TypeOpName)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Roles (Role) as R

-- -----------------------------------------------------------------------
-- Source positions and ranges (CST-specific, with srcLine/srcColumn fields)
-- -----------------------------------------------------------------------

newtype CSTSourcePos = CSTSourcePos
  { srcLine   :: Int
  , srcColumn :: Int
  }

derive instance eqCSTSourcePos  :: Eq CSTSourcePos
derive instance ordCSTSourcePos :: Ord CSTSourcePos

instance showCSTSourcePos :: Show CSTSourcePos where
  show (CSTSourcePos { srcLine, srcColumn }) =
    "(CSTSourcePos " <> show srcLine <> ":" <> show srcColumn <> ")"

data SourceRange = SourceRange
  { srcStart :: CSTSourcePos
  , srcEnd   :: CSTSourcePos
  }

derive instance eqSourceRange  :: Eq SourceRange
derive instance ordSourceRange :: Ord SourceRange

instance showSourceRange :: Show SourceRange where
  show _ = "<SourceRange>"

-- -----------------------------------------------------------------------
-- Comments and whitespace
-- -----------------------------------------------------------------------

data LineFeed = LF | CRLF

derive instance eqLineFeed  :: Eq LineFeed
derive instance ordLineFeed :: Ord LineFeed

instance showLineFeed :: Show LineFeed where
  show LF   = "LF"
  show CRLF = "CRLF"

data Comment l
  = Comment String
  | Space Int
  | Line l

derive instance functorComment :: Functor Comment
derive instance eqComment  :: Eq l  => Eq (Comment l)
derive instance ordComment :: Ord l => Ord (Comment l)

instance showComment :: Show l => Show (Comment l) where
  show (Comment s) = "(Comment " <> show s <> ")"
  show (Space n)   = "(Space " <> show n <> ")"
  show (Line l)    = "(Line " <> show l <> ")"

-- -----------------------------------------------------------------------
-- Token annotations
-- -----------------------------------------------------------------------

data TokenAnn = TokenAnn
  { tokRange            :: SourceRange
  , tokLeadingComments  :: Array (Comment LineFeed)
  , tokTrailingComments :: Array (Comment Void)
  }

derive instance eqTokenAnn  :: Eq TokenAnn
derive instance ordTokenAnn :: Ord TokenAnn

instance showTokenAnn :: Show TokenAnn where
  show _ = "<TokenAnn>"

-- -----------------------------------------------------------------------
-- Source style (ASCII vs Unicode operators)
-- -----------------------------------------------------------------------

data SourceStyle = ASCII | Unicode

derive instance eqSourceStyle  :: Eq SourceStyle
derive instance ordSourceStyle :: Ord SourceStyle

instance showSourceStyle :: Show SourceStyle where
  show ASCII   = "ASCII"
  show Unicode = "Unicode"

-- -----------------------------------------------------------------------
-- Tokens
-- -----------------------------------------------------------------------

data Token
  = TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  | TokLeftArrow SourceStyle
  | TokRightArrow SourceStyle
  | TokRightFatArrow SourceStyle
  | TokDoubleColon SourceStyle
  | TokForall SourceStyle
  | TokEquals
  | TokPipe
  | TokTick
  | TokDot
  | TokComma
  | TokUnderscore
  | TokBackslash
  | TokLowerName (Array String) String
  | TokUpperName (Array String) String
  | TokOperator (Array String) String
  | TokSymbolName (Array String) String
  | TokSymbolArr SourceStyle
  | TokHole String
  | TokChar String Char
  | TokString String PSString
  | TokRawString String
  | TokInt String Int
  | TokNumber String Number
  | TokLayoutStart
  | TokLayoutSep
  | TokLayoutEnd
  | TokEof

derive instance eqToken  :: Eq Token
derive instance ordToken :: Ord Token

instance showToken :: Show Token where
  show TokLeftParen         = "TokLeftParen"
  show TokRightParen        = "TokRightParen"
  show TokLeftBrace         = "TokLeftBrace"
  show TokRightBrace        = "TokRightBrace"
  show TokLeftSquare        = "TokLeftSquare"
  show TokRightSquare       = "TokRightSquare"
  show (TokLeftArrow s)     = "(TokLeftArrow " <> show s <> ")"
  show (TokRightArrow s)    = "(TokRightArrow " <> show s <> ")"
  show (TokRightFatArrow s) = "(TokRightFatArrow " <> show s <> ")"
  show (TokDoubleColon s)   = "(TokDoubleColon " <> show s <> ")"
  show (TokForall s)        = "(TokForall " <> show s <> ")"
  show TokEquals            = "TokEquals"
  show TokPipe              = "TokPipe"
  show TokTick              = "TokTick"
  show TokDot               = "TokDot"
  show TokComma             = "TokComma"
  show TokUnderscore        = "TokUnderscore"
  show TokBackslash         = "TokBackslash"
  show (TokLowerName qs n)  = "(TokLowerName " <> show qs <> " " <> show n <> ")"
  show (TokUpperName qs n)  = "(TokUpperName " <> show qs <> " " <> show n <> ")"
  show (TokOperator qs n)   = "(TokOperator " <> show qs <> " " <> show n <> ")"
  show (TokSymbolName qs n) = "(TokSymbolName " <> show qs <> " " <> show n <> ")"
  show (TokSymbolArr s)     = "(TokSymbolArr " <> show s <> ")"
  show (TokHole n)          = "(TokHole " <> show n <> ")"
  show (TokChar t _)        = "(TokChar " <> show t <> ")"
  show (TokString t _)      = "(TokString " <> show t <> ")"
  show (TokRawString t)     = "(TokRawString " <> show t <> ")"
  show (TokInt t _)         = "(TokInt " <> show t <> ")"
  show (TokNumber t _)      = "(TokNumber " <> show t <> ")"
  show TokLayoutStart       = "TokLayoutStart"
  show TokLayoutSep         = "TokLayoutSep"
  show TokLayoutEnd         = "TokLayoutEnd"
  show TokEof               = "TokEof"

data SourceToken = SourceToken
  { tokAnn   :: TokenAnn
  , tokValue :: Token
  }

derive instance eqSourceToken  :: Eq SourceToken
derive instance ordSourceToken :: Ord SourceToken

instance showSourceToken :: Show SourceToken where
  show (SourceToken { tokValue }) = "(SourceToken " <> show tokValue <> ")"

-- -----------------------------------------------------------------------
-- CST identifier (distinct from the AST Ident in Names.purs)
-- -----------------------------------------------------------------------

newtype Ident = Ident String

getIdent :: Ident -> String
getIdent (Ident s) = s

derive instance eqIdent  :: Eq Ident
derive instance ordIdent :: Ord Ident

instance showIdent :: Show Ident where
  show (Ident s) = "(Ident " <> show s <> ")"

-- -----------------------------------------------------------------------
-- Named and qualified nodes
-- -----------------------------------------------------------------------

data Name a = Name
  { nameTok   :: SourceToken
  , nameValue :: a
  }

derive instance functorName  :: Functor Name
derive instance eqName  :: Eq a  => Eq (Name a)
derive instance ordName :: Ord a => Ord (Name a)

instance showName :: Show a => Show (Name a) where
  show (Name { nameValue }) = "(Name " <> show nameValue <> ")"

instance foldableName :: Foldable Name where
  foldr f z (Name { nameValue }) = f nameValue z
  foldl f z (Name { nameValue }) = f z nameValue
  foldMap f (Name { nameValue }) = f nameValue

instance traversableName :: Traversable Name where
  traverse f (Name n) = (\v -> Name n { nameValue = v }) <$> f n.nameValue
  sequence (Name n) = (\v -> Name n { nameValue = v }) <$> n.nameValue

data QualifiedName a = QualifiedName
  { qualTok    :: SourceToken
  , qualModule :: Maybe ModuleName
  , qualName   :: a
  }

derive instance functorQualifiedName  :: Functor QualifiedName
derive instance eqQualifiedName  :: Eq a  => Eq (QualifiedName a)
derive instance ordQualifiedName :: Ord a => Ord (QualifiedName a)

instance showQualifiedName :: Show a => Show (QualifiedName a) where
  show (QualifiedName { qualModule, qualName }) =
    "(QualifiedName " <> show qualModule <> " " <> show qualName <> ")"

instance foldableQualifiedName :: Foldable QualifiedName where
  foldr f z (QualifiedName { qualName }) = f qualName z
  foldl f z (QualifiedName { qualName }) = f z qualName
  foldMap f (QualifiedName { qualName }) = f qualName

instance traversableQualifiedName :: Traversable QualifiedName where
  traverse f (QualifiedName q) = (\v -> QualifiedName q { qualName = v }) <$> f q.qualName
  sequence (QualifiedName q)   = (\v -> QualifiedName q { qualName = v }) <$> q.qualName

-- | CST label (with its source token), distinct from Language.PureScript.Label
data Label = Label
  { lblTok  :: SourceToken
  , lblName :: PSString
  }

derive instance eqLabel  :: Eq Label
derive instance ordLabel :: Ord Label

instance showLabel :: Show Label where
  show (Label { lblName }) = "(Label " <> show lblName <> ")"

-- -----------------------------------------------------------------------
-- Container types
-- -----------------------------------------------------------------------

data Wrapped a = Wrapped
  { wrpOpen  :: SourceToken
  , wrpValue :: a
  , wrpClose :: SourceToken
  }

derive instance functorWrapped  :: Functor Wrapped
derive instance eqWrapped  :: Eq a  => Eq (Wrapped a)
derive instance ordWrapped :: Ord a => Ord (Wrapped a)

instance showWrapped :: Show a => Show (Wrapped a) where
  show (Wrapped { wrpValue }) = "(Wrapped " <> show wrpValue <> ")"

instance foldableWrapped :: Foldable Wrapped where
  foldr f z (Wrapped { wrpValue }) = f wrpValue z
  foldl f z (Wrapped { wrpValue }) = f z wrpValue
  foldMap f (Wrapped { wrpValue }) = f wrpValue

instance traversableWrapped :: Traversable Wrapped where
  traverse f (Wrapped w) = (\v -> Wrapped w { wrpValue = v }) <$> f w.wrpValue
  sequence (Wrapped w)   = (\v -> Wrapped w { wrpValue = v }) <$> w.wrpValue

data Separated a = Separated
  { sepHead :: a
  , sepTail :: Array (Tuple SourceToken a)
  }

derive instance functorSeparated  :: Functor Separated
derive instance eqSeparated  :: Eq a  => Eq (Separated a)
derive instance ordSeparated :: Ord a => Ord (Separated a)

instance showSeparated :: Show a => Show (Separated a) where
  show (Separated { sepHead }) = "(Separated " <> show sepHead <> " ...)"

instance foldableSeparated :: Foldable Separated where
  foldr f z (Separated { sepHead, sepTail }) =
    f sepHead (foldr (\(Tuple _ a) acc -> f a acc) z sepTail)
  foldl f z (Separated { sepHead, sepTail }) =
    foldl (\acc (Tuple _ a) -> f acc a) (f z sepHead) sepTail
  foldMap f (Separated { sepHead, sepTail }) =
    f sepHead <> foldMap (\(Tuple _ a) -> f a) sepTail

instance traversableSeparated :: Traversable Separated where
  traverse f (Separated { sepHead, sepTail }) =
    (\h t -> Separated { sepHead: h, sepTail: t })
      <$> f sepHead
      <*> traverse (\(Tuple tok a) -> Tuple tok <$> f a) sepTail
  sequence (Separated { sepHead, sepTail }) =
    (\h t -> Separated { sepHead: h, sepTail: t })
      <$> sepHead
      <*> traverse (\(Tuple tok a) -> Tuple tok <$> a) sepTail

data Labeled a b = Labeled
  { lblLabel :: a
  , lblSep   :: SourceToken
  , lblValue :: b
  }

derive instance functorLabeled  :: Functor (Labeled a)
derive instance eqLabeled  :: (Eq a,  Eq b)  => Eq (Labeled a b)
derive instance ordLabeled :: (Ord a, Ord b) => Ord (Labeled a b)

instance showLabeled :: (Show a, Show b) => Show (Labeled a b) where
  show (Labeled { lblLabel, lblValue }) =
    "(Labeled " <> show lblLabel <> " " <> show lblValue <> ")"

instance foldableLabeled :: Foldable (Labeled a) where
  foldr f z (Labeled { lblValue }) = f lblValue z
  foldl f z (Labeled { lblValue }) = f z lblValue
  foldMap f (Labeled { lblValue }) = f lblValue

instance traversableLabeled :: Traversable (Labeled a) where
  traverse f (Labeled l) = (\v -> Labeled l { lblValue = v }) <$> f l.lblValue
  sequence (Labeled l)   = (\v -> Labeled l { lblValue = v }) <$> l.lblValue

-- | A value wrapped in delimiters, possibly empty
type Delimited a = Wrapped (Maybe (Separated a))

-- | A value wrapped in delimiters, guaranteed non-empty
type DelimitedNonEmpty a = Wrapped (Separated a)

data OneOrDelimited a
  = One a
  | Many (DelimitedNonEmpty a)

derive instance functorOneOrDelimited  :: Functor OneOrDelimited
derive instance eqOneOrDelimited  :: Eq a  => Eq (OneOrDelimited a)
derive instance ordOneOrDelimited :: Ord a => Ord (OneOrDelimited a)

instance showOneOrDelimited :: Show a => Show (OneOrDelimited a) where
  show (One a)   = "(One " <> show a <> ")"
  show (Many ws) = "(Many " <> show ws <> ")"

instance foldableOneOrDelimited :: Foldable OneOrDelimited where
  foldr f z (One a)   = f a z
  foldr f z (Many ws) = foldr (flip (foldr f)) z ws
  foldl f z (One a)   = f z a
  foldl f z (Many ws) = foldl (foldl f) z ws
  foldMap f (One a)   = f a
  foldMap f (Many ws) = foldMap (foldMap f) ws

instance traversableOneOrDelimited :: Traversable OneOrDelimited where
  traverse f (One a)   = One <$> f a
  traverse f (Many ws) = Many <$> traverse (traverse f) ws
  sequence = traverse identity

-- -----------------------------------------------------------------------
-- Types
-- -----------------------------------------------------------------------

data Type a
  = TypeVar a (Name Ident)
  | TypeConstructor a (QualifiedName (ProperName TypeName))
  | TypeWildcard a SourceToken
  | TypeHole a (Name Ident)
  | TypeString a SourceToken PSString
  | TypeInt a (Maybe SourceToken) SourceToken Int
  | TypeRow a (Wrapped (Row a))
  | TypeRecord a (Wrapped (Row a))
  | TypeForall a SourceToken (NonEmptyList (TypeVarBinding a)) SourceToken (Type a)
  | TypeKinded a (Type a) SourceToken (Type a)
  | TypeApp a (Type a) (Type a)
  | TypeOp a (Type a) (QualifiedName (OpName TypeOpName)) (Type a)
  | TypeOpName a (QualifiedName (OpName TypeOpName))
  | TypeArr a (Type a) SourceToken (Type a)
  | TypeArrName a SourceToken
  | TypeConstrained a (Constraint a) SourceToken (Type a)
  | TypeParens a (Wrapped (Type a))
  | TypeUnaryRow a SourceToken (Type a)

derive instance functorType :: Functor Type
derive instance eqType :: Eq a => Eq (Type a)
derive instance ordType :: Ord a => Ord (Type a)

instance showType :: Show a => Show (Type a) where
  show _ = "<CSTType>"

instance foldableType :: Foldable Type where
  foldr f z (TypeVar a _)              = f a z
  foldr f z (TypeConstructor a _)      = f a z
  foldr f z (TypeWildcard a _)         = f a z
  foldr f z (TypeHole a _)             = f a z
  foldr f z (TypeString a _ _)         = f a z
  foldr f z (TypeInt a _ _ _)          = f a z
  foldr f z (TypeRow a (Wrapped w))    = f a (foldr f z w.wrpValue)
  foldr f z (TypeRecord a (Wrapped w)) = f a (foldr f z w.wrpValue)
  foldr f z (TypeForall a _ tvbs _ ty) = f a (foldr (flip (foldr f)) (foldr f z ty) tvbs)
  foldr f z (TypeKinded a t1 _ t2)     = f a (foldr f (foldr f z t2) t1)
  foldr f z (TypeApp a t1 t2)          = f a (foldr f (foldr f z t2) t1)
  foldr f z (TypeOp a t1 _ t2)         = f a (foldr f (foldr f z t2) t1)
  foldr f z (TypeOpName a _)           = f a z
  foldr f z (TypeArr a t1 _ t2)        = f a (foldr f (foldr f z t2) t1)
  foldr f z (TypeArrName a _)          = f a z
  foldr f z (TypeConstrained a c _ t)  = f a (foldr f (foldr f z t) c)
  foldr f z (TypeParens a (Wrapped w)) = f a (foldr f z w.wrpValue)
  foldr f z (TypeUnaryRow a _ t)       = f a (foldr f z t)
  foldl f z (TypeVar a _)              = f z a
  foldl f z (TypeConstructor a _)      = f z a
  foldl f z (TypeWildcard a _)         = f z a
  foldl f z (TypeHole a _)             = f z a
  foldl f z (TypeString a _ _)         = f z a
  foldl f z (TypeInt a _ _ _)          = f z a
  foldl f z (TypeRow a (Wrapped w))    = foldl f (f z a) w.wrpValue
  foldl f z (TypeRecord a (Wrapped w)) = foldl f (f z a) w.wrpValue
  foldl f z (TypeForall a _ tvbs _ ty) = foldl f (foldl (foldl f) (f z a) tvbs) ty
  foldl f z (TypeKinded a t1 _ t2)     = foldl f (foldl f (f z a) t1) t2
  foldl f z (TypeApp a t1 t2)          = foldl f (foldl f (f z a) t1) t2
  foldl f z (TypeOp a t1 _ t2)         = foldl f (foldl f (f z a) t1) t2
  foldl f z (TypeOpName a _)           = f z a
  foldl f z (TypeArr a t1 _ t2)        = foldl f (foldl f (f z a) t1) t2
  foldl f z (TypeArrName a _)          = f z a
  foldl f z (TypeConstrained a c _ t)  = foldl f (foldl f (f z a) c) t
  foldl f z (TypeParens a (Wrapped w)) = foldl f (f z a) w.wrpValue
  foldl f z (TypeUnaryRow a _ t)       = foldl f (f z a) t
  foldMap f (TypeVar a _)              = f a
  foldMap f (TypeConstructor a _)      = f a
  foldMap f (TypeWildcard a _)         = f a
  foldMap f (TypeHole a _)             = f a
  foldMap f (TypeString a _ _)         = f a
  foldMap f (TypeInt a _ _ _)          = f a
  foldMap f (TypeRow a (Wrapped w))    = f a <> foldMap f w.wrpValue
  foldMap f (TypeRecord a (Wrapped w)) = f a <> foldMap f w.wrpValue
  foldMap f (TypeForall a _ tvbs _ ty) = f a <> foldMap (foldMap f) tvbs <> foldMap f ty
  foldMap f (TypeKinded a t1 _ t2)     = f a <> foldMap f t1 <> foldMap f t2
  foldMap f (TypeApp a t1 t2)          = f a <> foldMap f t1 <> foldMap f t2
  foldMap f (TypeOp a t1 _ t2)         = f a <> foldMap f t1 <> foldMap f t2
  foldMap f (TypeOpName a _)           = f a
  foldMap f (TypeArr a t1 _ t2)        = f a <> foldMap f t1 <> foldMap f t2
  foldMap f (TypeArrName a _)          = f a
  foldMap f (TypeConstrained a c _ t)  = f a <> foldMap f c <> foldMap f t
  foldMap f (TypeParens a (Wrapped w)) = f a <> foldMap f w.wrpValue
  foldMap f (TypeUnaryRow a _ t)       = f a <> foldMap f t

instance traversableType :: Traversable Type where
  traverse f (TypeVar a n)              = TypeVar <$> f a <*> pure n
  traverse f (TypeConstructor a q)      = TypeConstructor <$> f a <*> pure q
  traverse f (TypeWildcard a t)         = TypeWildcard <$> f a <*> pure t
  traverse f (TypeHole a n)            = TypeHole <$> f a <*> pure n
  traverse f (TypeString a t ps)        = TypeString <$> f a <*> pure t <*> pure ps
  traverse f (TypeInt a mt t i)         = TypeInt <$> f a <*> pure mt <*> pure t <*> pure i
  traverse f (TypeRow a (Wrapped w))    = (\a' r -> TypeRow a' (Wrapped w { wrpValue = r })) <$> f a <*> traverse f w.wrpValue
  traverse f (TypeRecord a (Wrapped w)) = (\a' r -> TypeRecord a' (Wrapped w { wrpValue = r })) <$> f a <*> traverse f w.wrpValue
  traverse f (TypeForall a t1 tvbs t2 ty) =
    TypeForall <$> f a <*> pure t1 <*> traverse (traverse f) tvbs <*> pure t2 <*> traverse f ty
  traverse f (TypeKinded a t1 tok t2)   = TypeKinded <$> f a <*> traverse f t1 <*> pure tok <*> traverse f t2
  traverse f (TypeApp a t1 t2)          = TypeApp <$> f a <*> traverse f t1 <*> traverse f t2
  traverse f (TypeOp a t1 q t2)         = TypeOp <$> f a <*> traverse f t1 <*> pure q <*> traverse f t2
  traverse f (TypeOpName a q)           = TypeOpName <$> f a <*> pure q
  traverse f (TypeArr a t1 tok t2)      = TypeArr <$> f a <*> traverse f t1 <*> pure tok <*> traverse f t2
  traverse f (TypeArrName a tok)        = TypeArrName <$> f a <*> pure tok
  traverse f (TypeConstrained a c tok ty) = TypeConstrained <$> f a <*> traverse f c <*> pure tok <*> traverse f ty
  traverse f (TypeParens a (Wrapped w)) = (\a' v -> TypeParens a' (Wrapped w { wrpValue = v })) <$> f a <*> traverse f w.wrpValue
  traverse f (TypeUnaryRow a tok ty)    = TypeUnaryRow <$> f a <*> pure tok <*> traverse f ty
  sequence = traverse identity

data TypeVarBinding a
  = TypeVarKinded (Wrapped (Labeled (Tuple (Maybe SourceToken) (Name Ident)) (Type a)))
  | TypeVarName (Tuple (Maybe SourceToken) (Name Ident))

derive instance functorTypeVarBinding  :: Functor TypeVarBinding
derive instance eqTypeVarBinding  :: Eq a  => Eq (TypeVarBinding a)
derive instance ordTypeVarBinding :: Ord a => Ord (TypeVarBinding a)

instance showTypeVarBinding :: Show a => Show (TypeVarBinding a) where
  show _ = "<TypeVarBinding>"

instance foldableTypeVarBinding :: Foldable TypeVarBinding where
  foldr f z (TypeVarKinded (Wrapped w)) = foldr (flip (foldr f)) z w.wrpValue
  foldr _ z (TypeVarName _)             = z
  foldl f z (TypeVarKinded (Wrapped w)) = foldl (foldl f) z w.wrpValue
  foldl f z (TypeVarName _)             = z
  foldMap f (TypeVarKinded (Wrapped w)) = foldMap (foldMap f) w.wrpValue
  foldMap _ (TypeVarName _)             = mempty

instance traversableTypeVarBinding :: Traversable TypeVarBinding where
  traverse f (TypeVarKinded (Wrapped w)) =
    (\v -> TypeVarKinded (Wrapped w { wrpValue = v })) <$> traverse (traverse f) w.wrpValue
  traverse _ (TypeVarName pair) = pure (TypeVarName pair)
  sequence = traverse identity

data Constraint a
  = Constraint a (QualifiedName (ProperName ClassName)) (Array (Type a))
  | ConstraintParens a (Wrapped (Constraint a))

derive instance functorConstraint  :: Functor Constraint
derive instance eqConstraint  :: Eq a  => Eq (Constraint a)
derive instance ordConstraint :: Ord a => Ord (Constraint a)

instance showConstraint :: Show a => Show (Constraint a) where
  show _ = "<CSTConstraint>"

instance foldableConstraint :: Foldable Constraint where
  foldr f z (Constraint a _ tys)         = f a (foldr (flip (foldr f)) z tys)
  foldr f z (ConstraintParens a (Wrapped w)) = f a (foldr f z w.wrpValue)
  foldl f z (Constraint a _ tys)         = foldl (foldl f) (f z a) tys
  foldl f z (ConstraintParens a (Wrapped w)) = foldl f (f z a) w.wrpValue
  foldMap f (Constraint a _ tys)         = f a <> foldMap (foldMap f) tys
  foldMap f (ConstraintParens a (Wrapped w)) = f a <> foldMap f w.wrpValue

instance traversableConstraint :: Traversable Constraint where
  traverse f (Constraint a q tys)          = Constraint <$> f a <*> pure q <*> traverse (traverse f) tys
  traverse f (ConstraintParens a (Wrapped w)) =
    (\a' v -> ConstraintParens a' (Wrapped w { wrpValue = v })) <$> f a <*> traverse f w.wrpValue
  sequence = traverse identity

data Row a = Row
  { rowLabels :: Maybe (Separated (Labeled Label (Type a)))
  , rowTail   :: Maybe (Tuple SourceToken (Type a))
  }

derive instance functorRow  :: Functor Row
derive instance eqRow  :: Eq a  => Eq (Row a)
derive instance ordRow :: Ord a => Ord (Row a)

instance showRow :: Show a => Show (Row a) where
  show _ = "<CSTRow>"

instance foldableRow :: Foldable Row where
  foldr f z (Row { rowLabels, rowTail }) =
    foldr (flip (foldr (flip (foldr (flip (foldr f)))))) (foldr (\(Tuple _ t) acc -> foldr f acc t) z rowTail) rowLabels
  foldl f z (Row { rowLabels, rowTail }) =
    foldl (foldl (foldl (foldl f))) (foldl (\acc (Tuple _ t) -> foldl f acc t) z rowTail) rowLabels
  foldMap f (Row { rowLabels, rowTail }) =
    foldMap (foldMap (foldMap (foldMap f))) rowLabels <>
    foldMap (\(Tuple _ t) -> foldMap f t) rowTail

instance traversableRow :: Traversable Row where
  traverse f (Row { rowLabels, rowTail }) =
    (\ls tl -> Row { rowLabels: ls, rowTail: tl })
      <$> traverse (traverse (traverse (traverse f))) rowLabels
      <*> traverse (\(Tuple tok t) -> Tuple tok <$> traverse f t) rowTail
  sequence = traverse identity

-- -----------------------------------------------------------------------
-- Module structure
-- -----------------------------------------------------------------------

data Module a = Module
  { modAnn             :: a
  , modKeyword         :: SourceToken
  , modNamespace       :: Name ModuleName
  , modExports         :: Maybe (DelimitedNonEmpty (Export a))
  , modWhere           :: SourceToken
  , modImports         :: Array (ImportDecl a)
  , modDecls           :: Array (Declaration a)
  , modTrailingComments :: Array (Comment LineFeed)
  }

derive instance functorModule :: Functor Module
derive instance eqModule :: Eq a => Eq (Module a)
derive instance ordModule :: Ord a => Ord (Module a)

instance showModule :: Show a => Show (Module a) where
  show (Module { modNamespace }) = "(CSTModule " <> show modNamespace <> ")"

instance foldableModule :: Foldable Module where
  foldr f z (Module m) = f m.modAnn (foldr (flip (foldr f)) (foldr (flip (foldr f)) z m.modDecls) m.modImports)
  foldl f z (Module m) = foldl (foldl f) (foldl (foldl f) (f z m.modAnn) m.modImports) m.modDecls
  foldMap f (Module m) = f m.modAnn <> foldMap (foldMap f) m.modImports <> foldMap (foldMap f) m.modDecls

instance traversableModule :: Traversable Module where
  traverse f (Module m) =
    (\a exps imps decls -> Module m { modAnn = a, modExports = exps, modImports = imps, modDecls = decls })
      <$> f m.modAnn
      <*> traverse (traverse (traverse (traverse f))) m.modExports
      <*> traverse (traverse f) m.modImports
      <*> traverse (traverse f) m.modDecls
  sequence = traverse identity

data Export a
  = ExportValue a (Name Ident)
  | ExportOp a (Name (OpName ValueOpName))
  | ExportType a (Name (ProperName TypeName)) (Maybe (DataMembers a))
  | ExportTypeOp a SourceToken (Name (OpName TypeOpName))
  | ExportClass a SourceToken (Name (ProperName ClassName))
  | ExportModule a SourceToken (Name ModuleName)

derive instance functorExport :: Functor Export
derive instance eqExport :: Eq a => Eq (Export a)
derive instance ordExport :: Ord a => Ord (Export a)

instance showExport :: Show a => Show (Export a) where
  show _ = "<Export>"

instance foldableExport :: Foldable Export where
  foldr f z (ExportValue a _)       = f a z
  foldr f z (ExportOp a _)          = f a z
  foldr f z (ExportType a _ mDM)    = f a (foldr (flip (foldr f)) z mDM)
  foldr f z (ExportTypeOp a _ _)    = f a z
  foldr f z (ExportClass a _ _)     = f a z
  foldr f z (ExportModule a _ _)    = f a z
  foldl f z (ExportValue a _)       = f z a
  foldl f z (ExportOp a _)          = f z a
  foldl f z (ExportType a _ mDM)    = foldl (foldl f) (f z a) mDM
  foldl f z (ExportTypeOp a _ _)    = f z a
  foldl f z (ExportClass a _ _)     = f z a
  foldl f z (ExportModule a _ _)    = f z a
  foldMap f (ExportValue a _)       = f a
  foldMap f (ExportOp a _)          = f a
  foldMap f (ExportType a _ mDM)    = f a <> foldMap (foldMap f) mDM
  foldMap f (ExportTypeOp a _ _)    = f a
  foldMap f (ExportClass a _ _)     = f a
  foldMap f (ExportModule a _ _)    = f a

instance traversableExport :: Traversable Export where
  traverse f (ExportValue a n)        = ExportValue <$> f a <*> pure n
  traverse f (ExportOp a n)           = ExportOp <$> f a <*> pure n
  traverse f (ExportType a n mDM)     = ExportType <$> f a <*> pure n <*> traverse (traverse f) mDM
  traverse f (ExportTypeOp a tok n)   = ExportTypeOp <$> f a <*> pure tok <*> pure n
  traverse f (ExportClass a tok n)    = ExportClass <$> f a <*> pure tok <*> pure n
  traverse f (ExportModule a tok n)   = ExportModule <$> f a <*> pure tok <*> pure n
  sequence = traverse identity

data DataMembers a
  = DataAll a SourceToken
  | DataEnumerated a (Delimited (Name (ProperName ConstructorName)))

derive instance functorDataMembers :: Functor DataMembers
derive instance eqDataMembers :: Eq a => Eq (DataMembers a)
derive instance ordDataMembers :: Ord a => Ord (DataMembers a)

instance showDataMembers :: Show a => Show (DataMembers a) where
  show _ = "<DataMembers>"

instance foldableDataMembers :: Foldable DataMembers where
  foldr f z (DataAll a _)          = f a z
  foldr f z (DataEnumerated a _)   = f a z
  foldl f z (DataAll a _)          = f z a
  foldl f z (DataEnumerated a _)   = f z a
  foldMap f (DataAll a _)          = f a
  foldMap f (DataEnumerated a _)   = f a

instance traversableDataMembers :: Traversable DataMembers where
  traverse f (DataAll a tok)        = DataAll <$> f a <*> pure tok
  traverse f (DataEnumerated a del) = DataEnumerated <$> f a <*> pure del
  sequence = traverse identity

data Declaration a
  = DeclData a (DataHead a) (Maybe (Tuple SourceToken (Separated (DataCtor a))))
  | DeclType a (DataHead a) SourceToken (Type a)
  | DeclNewtype a (DataHead a) SourceToken (Name (ProperName ConstructorName)) (Type a)
  | DeclClass a (ClassHead a) (Maybe (Tuple SourceToken (NonEmptyList (Labeled (Name Ident) (Type a)))))
  | DeclInstanceChain a (Separated (Instance a))
  | DeclDerive a SourceToken (Maybe SourceToken) (InstanceHead a)
  | DeclKindSignature a SourceToken (Labeled (Name (ProperName TypeName)) (Type a))
  | DeclSignature a (Labeled (Name Ident) (Type a))
  | DeclValue a (ValueBindingFields a)
  | DeclFixity a FixityFields
  | DeclForeign a SourceToken SourceToken (Foreign a)
  | DeclRole a SourceToken SourceToken (Name (ProperName TypeName)) (NonEmptyList Role)

derive instance functorDeclaration :: Functor Declaration
derive instance eqDeclaration :: Eq a => Eq (Declaration a)
derive instance ordDeclaration :: Ord a => Ord (Declaration a)

instance showDeclaration :: Show a => Show (Declaration a) where
  show _ = "<CSTDeclaration>"

instance foldableDeclaration :: Foldable Declaration where
  foldr f z (DeclData a _ _)          = f a z
  foldr f z (DeclType a _ _ _)        = f a z
  foldr f z (DeclNewtype a _ _ _ _)   = f a z
  foldr f z (DeclClass a _ _)         = f a z
  foldr f z (DeclInstanceChain a _)   = f a z
  foldr f z (DeclDerive a _ _ _)      = f a z
  foldr f z (DeclKindSignature a _ _) = f a z
  foldr f z (DeclSignature a _)       = f a z
  foldr f z (DeclValue a _)           = f a z
  foldr f z (DeclFixity a _)          = f a z
  foldr f z (DeclForeign a _ _ _)     = f a z
  foldr f z (DeclRole a _ _ _ _)      = f a z
  foldl f z (DeclData a _ _)          = f z a
  foldl f z (DeclType a _ _ _)        = f z a
  foldl f z (DeclNewtype a _ _ _ _)   = f z a
  foldl f z (DeclClass a _ _)         = f z a
  foldl f z (DeclInstanceChain a _)   = f z a
  foldl f z (DeclDerive a _ _ _)      = f z a
  foldl f z (DeclKindSignature a _ _) = f z a
  foldl f z (DeclSignature a _)       = f z a
  foldl f z (DeclValue a _)           = f z a
  foldl f z (DeclFixity a _)          = f z a
  foldl f z (DeclForeign a _ _ _)     = f z a
  foldl f z (DeclRole a _ _ _ _)      = f z a
  foldMap f (DeclData a _ _)          = f a
  foldMap f (DeclType a _ _ _)        = f a
  foldMap f (DeclNewtype a _ _ _ _)   = f a
  foldMap f (DeclClass a _ _)         = f a
  foldMap f (DeclInstanceChain a _)   = f a
  foldMap f (DeclDerive a _ _ _)      = f a
  foldMap f (DeclKindSignature a _ _) = f a
  foldMap f (DeclSignature a _)       = f a
  foldMap f (DeclValue a _)           = f a
  foldMap f (DeclFixity a _)          = f a
  foldMap f (DeclForeign a _ _ _)     = f a
  foldMap f (DeclRole a _ _ _ _)      = f a

instance traversableDeclaration :: Traversable Declaration where
  traverse f (DeclData a dh mctors) =
    DeclData <$> f a <*> traverse f dh <*> traverse (traverse (traverse (traverse f))) mctors
  traverse f (DeclType a dh tok ty) =
    DeclType <$> f a <*> traverse f dh <*> pure tok <*> traverse f ty
  traverse f (DeclNewtype a dh tok n ty) =
    DeclNewtype <$> f a <*> traverse f dh <*> pure tok <*> pure n <*> traverse f ty
  traverse f (DeclClass a ch mbody) =
    DeclClass <$> f a <*> traverse f ch <*> traverse (\(Tuple tok bs) -> Tuple tok <$> traverse (traverse (traverse f)) bs) mbody
  traverse f (DeclInstanceChain a sep) =
    DeclInstanceChain <$> f a <*> traverse (traverse f) sep
  traverse f (DeclDerive a tok mnt ih) =
    DeclDerive <$> f a <*> pure tok <*> pure mnt <*> traverse f ih
  traverse f (DeclKindSignature a tok lab) =
    DeclKindSignature <$> f a <*> pure tok <*> traverse (traverse f) lab
  traverse f (DeclSignature a lab) =
    DeclSignature <$> f a <*> traverse (traverse f) lab
  traverse f (DeclValue a vbf) =
    DeclValue <$> f a <*> traverse f vbf
  traverse f (DeclFixity a ff) =
    DeclFixity <$> f a <*> pure ff
  traverse f (DeclForeign a tok1 tok2 fgn) =
    DeclForeign <$> f a <*> pure tok1 <*> pure tok2 <*> traverse f fgn
  traverse f (DeclRole a tok1 tok2 n roles) =
    DeclRole <$> f a <*> pure tok1 <*> pure tok2 <*> pure n <*> pure roles
  sequence = traverse identity

data Instance a = Instance
  { instHead :: InstanceHead a
  , instBody :: Maybe (Tuple SourceToken (NonEmptyList (InstanceBinding a)))
  }

derive instance functorInstance  :: Functor Instance
derive instance eqInstance  :: Eq a  => Eq (Instance a)
derive instance ordInstance :: Ord a => Ord (Instance a)

instance showInstance :: Show a => Show (Instance a) where
  show _ = "<Instance>"

instance foldableInstance :: Foldable Instance where
  foldr f z (Instance { instHead, instBody }) =
    foldr f (foldr (\(Tuple _ bs) acc -> foldr (flip (foldr f)) acc bs) z instBody) instHead
  foldl f z (Instance { instHead, instBody }) =
    foldl (\acc (Tuple _ bs) -> foldl (foldl f) acc bs) (foldl f z instHead) instBody
  foldMap f (Instance { instHead, instBody }) =
    foldMap f instHead <> foldMap (\(Tuple _ bs) -> foldMap (foldMap f) bs) instBody

instance traversableInstance :: Traversable Instance where
  traverse f (Instance inst) =
    (\h b -> Instance inst { instHead = h, instBody = b })
      <$> traverse f inst.instHead
      <*> traverse (\(Tuple tok bs) -> Tuple tok <$> traverse (traverse f) bs) inst.instBody
  sequence = traverse identity

data InstanceBinding a
  = InstanceBindingSignature a (Labeled (Name Ident) (Type a))
  | InstanceBindingName a (ValueBindingFields a)

derive instance functorInstanceBinding :: Functor InstanceBinding
derive instance eqInstanceBinding :: Eq a => Eq (InstanceBinding a)
derive instance ordInstanceBinding :: Ord a => Ord (InstanceBinding a)

instance showInstanceBinding :: Show a => Show (InstanceBinding a) where
  show _ = "<InstanceBinding>"

instance foldableInstanceBinding :: Foldable InstanceBinding where
  foldr f z (InstanceBindingSignature a _) = f a z
  foldr f z (InstanceBindingName a _)      = f a z
  foldl f z (InstanceBindingSignature a _) = f z a
  foldl f z (InstanceBindingName a _)      = f z a
  foldMap f (InstanceBindingSignature a _) = f a
  foldMap f (InstanceBindingName a _)      = f a

instance traversableInstanceBinding :: Traversable InstanceBinding where
  traverse f (InstanceBindingSignature a lab) = InstanceBindingSignature <$> f a <*> traverse (traverse f) lab
  traverse f (InstanceBindingName a vbf)      = InstanceBindingName <$> f a <*> traverse f vbf
  sequence = traverse identity

data ImportDecl a = ImportDecl
  { impAnn     :: a
  , impKeyword :: SourceToken
  , impModule  :: Name ModuleName
  , impNames   :: Maybe (Tuple (Maybe SourceToken) (DelimitedNonEmpty (Import a)))
  , impQual    :: Maybe (Tuple SourceToken (Name ModuleName))
  }

derive instance functorImportDecl :: Functor ImportDecl
derive instance eqImportDecl :: Eq a => Eq (ImportDecl a)
derive instance ordImportDecl :: Ord a => Ord (ImportDecl a)

instance showImportDecl :: Show a => Show (ImportDecl a) where
  show (ImportDecl { impModule }) = "(ImportDecl " <> show impModule <> ")"

instance foldableImportDecl :: Foldable ImportDecl where
  foldr f z (ImportDecl { impAnn, impNames }) =
    f impAnn (foldr (\(Tuple _ del) acc -> foldr (flip (foldr (flip (foldr f)))) acc del) z impNames)
  foldl f z (ImportDecl { impAnn, impNames }) =
    foldl (\acc (Tuple _ del) -> foldl (foldl (foldl f)) acc del) (f z impAnn) impNames
  foldMap f (ImportDecl { impAnn, impNames }) =
    f impAnn <> foldMap (\(Tuple _ del) -> foldMap (foldMap (foldMap f)) del) impNames

instance traversableImportDecl :: Traversable ImportDecl where
  traverse f (ImportDecl imp) =
    (\a names -> ImportDecl imp { impAnn = a, impNames = names })
      <$> f imp.impAnn
      <*> traverse (\(Tuple mhide del) -> Tuple mhide <$> traverse (traverse (traverse f)) del) imp.impNames
  sequence = traverse identity

data Import a
  = ImportValue a (Name Ident)
  | ImportOp a (Name (OpName ValueOpName))
  | ImportType a (Name (ProperName TypeName)) (Maybe (DataMembers a))
  | ImportTypeOp a SourceToken (Name (OpName TypeOpName))
  | ImportClass a SourceToken (Name (ProperName ClassName))

derive instance functorImport :: Functor Import
derive instance eqImport :: Eq a => Eq (Import a)
derive instance ordImport :: Ord a => Ord (Import a)

instance showImport :: Show a => Show (Import a) where
  show _ = "<Import>"

instance foldableImport :: Foldable Import where
  foldr f z (ImportValue a _)       = f a z
  foldr f z (ImportOp a _)          = f a z
  foldr f z (ImportType a _ _)      = f a z
  foldr f z (ImportTypeOp a _ _)    = f a z
  foldr f z (ImportClass a _ _)     = f a z
  foldl f z (ImportValue a _)       = f z a
  foldl f z (ImportOp a _)          = f z a
  foldl f z (ImportType a _ _)      = f z a
  foldl f z (ImportTypeOp a _ _)    = f z a
  foldl f z (ImportClass a _ _)     = f z a
  foldMap f (ImportValue a _)       = f a
  foldMap f (ImportOp a _)          = f a
  foldMap f (ImportType a _ _)      = f a
  foldMap f (ImportTypeOp a _ _)    = f a
  foldMap f (ImportClass a _ _)     = f a

instance traversableImport :: Traversable Import where
  traverse f (ImportValue a n)       = ImportValue <$> f a <*> pure n
  traverse f (ImportOp a n)          = ImportOp <$> f a <*> pure n
  traverse f (ImportType a n mDM)    = ImportType <$> f a <*> pure n <*> traverse (traverse f) mDM
  traverse f (ImportTypeOp a tok n)  = ImportTypeOp <$> f a <*> pure tok <*> pure n
  traverse f (ImportClass a tok n)   = ImportClass <$> f a <*> pure tok <*> pure n
  sequence = traverse identity

data DataHead a = DataHead
  { dataHdKeyword :: SourceToken
  , dataHdName    :: Name (ProperName TypeName)
  , dataHdVars    :: Array (TypeVarBinding a)
  }

derive instance functorDataHead :: Functor DataHead
derive instance eqDataHead :: Eq a => Eq (DataHead a)
derive instance ordDataHead :: Ord a => Ord (DataHead a)

instance showDataHead :: Show a => Show (DataHead a) where
  show (DataHead { dataHdName }) = "(DataHead " <> show dataHdName <> ")"

instance foldableDataHead :: Foldable DataHead where
  foldr f z (DataHead { dataHdVars }) = foldr (flip (foldr f)) z dataHdVars
  foldl f z (DataHead { dataHdVars }) = foldl (foldl f) z dataHdVars
  foldMap f (DataHead { dataHdVars }) = foldMap (foldMap f) dataHdVars

instance traversableDataHead :: Traversable DataHead where
  traverse f (DataHead dh) = (\vs -> DataHead dh { dataHdVars = vs }) <$> traverse (traverse f) dh.dataHdVars
  sequence = traverse identity

data DataCtor a = DataCtor
  { dataCtorAnn    :: a
  , dataCtorName   :: Name (ProperName ConstructorName)
  , dataCtorFields :: Array (Type a)
  }

derive instance functorDataCtor :: Functor DataCtor
derive instance eqDataCtor :: Eq a => Eq (DataCtor a)
derive instance ordDataCtor :: Ord a => Ord (DataCtor a)

instance showDataCtor :: Show a => Show (DataCtor a) where
  show (DataCtor { dataCtorName }) = "(DataCtor " <> show dataCtorName <> ")"

instance foldableDataCtor :: Foldable DataCtor where
  foldr f z (DataCtor { dataCtorAnn, dataCtorFields }) =
    f dataCtorAnn (foldr (flip (foldr f)) z dataCtorFields)
  foldl f z (DataCtor { dataCtorAnn, dataCtorFields }) =
    foldl (foldl f) (f z dataCtorAnn) dataCtorFields
  foldMap f (DataCtor { dataCtorAnn, dataCtorFields }) =
    f dataCtorAnn <> foldMap (foldMap f) dataCtorFields

instance traversableDataCtor :: Traversable DataCtor where
  traverse f (DataCtor dc) =
    (\a flds -> DataCtor dc { dataCtorAnn = a, dataCtorFields = flds })
      <$> f dc.dataCtorAnn
      <*> traverse (traverse f) dc.dataCtorFields
  sequence = traverse identity

data ClassHead a = ClassHead
  { clsKeyword :: SourceToken
  , clsSuper   :: Maybe (Tuple (OneOrDelimited (Constraint a)) SourceToken)
  , clsName    :: Name (ProperName ClassName)
  , clsVars    :: Array (TypeVarBinding a)
  , clsFundeps :: Maybe (Tuple SourceToken (Separated ClassFundep))
  }

derive instance functorClassHead :: Functor ClassHead
derive instance eqClassHead :: Eq a => Eq (ClassHead a)
derive instance ordClassHead :: Ord a => Ord (ClassHead a)

instance showClassHead :: Show a => Show (ClassHead a) where
  show (ClassHead { clsName }) = "(ClassHead " <> show clsName <> ")"

instance foldableClassHead :: Foldable ClassHead where
  foldr f z (ClassHead { clsSuper, clsVars }) =
    foldr (\(Tuple od _) acc -> foldr (flip (foldr f)) acc od) (foldr (flip (foldr f)) z clsVars) clsSuper
  foldl f z (ClassHead { clsSuper, clsVars }) =
    foldl (foldl f) (foldl (\acc (Tuple od _) -> foldl (foldl f) acc od) z clsSuper) clsVars
  foldMap f (ClassHead { clsSuper, clsVars }) =
    foldMap (\(Tuple od _) -> foldMap (foldMap f) od) clsSuper <>
    foldMap (foldMap f) clsVars

instance traversableClassHead :: Traversable ClassHead where
  traverse f (ClassHead ch) =
    (\super vars -> ClassHead ch { clsSuper = super, clsVars = vars })
      <$> traverse (\(Tuple od tok) -> Tuple <$> traverse (traverse f) od <*> pure tok) ch.clsSuper
      <*> traverse (traverse f) ch.clsVars
  sequence = traverse identity

data ClassFundep
  = FundepDetermined SourceToken (NonEmptyList (Name Ident))
  | FundepDetermines (NonEmptyList (Name Ident)) SourceToken (NonEmptyList (Name Ident))

derive instance eqClassFundep  :: Eq ClassFundep
derive instance ordClassFundep :: Ord ClassFundep

instance showClassFundep :: Show ClassFundep where
  show _ = "<ClassFundep>"

data InstanceHead a = InstanceHead
  { instKeyword     :: SourceToken
  , instNameSep     :: Maybe (Tuple (Name Ident) SourceToken)
  , instConstraints :: Maybe (Tuple (OneOrDelimited (Constraint a)) SourceToken)
  , instClass       :: QualifiedName (ProperName ClassName)
  , instTypes       :: Array (Type a)
  }

derive instance functorInstanceHead :: Functor InstanceHead
derive instance eqInstanceHead :: Eq a => Eq (InstanceHead a)
derive instance ordInstanceHead :: Ord a => Ord (InstanceHead a)

instance showInstanceHead :: Show a => Show (InstanceHead a) where
  show _ = "<InstanceHead>"

instance foldableInstanceHead :: Foldable InstanceHead where
  foldr f z (InstanceHead { instConstraints, instTypes }) =
    foldr (\(Tuple od _) acc -> foldr (flip (foldr f)) acc od) (foldr (flip (foldr f)) z instTypes) instConstraints
  foldl f z (InstanceHead { instConstraints, instTypes }) =
    foldl (foldl f) (foldl (\acc (Tuple od _) -> foldl (foldl f) acc od) z instConstraints) instTypes
  foldMap f (InstanceHead { instConstraints, instTypes }) =
    foldMap (\(Tuple od _) -> foldMap (foldMap f) od) instConstraints <>
    foldMap (foldMap f) instTypes

instance traversableInstanceHead :: Traversable InstanceHead where
  traverse f (InstanceHead ih) =
    (\cs tys -> InstanceHead ih { instConstraints = cs, instTypes = tys })
      <$> traverse (\(Tuple od tok) -> Tuple <$> traverse (traverse f) od <*> pure tok) ih.instConstraints
      <*> traverse (traverse f) ih.instTypes
  sequence = traverse identity

data Fixity = Infix | Infixl | Infixr

derive instance eqFixity  :: Eq Fixity
derive instance ordFixity :: Ord Fixity

instance showFixity :: Show Fixity where
  show Infix  = "Infix"
  show Infixl = "Infixl"
  show Infixr = "Infixr"

data FixityOp
  = FixityValue (QualifiedName (Either Ident (ProperName ConstructorName))) SourceToken (Name (OpName ValueOpName))
  | FixityType SourceToken (QualifiedName (ProperName TypeName)) SourceToken (Name (OpName TypeOpName))

derive instance eqFixityOp  :: Eq FixityOp
derive instance ordFixityOp :: Ord FixityOp

instance showFixityOp :: Show FixityOp where
  show _ = "<FixityOp>"

data FixityFields = FixityFields
  { fxtKeyword :: Tuple SourceToken Fixity
  , fxtPrec    :: Tuple SourceToken Int
  , fxtOp      :: FixityOp
  }

derive instance eqFixityFields  :: Eq FixityFields
derive instance ordFixityFields :: Ord FixityFields

instance showFixityFields :: Show FixityFields where
  show _ = "<FixityFields>"

data ValueBindingFields a = ValueBindingFields
  { valName    :: Name Ident
  , valBinders :: Array (Binder a)
  , valGuarded :: Guarded a
  }

derive instance functorValueBindingFields :: Functor ValueBindingFields
derive instance eqValueBindingFields :: Eq a => Eq (ValueBindingFields a)
derive instance ordValueBindingFields :: Ord a => Ord (ValueBindingFields a)

instance showValueBindingFields :: Show a => Show (ValueBindingFields a) where
  show (ValueBindingFields { valName }) = "(ValueBindingFields " <> show valName <> ")"

instance foldableValueBindingFields :: Foldable ValueBindingFields where
  foldr f z (ValueBindingFields { valBinders, valGuarded }) =
    foldr (flip (foldr f)) (foldr f z valGuarded) valBinders
  foldl f z (ValueBindingFields { valBinders, valGuarded }) =
    foldl f (foldl (foldl f) z valBinders) valGuarded
  foldMap f (ValueBindingFields { valBinders, valGuarded }) =
    foldMap (foldMap f) valBinders <> foldMap f valGuarded

instance traversableValueBindingFields :: Traversable ValueBindingFields where
  traverse f (ValueBindingFields vbf) =
    (\bs g -> ValueBindingFields vbf { valBinders = bs, valGuarded = g })
      <$> traverse (traverse f) vbf.valBinders
      <*> traverse f vbf.valGuarded
  sequence = traverse identity

data Guarded a
  = Unconditional SourceToken (Where a)
  | Guarded (NonEmptyList (GuardedExpr a))

derive instance functorGuarded :: Functor Guarded
derive instance eqGuarded :: Eq a => Eq (Guarded a)
derive instance ordGuarded :: Ord a => Ord (Guarded a)

instance showGuarded :: Show a => Show (Guarded a) where
  show _ = "<Guarded>"

instance foldableGuarded :: Foldable Guarded where
  foldr f z (Unconditional _ w) = foldr f z w
  foldr f z (Guarded ges)       = foldr (flip (foldr f)) z ges
  foldl f z (Unconditional _ w) = foldl f z w
  foldl f z (Guarded ges)       = foldl (foldl f) z ges
  foldMap f (Unconditional _ w) = foldMap f w
  foldMap f (Guarded ges)       = foldMap (foldMap f) ges

instance traversableGuarded :: Traversable Guarded where
  traverse f (Unconditional tok w) = Unconditional tok <$> traverse f w
  traverse f (Guarded ges)         = Guarded <$> traverse (traverse f) ges
  sequence = traverse identity

data GuardedExpr a = GuardedExpr
  { grdBar      :: SourceToken
  , grdPatterns :: Separated (PatternGuard a)
  , grdSep      :: SourceToken
  , grdWhere    :: Where a
  }

derive instance functorGuardedExpr :: Functor GuardedExpr
derive instance eqGuardedExpr :: Eq a => Eq (GuardedExpr a)
derive instance ordGuardedExpr :: Ord a => Ord (GuardedExpr a)

instance showGuardedExpr :: Show a => Show (GuardedExpr a) where
  show _ = "<GuardedExpr>"

instance foldableGuardedExpr :: Foldable GuardedExpr where
  foldr f z (GuardedExpr { grdPatterns, grdWhere }) =
    foldr (flip (foldr f)) (foldr f z grdWhere) grdPatterns
  foldl f z (GuardedExpr { grdPatterns, grdWhere }) =
    foldl f (foldl (foldl f) z grdPatterns) grdWhere
  foldMap f (GuardedExpr { grdPatterns, grdWhere }) =
    foldMap (foldMap f) grdPatterns <> foldMap f grdWhere

instance traversableGuardedExpr :: Traversable GuardedExpr where
  traverse f (GuardedExpr ge) =
    (\pats wh -> GuardedExpr ge { grdPatterns = pats, grdWhere = wh })
      <$> traverse (traverse f) ge.grdPatterns
      <*> traverse f ge.grdWhere
  sequence = traverse identity

data PatternGuard a = PatternGuard
  { patBinder :: Maybe (Tuple (Binder a) SourceToken)
  , patExpr   :: Expr a
  }

derive instance functorPatternGuard :: Functor PatternGuard
derive instance eqPatternGuard :: Eq a => Eq (PatternGuard a)
derive instance ordPatternGuard :: Ord a => Ord (PatternGuard a)

instance showPatternGuard :: Show a => Show (PatternGuard a) where
  show _ = "<PatternGuard>"

instance foldablePatternGuard :: Foldable PatternGuard where
  foldr f z (PatternGuard { patBinder, patExpr }) =
    foldr (flip (foldr f) <<< fst') (foldr f z patExpr) patBinder
    where fst' (Tuple b _) = b
  foldl f z (PatternGuard { patBinder, patExpr }) =
    foldl f (foldl (\acc (Tuple b _) -> foldl f acc b) z patBinder) patExpr
  foldMap f (PatternGuard { patBinder, patExpr }) =
    foldMap (\(Tuple b _) -> foldMap f b) patBinder <> foldMap f patExpr

instance traversablePatternGuard :: Traversable PatternGuard where
  traverse f (PatternGuard pg) =
    (\pb e -> PatternGuard pg { patBinder = pb, patExpr = e })
      <$> traverse (\(Tuple b tok) -> Tuple <$> traverse f b <*> pure tok) pg.patBinder
      <*> traverse f pg.patExpr
  sequence = traverse identity

data Foreign a
  = ForeignValue (Labeled (Name Ident) (Type a))
  | ForeignData SourceToken (Labeled (Name (ProperName TypeName)) (Type a))
  | ForeignKind SourceToken (Name (ProperName TypeName))

derive instance functorForeign :: Functor Foreign
derive instance eqForeign :: Eq a => Eq (Foreign a)
derive instance ordForeign :: Ord a => Ord (Foreign a)

instance showForeign :: Show a => Show (Foreign a) where
  show _ = "<Foreign>"

instance foldableForeign :: Foldable Foreign where
  foldr f z (ForeignValue lab)       = foldr (flip (foldr f)) z lab
  foldr f z (ForeignData _ lab)      = foldr (flip (foldr f)) z lab
  foldr _ z (ForeignKind _ _)        = z
  foldl f z (ForeignValue lab)       = foldl (foldl f) z lab
  foldl f z (ForeignData _ lab)      = foldl (foldl f) z lab
  foldl f z (ForeignKind _ _)        = z
  foldMap f (ForeignValue lab)       = foldMap (foldMap f) lab
  foldMap f (ForeignData _ lab)      = foldMap (foldMap f) lab
  foldMap _ (ForeignKind _ _)        = mempty

instance traversableForeign :: Traversable Foreign where
  traverse f (ForeignValue lab)      = ForeignValue <$> traverse (traverse f) lab
  traverse f (ForeignData tok lab)   = ForeignData tok <$> traverse (traverse f) lab
  traverse _ (ForeignKind tok n)     = pure (ForeignKind tok n)
  sequence = traverse identity

-- | CST Role (with source token, distinct from Roles.Role)
data Role = Role
  { roleTok   :: SourceToken
  , roleValue :: R.Role
  }

derive instance eqRole  :: Eq Role
derive instance ordRole :: Ord Role

instance showRole :: Show Role where
  show (Role { roleValue }) = "(Role " <> show roleValue <> ")"

-- -----------------------------------------------------------------------
-- Expressions
-- -----------------------------------------------------------------------

data Expr a
  = ExprHole a (Name Ident)
  | ExprSection a SourceToken
  | ExprIdent a (QualifiedName Ident)
  | ExprConstructor a (QualifiedName (ProperName ConstructorName))
  | ExprBoolean a SourceToken Boolean
  | ExprChar a SourceToken Char
  | ExprString a SourceToken PSString
  | ExprNumber a SourceToken (Either Int Number)
  | ExprArray a (Delimited (Expr a))
  | ExprRecord a (Delimited (RecordLabeled (Expr a)))
  | ExprParens a (Wrapped (Expr a))
  | ExprTyped a (Expr a) SourceToken (Type a)
  | ExprInfix a (Expr a) (Wrapped (Expr a)) (Expr a)
  | ExprOp a (Expr a) (QualifiedName (OpName ValueOpName)) (Expr a)
  | ExprOpName a (QualifiedName (OpName ValueOpName))
  | ExprNegate a SourceToken (Expr a)
  | ExprRecordAccessor a (RecordAccessor a)
  | ExprRecordUpdate a (Expr a) (DelimitedNonEmpty (RecordUpdate a))
  | ExprApp a (Expr a) (Expr a)
  | ExprVisibleTypeApp a (Expr a) SourceToken (Type a)
  | ExprLambda a (Lambda a)
  | ExprIf a (IfThenElse a)
  | ExprCase a (CaseOf a)
  | ExprLet a (LetIn a)
  | ExprDo a (DoBlock a)
  | ExprAdo a (AdoBlock a)

derive instance functorExpr :: Functor Expr
derive instance eqExpr :: Eq a => Eq (Expr a)
derive instance ordExpr :: Ord a => Ord (Expr a)

instance showExpr :: Show a => Show (Expr a) where
  show _ = "<CSTExpr>"

instance foldableExpr :: Foldable Expr where
  foldr f z e = f (exprAnn e) z
  foldl f z e = f z (exprAnn e)
  foldMap f e = f (exprAnn e)

instance traversableExpr :: Traversable Expr where
  traverse f (ExprHole a n)           = ExprHole <$> f a <*> pure n
  traverse f (ExprSection a tok)      = ExprSection <$> f a <*> pure tok
  traverse f (ExprIdent a q)          = ExprIdent <$> f a <*> pure q
  traverse f (ExprConstructor a q)    = ExprConstructor <$> f a <*> pure q
  traverse f (ExprBoolean a tok b)    = ExprBoolean <$> f a <*> pure tok <*> pure b
  traverse f (ExprChar a tok c)       = ExprChar <$> f a <*> pure tok <*> pure c
  traverse f (ExprString a tok ps)    = ExprString <$> f a <*> pure tok <*> pure ps
  traverse f (ExprNumber a tok n)     = ExprNumber <$> f a <*> pure tok <*> pure n
  traverse f (ExprArray a del)        = ExprArray <$> f a <*> traverse (traverse (traverse (traverse f))) del
  traverse f (ExprRecord a del)       = ExprRecord <$> f a <*> traverse (traverse (traverse (traverse (traverse f)))) del
  traverse f (ExprParens a w)         = ExprParens <$> f a <*> traverse (traverse f) w
  traverse f (ExprTyped a e tok ty)   = ExprTyped <$> f a <*> traverse f e <*> pure tok <*> traverse f ty
  traverse f (ExprInfix a e1 w e2)    = ExprInfix <$> f a <*> traverse f e1 <*> traverse (traverse f) w <*> traverse f e2
  traverse f (ExprOp a e1 q e2)       = ExprOp <$> f a <*> traverse f e1 <*> pure q <*> traverse f e2
  traverse f (ExprOpName a q)         = ExprOpName <$> f a <*> pure q
  traverse f (ExprNegate a tok e)     = ExprNegate <$> f a <*> pure tok <*> traverse f e
  traverse f (ExprRecordAccessor a ra) = ExprRecordAccessor <$> f a <*> traverse f ra
  traverse f (ExprRecordUpdate a e del) = ExprRecordUpdate <$> f a <*> traverse f e <*> traverse (traverse (traverse f)) del
  traverse f (ExprApp a e1 e2)        = ExprApp <$> f a <*> traverse f e1 <*> traverse f e2
  traverse f (ExprVisibleTypeApp a e tok ty) = ExprVisibleTypeApp <$> f a <*> traverse f e <*> pure tok <*> traverse f ty
  traverse f (ExprLambda a lam)       = ExprLambda <$> f a <*> traverse f lam
  traverse f (ExprIf a ite)           = ExprIf <$> f a <*> traverse f ite
  traverse f (ExprCase a co)          = ExprCase <$> f a <*> traverse f co
  traverse f (ExprLet a li)           = ExprLet <$> f a <*> traverse f li
  traverse f (ExprDo a db)            = ExprDo <$> f a <*> traverse f db
  traverse f (ExprAdo a ab)           = ExprAdo <$> f a <*> traverse f ab
  sequence = traverse identity

exprAnn :: forall a. Expr a -> a
exprAnn (ExprHole a _)            = a
exprAnn (ExprSection a _)         = a
exprAnn (ExprIdent a _)           = a
exprAnn (ExprConstructor a _)     = a
exprAnn (ExprBoolean a _ _)       = a
exprAnn (ExprChar a _ _)          = a
exprAnn (ExprString a _ _)        = a
exprAnn (ExprNumber a _ _)        = a
exprAnn (ExprArray a _)           = a
exprAnn (ExprRecord a _)          = a
exprAnn (ExprParens a _)          = a
exprAnn (ExprTyped a _ _ _)       = a
exprAnn (ExprInfix a _ _ _)       = a
exprAnn (ExprOp a _ _ _)          = a
exprAnn (ExprOpName a _)          = a
exprAnn (ExprNegate a _ _)        = a
exprAnn (ExprRecordAccessor a _)  = a
exprAnn (ExprRecordUpdate a _ _)  = a
exprAnn (ExprApp a _ _)           = a
exprAnn (ExprVisibleTypeApp a _ _ _) = a
exprAnn (ExprLambda a _)          = a
exprAnn (ExprIf a _)              = a
exprAnn (ExprCase a _)            = a
exprAnn (ExprLet a _)             = a
exprAnn (ExprDo a _)              = a
exprAnn (ExprAdo a _)             = a

data RecordLabeled a
  = RecordPun (Name Ident)
  | RecordField Label SourceToken a

derive instance functorRecordLabeled :: Functor RecordLabeled
derive instance eqRecordLabeled :: Eq a => Eq (RecordLabeled a)
derive instance ordRecordLabeled :: Ord a => Ord (RecordLabeled a)

instance showRecordLabeled :: Show a => Show (RecordLabeled a) where
  show _ = "<RecordLabeled>"

instance foldableRecordLabeled :: Foldable RecordLabeled where
  foldr _ z (RecordPun _)          = z
  foldr f z (RecordField _ _ a)    = f a z
  foldl _ z (RecordPun _)          = z
  foldl f z (RecordField _ _ a)    = f z a
  foldMap _ (RecordPun _)          = mempty
  foldMap f (RecordField _ _ a)    = f a

instance traversableRecordLabeled :: Traversable RecordLabeled where
  traverse _ (RecordPun n)           = pure (RecordPun n)
  traverse f (RecordField l tok a)   = RecordField l tok <$> f a
  sequence = traverse identity

data RecordUpdate a
  = RecordUpdateLeaf Label SourceToken (Expr a)
  | RecordUpdateBranch Label (DelimitedNonEmpty (RecordUpdate a))

derive instance functorRecordUpdate :: Functor RecordUpdate
derive instance eqRecordUpdate :: Eq a => Eq (RecordUpdate a)
derive instance ordRecordUpdate :: Ord a => Ord (RecordUpdate a)

instance showRecordUpdate :: Show a => Show (RecordUpdate a) where
  show _ = "<RecordUpdate>"

instance foldableRecordUpdate :: Foldable RecordUpdate where
  foldr f z (RecordUpdateLeaf _ _ e) = foldr f z e
  foldr f z (RecordUpdateBranch _ del) = foldr (flip (foldr (flip (foldr f)))) z del
  foldl f z (RecordUpdateLeaf _ _ e) = foldl f z e
  foldl f z (RecordUpdateBranch _ del) = foldl (foldl (foldl f)) z del
  foldMap f (RecordUpdateLeaf _ _ e) = foldMap f e
  foldMap f (RecordUpdateBranch _ del) = foldMap (foldMap (foldMap f)) del

instance traversableRecordUpdate :: Traversable RecordUpdate where
  traverse f (RecordUpdateLeaf l tok e)  = RecordUpdateLeaf l tok <$> traverse f e
  traverse f (RecordUpdateBranch l del)  = RecordUpdateBranch l <$> traverse (traverse (traverse f)) del
  sequence = traverse identity

data RecordAccessor a = RecordAccessor
  { recExpr :: Expr a
  , recDot  :: SourceToken
  , recPath :: Separated Label
  }

derive instance functorRecordAccessor :: Functor RecordAccessor
derive instance eqRecordAccessor :: Eq a => Eq (RecordAccessor a)
derive instance ordRecordAccessor :: Ord a => Ord (RecordAccessor a)

instance showRecordAccessor :: Show a => Show (RecordAccessor a) where
  show _ = "<RecordAccessor>"

instance foldableRecordAccessor :: Foldable RecordAccessor where
  foldr f z (RecordAccessor { recExpr }) = foldr f z recExpr
  foldl f z (RecordAccessor { recExpr }) = foldl f z recExpr
  foldMap f (RecordAccessor { recExpr }) = foldMap f recExpr

instance traversableRecordAccessor :: Traversable RecordAccessor where
  traverse f (RecordAccessor ra) = (\e -> RecordAccessor ra { recExpr = e }) <$> traverse f ra.recExpr
  sequence = traverse identity

data Lambda a = Lambda
  { lmbSymbol  :: SourceToken
  , lmbBinders :: NonEmptyList (Binder a)
  , lmbArr     :: SourceToken
  , lmbBody    :: Expr a
  }

derive instance functorLambda :: Functor Lambda
derive instance eqLambda :: Eq a => Eq (Lambda a)
derive instance ordLambda :: Ord a => Ord (Lambda a)

instance showLambda :: Show a => Show (Lambda a) where
  show _ = "<Lambda>"

instance foldableLambda :: Foldable Lambda where
  foldr f z (Lambda { lmbBinders, lmbBody }) =
    foldr (flip (foldr f)) (foldr f z lmbBody) lmbBinders
  foldl f z (Lambda { lmbBinders, lmbBody }) =
    foldl f (foldl (foldl f) z lmbBinders) lmbBody
  foldMap f (Lambda { lmbBinders, lmbBody }) =
    foldMap (foldMap f) lmbBinders <> foldMap f lmbBody

instance traversableLambda :: Traversable Lambda where
  traverse f (Lambda lam) =
    (\bs body -> Lambda lam { lmbBinders = bs, lmbBody = body })
      <$> traverse (traverse f) lam.lmbBinders
      <*> traverse f lam.lmbBody
  sequence = traverse identity

data IfThenElse a = IfThenElse
  { iteIf    :: SourceToken
  , iteCond  :: Expr a
  , iteThen  :: SourceToken
  , iteTrue  :: Expr a
  , iteElse  :: SourceToken
  , iteFalse :: Expr a
  }

derive instance functorIfThenElse :: Functor IfThenElse
derive instance eqIfThenElse :: Eq a => Eq (IfThenElse a)
derive instance ordIfThenElse :: Ord a => Ord (IfThenElse a)

instance showIfThenElse :: Show a => Show (IfThenElse a) where
  show _ = "<IfThenElse>"

instance foldableIfThenElse :: Foldable IfThenElse where
  foldr f z (IfThenElse { iteCond, iteTrue, iteFalse }) =
    foldr f (foldr f (foldr f z iteFalse) iteTrue) iteCond
  foldl f z (IfThenElse { iteCond, iteTrue, iteFalse }) =
    foldl f (foldl f (foldl f z iteCond) iteTrue) iteFalse
  foldMap f (IfThenElse { iteCond, iteTrue, iteFalse }) =
    foldMap f iteCond <> foldMap f iteTrue <> foldMap f iteFalse

instance traversableIfThenElse :: Traversable IfThenElse where
  traverse f (IfThenElse ite) =
    (\c t e -> IfThenElse ite { iteCond = c, iteTrue = t, iteFalse = e })
      <$> traverse f ite.iteCond
      <*> traverse f ite.iteTrue
      <*> traverse f ite.iteFalse
  sequence = traverse identity

data CaseOf a = CaseOf
  { caseKeyword  :: SourceToken
  , caseHead     :: Separated (Expr a)
  , caseOf       :: SourceToken
  , caseBranches :: NonEmptyList (Tuple (Separated (Binder a)) (Guarded a))
  }

derive instance functorCaseOf :: Functor CaseOf
derive instance eqCaseOf :: Eq a => Eq (CaseOf a)
derive instance ordCaseOf :: Ord a => Ord (CaseOf a)

instance showCaseOf :: Show a => Show (CaseOf a) where
  show _ = "<CaseOf>"

instance foldableCaseOf :: Foldable CaseOf where
  foldr f z (CaseOf { caseHead, caseBranches }) =
    foldr (flip (foldr f)) (foldr (\(Tuple bs g) acc -> foldr (flip (foldr f)) (foldr f acc g) bs) z caseBranches) caseHead
  foldl f z (CaseOf { caseHead, caseBranches }) =
    foldl (\acc (Tuple bs g) -> foldl f (foldl (foldl f) acc bs) g) (foldl (foldl f) z caseHead) caseBranches
  foldMap f (CaseOf { caseHead, caseBranches }) =
    foldMap (foldMap f) caseHead <>
    foldMap (\(Tuple bs g) -> foldMap (foldMap f) bs <> foldMap f g) caseBranches

instance traversableCaseOf :: Traversable CaseOf where
  traverse f (CaseOf co) =
    (\h bs -> CaseOf co { caseHead = h, caseBranches = bs })
      <$> traverse (traverse f) co.caseHead
      <*> traverse (\(Tuple bs g) -> Tuple <$> traverse (traverse f) bs <*> traverse f g) co.caseBranches
  sequence = traverse identity

data LetIn a = LetIn
  { letKeyword  :: SourceToken
  , letBindings :: NonEmptyList (LetBinding a)
  , letIn       :: SourceToken
  , letBody     :: Expr a
  }

derive instance functorLetIn :: Functor LetIn
derive instance eqLetIn :: Eq a => Eq (LetIn a)
derive instance ordLetIn :: Ord a => Ord (LetIn a)

instance showLetIn :: Show a => Show (LetIn a) where
  show _ = "<LetIn>"

instance foldableLetIn :: Foldable LetIn where
  foldr f z (LetIn { letBindings, letBody }) =
    foldr (flip (foldr f)) (foldr f z letBody) letBindings
  foldl f z (LetIn { letBindings, letBody }) =
    foldl f (foldl (foldl f) z letBindings) letBody
  foldMap f (LetIn { letBindings, letBody }) =
    foldMap (foldMap f) letBindings <> foldMap f letBody

instance traversableLetIn :: Traversable LetIn where
  traverse f (LetIn li) =
    (\bs body -> LetIn li { letBindings = bs, letBody = body })
      <$> traverse (traverse f) li.letBindings
      <*> traverse f li.letBody
  sequence = traverse identity

data Where a = Where
  { whereExpr     :: Expr a
  , whereBindings :: Maybe (Tuple SourceToken (NonEmptyList (LetBinding a)))
  }

derive instance functorWhere :: Functor Where
derive instance eqWhere :: Eq a => Eq (Where a)
derive instance ordWhere :: Ord a => Ord (Where a)

instance showWhere :: Show a => Show (Where a) where
  show _ = "<Where>"

instance foldableWhere :: Foldable Where where
  foldr f z (Where { whereExpr, whereBindings }) =
    foldr f (foldr (\(Tuple _ bs) acc -> foldr (flip (foldr f)) acc bs) z whereBindings) whereExpr
  foldl f z (Where { whereExpr, whereBindings }) =
    foldl (\acc (Tuple _ bs) -> foldl (foldl f) acc bs) (foldl f z whereExpr) whereBindings
  foldMap f (Where { whereExpr, whereBindings }) =
    foldMap f whereExpr <>
    foldMap (\(Tuple _ bs) -> foldMap (foldMap f) bs) whereBindings

instance traversableWhere :: Traversable Where where
  traverse f (Where w) =
    (\e bs -> Where w { whereExpr = e, whereBindings = bs })
      <$> traverse f w.whereExpr
      <*> traverse (\(Tuple tok bs) -> Tuple tok <$> traverse (traverse f) bs) w.whereBindings
  sequence = traverse identity

data LetBinding a
  = LetBindingSignature a (Labeled (Name Ident) (Type a))
  | LetBindingName a (ValueBindingFields a)
  | LetBindingPattern a (Binder a) SourceToken (Where a)

derive instance functorLetBinding :: Functor LetBinding
derive instance eqLetBinding :: Eq a => Eq (LetBinding a)
derive instance ordLetBinding :: Ord a => Ord (LetBinding a)

instance showLetBinding :: Show a => Show (LetBinding a) where
  show _ = "<LetBinding>"

instance foldableLetBinding :: Foldable LetBinding where
  foldr f z (LetBindingSignature a _)      = f a z
  foldr f z (LetBindingName a _)           = f a z
  foldr f z (LetBindingPattern a _ _ _)    = f a z
  foldl f z (LetBindingSignature a _)      = f z a
  foldl f z (LetBindingName a _)           = f z a
  foldl f z (LetBindingPattern a _ _ _)    = f z a
  foldMap f (LetBindingSignature a _)      = f a
  foldMap f (LetBindingName a _)           = f a
  foldMap f (LetBindingPattern a _ _ _)    = f a

instance traversableLetBinding :: Traversable LetBinding where
  traverse f (LetBindingSignature a lab) = LetBindingSignature <$> f a <*> traverse (traverse f) lab
  traverse f (LetBindingName a vbf)      = LetBindingName <$> f a <*> traverse f vbf
  traverse f (LetBindingPattern a b tok w) =
    LetBindingPattern <$> f a <*> traverse f b <*> pure tok <*> traverse f w
  sequence = traverse identity

data DoBlock a = DoBlock
  { doKeyword    :: SourceToken
  , doStatements :: NonEmptyList (DoStatement a)
  }

derive instance functorDoBlock :: Functor DoBlock
derive instance eqDoBlock :: Eq a => Eq (DoBlock a)
derive instance ordDoBlock :: Ord a => Ord (DoBlock a)

instance showDoBlock :: Show a => Show (DoBlock a) where
  show _ = "<DoBlock>"

instance foldableDoBlock :: Foldable DoBlock where
  foldr f z (DoBlock { doStatements }) = foldr (flip (foldr f)) z doStatements
  foldl f z (DoBlock { doStatements }) = foldl (foldl f) z doStatements
  foldMap f (DoBlock { doStatements }) = foldMap (foldMap f) doStatements

instance traversableDoBlock :: Traversable DoBlock where
  traverse f (DoBlock db) = (\ss -> DoBlock db { doStatements = ss }) <$> traverse (traverse f) db.doStatements
  sequence = traverse identity

data DoStatement a
  = DoLet SourceToken (NonEmptyList (LetBinding a))
  | DoDiscard (Expr a)
  | DoBind (Binder a) SourceToken (Expr a)

derive instance functorDoStatement :: Functor DoStatement
derive instance eqDoStatement :: Eq a => Eq (DoStatement a)
derive instance ordDoStatement :: Ord a => Ord (DoStatement a)

instance showDoStatement :: Show a => Show (DoStatement a) where
  show _ = "<DoStatement>"

instance foldableDoStatement :: Foldable DoStatement where
  foldr f z (DoLet _ bs)        = foldr (flip (foldr f)) z bs
  foldr f z (DoDiscard e)       = foldr f z e
  foldr f z (DoBind b _ e)      = foldr f (foldr f z e) b
  foldl f z (DoLet _ bs)        = foldl (foldl f) z bs
  foldl f z (DoDiscard e)       = foldl f z e
  foldl f z (DoBind b _ e)      = foldl f (foldl f z b) e
  foldMap f (DoLet _ bs)        = foldMap (foldMap f) bs
  foldMap f (DoDiscard e)       = foldMap f e
  foldMap f (DoBind b _ e)      = foldMap f b <> foldMap f e

instance traversableDoStatement :: Traversable DoStatement where
  traverse f (DoLet tok bs)     = DoLet tok <$> traverse (traverse f) bs
  traverse f (DoDiscard e)      = DoDiscard <$> traverse f e
  traverse f (DoBind b tok e)   = DoBind <$> traverse f b <*> pure tok <*> traverse f e
  sequence = traverse identity

data AdoBlock a = AdoBlock
  { adoKeyword    :: SourceToken
  , adoStatements :: Array (DoStatement a)
  , adoIn         :: SourceToken
  , adoResult     :: Expr a
  }

derive instance functorAdoBlock :: Functor AdoBlock
derive instance eqAdoBlock :: Eq a => Eq (AdoBlock a)
derive instance ordAdoBlock :: Ord a => Ord (AdoBlock a)

instance showAdoBlock :: Show a => Show (AdoBlock a) where
  show _ = "<AdoBlock>"

instance foldableAdoBlock :: Foldable AdoBlock where
  foldr f z (AdoBlock { adoStatements, adoResult }) =
    foldr (flip (foldr f)) (foldr f z adoResult) adoStatements
  foldl f z (AdoBlock { adoStatements, adoResult }) =
    foldl f (foldl (foldl f) z adoStatements) adoResult
  foldMap f (AdoBlock { adoStatements, adoResult }) =
    foldMap (foldMap f) adoStatements <> foldMap f adoResult

instance traversableAdoBlock :: Traversable AdoBlock where
  traverse f (AdoBlock ab) =
    (\ss r -> AdoBlock ab { adoStatements = ss, adoResult = r })
      <$> traverse (traverse f) ab.adoStatements
      <*> traverse f ab.adoResult
  sequence = traverse identity

-- -----------------------------------------------------------------------
-- Binders
-- -----------------------------------------------------------------------

data Binder a
  = BinderWildcard a SourceToken
  | BinderVar a (Name Ident)
  | BinderNamed a (Name Ident) SourceToken (Binder a)
  | BinderConstructor a (QualifiedName (ProperName ConstructorName)) (Array (Binder a))
  | BinderBoolean a SourceToken Boolean
  | BinderChar a SourceToken Char
  | BinderString a SourceToken PSString
  | BinderNumber a (Maybe SourceToken) SourceToken (Either Int Number)
  | BinderArray a (Delimited (Binder a))
  | BinderRecord a (Delimited (RecordLabeled (Binder a)))
  | BinderParens a (Wrapped (Binder a))
  | BinderTyped a (Binder a) SourceToken (Type a)
  | BinderOp a (Binder a) (QualifiedName (OpName ValueOpName)) (Binder a)

derive instance functorBinder :: Functor Binder
derive instance eqBinder :: Eq a => Eq (Binder a)
derive instance ordBinder :: Ord a => Ord (Binder a)

instance showBinder :: Show a => Show (Binder a) where
  show _ = "<CSTBinder>"

instance foldableBinder :: Foldable Binder where
  foldr f z b = f (binderAnn b) z
  foldl f z b = f z (binderAnn b)
  foldMap f b = f (binderAnn b)

instance traversableBinder :: Traversable Binder where
  traverse f (BinderWildcard a tok)       = BinderWildcard <$> f a <*> pure tok
  traverse f (BinderVar a n)              = BinderVar <$> f a <*> pure n
  traverse f (BinderNamed a n tok b)      = BinderNamed <$> f a <*> pure n <*> pure tok <*> traverse f b
  traverse f (BinderConstructor a q bs)   = BinderConstructor <$> f a <*> pure q <*> traverse (traverse f) bs
  traverse f (BinderBoolean a tok b)      = BinderBoolean <$> f a <*> pure tok <*> pure b
  traverse f (BinderChar a tok c)         = BinderChar <$> f a <*> pure tok <*> pure c
  traverse f (BinderString a tok ps)      = BinderString <$> f a <*> pure tok <*> pure ps
  traverse f (BinderNumber a mt tok n)    = BinderNumber <$> f a <*> pure mt <*> pure tok <*> pure n
  traverse f (BinderArray a del)          = BinderArray <$> f a <*> traverse (traverse (traverse (traverse f))) del
  traverse f (BinderRecord a del)         = BinderRecord <$> f a <*> traverse (traverse (traverse (traverse (traverse f)))) del
  traverse f (BinderParens a w)           = BinderParens <$> f a <*> traverse (traverse f) w
  traverse f (BinderTyped a b tok ty)     = BinderTyped <$> f a <*> traverse f b <*> pure tok <*> traverse f ty
  traverse f (BinderOp a b1 q b2)         = BinderOp <$> f a <*> traverse f b1 <*> pure q <*> traverse f b2
  sequence = traverse identity

binderAnn :: forall a. Binder a -> a
binderAnn (BinderWildcard a _)       = a
binderAnn (BinderVar a _)            = a
binderAnn (BinderNamed a _ _ _)      = a
binderAnn (BinderConstructor a _ _)  = a
binderAnn (BinderBoolean a _ _)      = a
binderAnn (BinderChar a _ _)         = a
binderAnn (BinderString a _ _)       = a
binderAnn (BinderNumber a _ _ _)     = a
binderAnn (BinderArray a _)          = a
binderAnn (BinderRecord a _)         = a
binderAnn (BinderParens a _)         = a
binderAnn (BinderTyped a _ _ _)      = a
binderAnn (BinderOp a _ _ _)         = a
