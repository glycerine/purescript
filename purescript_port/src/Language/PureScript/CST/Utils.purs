module Language.PureScript.CST.Utils
  ( QualifiedProperName(..)
  , qualifiedProperName
  , ProperName(..)
  , properName
  , getProperName
  , getQualifiedProperName
  , QualifiedOpName(..)
  , qualifiedOpName
  , getQualifiedOpName
  , OpName(..)
  , opName
  , getOpName
  , lblTok
  , placeholder
  , unexpectedName
  , unexpectedQual
  , unexpectedLabel
  , unexpectedExpr
  , unexpectedBinder
  , unexpectedRecordUpdate
  , unexpectedRecordLabeled
  , rangeToks
  , unexpectedToks
  , separated
  , internalError
  , toModuleName
  , upperToModuleName
  , toQualifiedName
  , toName
  , toLabel
  , toString
  , toChar
  , toNumber
  , toInt
  , toBoolean
  , toConstraint
  , isConstrained
  , toBinderConstructor
  , toRecordFields
  , checkFundeps
  , TmpModuleDecl(..)
  , toModuleDecls
  , checkNoWildcards
  , checkNoForalls
  , revert
  , reservedNames
  , isValidModuleNamespace
  , isLeftFatArrow
  ) where

import Prelude

import Data.Array (all, cons, elem, head, null, reverse, snoc, uncons) as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_) as Foldable
import Data.List (List(..), null, toUnfoldable) as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (head, tail) as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set (fromFoldable, member) as Set
import Data.String (contains, joinWith, Pattern(..)) as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Language.PureScript.CST.Errors (ParserErrorType(..))
import Language.PureScript.CST.Monad (Parser, addFailure, parseFail, pushBack)
import Language.PureScript.CST.Positions
  ( TokenRange
  , binderRange
  , importDeclRange
  , recordUpdateRange
  , typeRange
  )
import Language.PureScript.CST.Traversals.Type (everythingOnTypes)
import Language.PureScript.CST.Types
  ( Binder(..)
  , ClassFundep(..)
  , ClassHead(..)
  , Constraint(..)
  , CSTSourcePos(..)
  , Declaration(..)
  , Expr(..)
  , Ident(..)
  , ImportDecl(..)
  , Instance(..)
  , InstanceHead(..)
  , Label(..)
  , Labeled(..)
  , Name(..)
  , OneOrDelimited(..)
  , QualifiedName(..)
  , RecordLabeled(..)
  , RecordUpdate(..)
  , Separated(..)
  , SourceRange(..)
  , SourceStyle(..)
  , SourceToken(..)
  , Token(..)
  , TokenAnn(..)
  , Type(..)
  , TypeVarBinding(..)
  , Wrapped(..)
  )
import Language.PureScript.Names
  ( ClassName
  , ConstructorName
  , ModuleName(..)
  , OpName(..) -- Names OpName
  , ProperName(..) -- Names ProperName
  , TypeName
  , TypeOpName
  , coerceOpName
  , coerceProperName
  ) as N
import Language.PureScript.PSString (PSString, mkString)
import Unsafe.Coerce (unsafeCoerce)

-- | A newtype for a qualified proper name whose ProperNameType has not yet been determined.
newtype QualifiedProperName = QualifiedProperName (QualifiedName (N.ProperName N.TypeName))

qualifiedProperName :: forall a. QualifiedName (N.ProperName a) -> QualifiedProperName
qualifiedProperName n = QualifiedProperName (unsafeCoerce n)

getQualifiedProperName :: forall a. QualifiedProperName -> QualifiedName (N.ProperName a)
getQualifiedProperName (QualifiedProperName n) = unsafeCoerce n

-- | A newtype for a proper name whose ProperNameType has not yet been determined.
newtype ProperName = ProperName (Name (N.ProperName N.TypeName))

properName :: forall a. Name (N.ProperName a) -> ProperName
properName n = ProperName (unsafeCoerce n)

getProperName :: forall a. ProperName -> Name (N.ProperName a)
getProperName (ProperName n) = unsafeCoerce n

-- | A newtype for a qualified operator name whose OpNameType has not yet been determined.
newtype QualifiedOpName = QualifiedOpName (QualifiedName (N.OpName N.TypeOpName))

qualifiedOpName :: forall a. QualifiedName (N.OpName a) -> QualifiedOpName
qualifiedOpName n = QualifiedOpName (unsafeCoerce n)

getQualifiedOpName :: forall a. QualifiedOpName -> QualifiedName (N.OpName a)
getQualifiedOpName (QualifiedOpName n) = unsafeCoerce n

-- | A newtype for an operator name whose OpNameType has not yet been determined.
newtype OpName = OpName (Name (N.OpName N.TypeOpName))

opName :: forall a. Name (N.OpName a) -> OpName
opName n = OpName (unsafeCoerce n)

getOpName :: forall a. OpName -> Name (N.OpName a)
getOpName (OpName n) = unsafeCoerce n

lblTok :: Label -> SourceToken
lblTok (Label { lblTok: t }) = t

placeholder :: SourceToken
placeholder = SourceToken
  { tokAnn: TokenAnn
      { tokRange: SourceRange
          { srcStart: CSTSourcePos { srcLine: 0, srcColumn: 0 }
          , srcEnd:   CSTSourcePos { srcLine: 0, srcColumn: 0 }
          }
      , tokLeadingComments: []
      , tokTrailingComments: []
      }
  , tokValue: TokLowerName [] "<placeholder>"
  }

unexpectedName :: SourceToken -> Name Ident
unexpectedName tok = Name { nameTok: tok, nameValue: Ident "<unexpected>" }

unexpectedQual :: SourceToken -> QualifiedName Ident
unexpectedQual tok = QualifiedName
  { qualTok: tok
  , qualModule: Nothing
  , qualName: Ident "<unexpected>"
  }

unexpectedLabel :: SourceToken -> Label
unexpectedLabel tok = Label { lblTok: tok, lblName: mkString "<unexpected>" }

unexpectedExpr :: forall a. Monoid a => Array SourceToken -> Expr a
unexpectedExpr toks =
  ExprIdent mempty (unexpectedQual (fromMaybe placeholder (Array.head toks)))

unexpectedBinder :: forall a. Monoid a => Array SourceToken -> Binder a
unexpectedBinder toks =
  BinderVar mempty (unexpectedName (fromMaybe placeholder (Array.head toks)))

unexpectedRecordUpdate :: forall a. Monoid a => Array SourceToken -> RecordUpdate a
unexpectedRecordUpdate toks =
  let tok = fromMaybe placeholder (Array.head toks)
  in RecordUpdateLeaf (unexpectedLabel tok) tok (unexpectedExpr toks)

unexpectedRecordLabeled :: forall a. Array SourceToken -> RecordLabeled a
unexpectedRecordLabeled toks =
  RecordPun (unexpectedName (fromMaybe placeholder (Array.head toks)))

rangeToks :: TokenRange -> Array SourceToken
rangeToks (Tuple a b) = [a, b]

unexpectedToks
  :: forall a b
   . (a -> TokenRange)
  -> (Array SourceToken -> b)
  -> ParserErrorType
  -> a
  -> Parser b
unexpectedToks toRange toCst err old = do
  let toks = rangeToks (toRange old)
  addFailure toks err
  pure (toCst toks)

-- | Build a Separated from a reversed list of (separator, value) pairs.
-- The last element's separator is discarded and it becomes the sepHead.
separated :: forall a. Array (Tuple SourceToken a) -> Separated a
separated = go []
  where
  go accum arr = case Array.uncons arr of
    Nothing -> internalError "Separated should not be empty"
    Just { head: h@(Tuple _ a), tail: rest } ->
      if Array.null rest
        then Separated { sepHead: a, sepTail: accum }
        else go (Array.cons h accum) rest

internalError :: forall a. String -> a
internalError msg = unsafeCoerce (unit)  -- Will be replaced with FFI throw

toModuleName :: SourceToken -> Array String -> Parser (Maybe N.ModuleName)
toModuleName _ [] = pure Nothing
toModuleName tok ns = do
  unless (Array.all isValidModuleNamespace ns) $ addFailure [tok] ErrModuleName
  pure <<< Just <<< N.ModuleName $ String.joinWith "." ns

upperToModuleName :: SourceToken -> Parser (Name N.ModuleName)
upperToModuleName tok = case tok of
  SourceToken { tokValue: TokUpperName q a } -> do
    let ns = Array.snoc q a
    unless (Array.all isValidModuleNamespace ns) $ addFailure [tok] ErrModuleName
    pure (Name { nameTok: tok, nameValue: N.ModuleName (String.joinWith "." ns) })
  _ -> internalError ("Invalid upper name: " <> show tok)

toQualifiedName :: forall a. (String -> a) -> SourceToken -> Parser (QualifiedName a)
toQualifiedName k tok = case tok of
  SourceToken { tokValue: TokLowerName q a } ->
    if not (Set.member a reservedNames)
      then map (\m -> QualifiedName { qualTok: tok, qualModule: m, qualName: k a }) (toModuleName tok q)
      else do
        addFailure [tok] ErrKeywordVar
        pure (QualifiedName { qualTok: tok, qualModule: Nothing, qualName: k "<unexpected>" })
  SourceToken { tokValue: TokUpperName q a } ->
    map (\m -> QualifiedName { qualTok: tok, qualModule: m, qualName: k a }) (toModuleName tok q)
  SourceToken { tokValue: TokSymbolName q a } ->
    map (\m -> QualifiedName { qualTok: tok, qualModule: m, qualName: k a }) (toModuleName tok q)
  SourceToken { tokValue: TokOperator q a } ->
    map (\m -> QualifiedName { qualTok: tok, qualModule: m, qualName: k a }) (toModuleName tok q)
  _ -> internalError ("Invalid qualified name: " <> show tok)

toName :: forall a. (String -> a) -> SourceToken -> Parser (Name a)
toName k tok = case tok of
  SourceToken { tokValue: TokLowerName [] a } ->
    if not (Set.member a reservedNames)
      then pure (Name { nameTok: tok, nameValue: k a })
      else do
        addFailure [tok] ErrKeywordVar
        pure (Name { nameTok: tok, nameValue: k "<unexpected>" })
  SourceToken { tokValue: TokString _ _ } -> parseFail tok ErrQuotedPun
  SourceToken { tokValue: TokRawString _ } -> parseFail tok ErrQuotedPun
  SourceToken { tokValue: TokUpperName [] a }  -> pure (Name { nameTok: tok, nameValue: k a })
  SourceToken { tokValue: TokSymbolName [] a } -> pure (Name { nameTok: tok, nameValue: k a })
  SourceToken { tokValue: TokOperator [] a }   -> pure (Name { nameTok: tok, nameValue: k a })
  SourceToken { tokValue: TokHole a }          -> pure (Name { nameTok: tok, nameValue: k a })
  _ -> internalError ("Invalid name: " <> show tok)

toLabel :: SourceToken -> Label
toLabel tok = case tok of
  SourceToken { tokValue: TokLowerName [] a } -> Label { lblTok: tok, lblName: mkString a }
  SourceToken { tokValue: TokString _ a }     -> Label { lblTok: tok, lblName: a }
  SourceToken { tokValue: TokRawString a }    -> Label { lblTok: tok, lblName: mkString a }
  SourceToken { tokValue: TokForall ASCII }   -> Label { lblTok: tok, lblName: mkString "forall" }
  _ -> internalError ("Invalid label: " <> show tok)

toString :: SourceToken -> Tuple SourceToken PSString
toString tok = case tok of
  SourceToken { tokValue: TokString _ a }  -> Tuple tok a
  SourceToken { tokValue: TokRawString a } -> Tuple tok (mkString a)
  _ -> internalError ("Invalid string literal: " <> show tok)

toChar :: SourceToken -> Tuple SourceToken Char
toChar tok = case tok of
  SourceToken { tokValue: TokChar _ a } -> Tuple tok a
  _ -> internalError ("Invalid char literal: " <> show tok)

toNumber :: SourceToken -> Tuple SourceToken (Either Int Number)
toNumber tok = case tok of
  SourceToken { tokValue: TokInt _ a }    -> Tuple tok (Left a)
  SourceToken { tokValue: TokNumber _ a } -> Tuple tok (Right a)
  _ -> internalError ("Invalid number literal: " <> show tok)

toInt :: SourceToken -> Tuple SourceToken Int
toInt tok = case tok of
  SourceToken { tokValue: TokInt _ a } -> Tuple tok a
  _ -> internalError ("Invalid integer literal: " <> show tok)

toBoolean :: SourceToken -> Tuple SourceToken Boolean
toBoolean tok = case tok of
  SourceToken { tokValue: TokLowerName [] "true"  } -> Tuple tok true
  SourceToken { tokValue: TokLowerName [] "false" } -> Tuple tok false
  _ -> internalError ("Invalid boolean literal: " <> show tok)

toConstraint :: forall a. Monoid a => Type a -> Parser (Constraint a)
toConstraint = convertParens
  where
  convertParens :: Type a -> Parser (Constraint a)
  convertParens ty = case ty of
    TypeParens a (Wrapped w) -> do
      c' <- convertParens w.wrpValue
      pure (ConstraintParens a (Wrapped w { wrpValue = c' }))
    _ -> convert mempty [] ty

  convert :: a -> Array (Type a) -> Type a -> Parser (Constraint a)
  convert ann acc ty = case ty of
    TypeApp a lhs rhs -> convert (a <> ann) (Array.cons rhs acc) lhs
    TypeConstructor a name -> do
      Foldable.traverse_ checkNoForalls acc
      pure (Constraint (a <> ann) (unsafeCoerce name) acc)
    _ ->
      let Tuple tok1 tok2 = typeRange ty
      in do
        addFailure [tok1, tok2] ErrTypeInConstraint
        pure (Constraint mempty
          (QualifiedName { qualTok: tok1, qualModule: Nothing, qualName: N.ProperName "<unexpected" })
          [])

isConstrained :: forall a. Type a -> Boolean
isConstrained = everythingOnTypes (||) isConst
  where
  isConst (TypeConstrained _ _ _ _) = true
  isConst _ = false

toBinderConstructor :: forall a. Monoid a => NonEmptyList (Binder a) -> Parser (Binder a)
toBinderConstructor nel =
  let h = NEL.head nel
      t = NEL.tail nel
  in case h of
    BinderConstructor a name [] ->
      pure (BinderConstructor a name (List.toUnfoldable t))
    _ | List.null t -> pure h
    _ -> unexpectedToks binderRange unexpectedBinder ErrExprInBinder h

toRecordFields
  :: forall a
   . Monoid a
  => Separated (Either (RecordLabeled (Expr a)) (RecordUpdate a))
  -> Parser (Either (Separated (RecordLabeled (Expr a))) (Separated (RecordUpdate a)))
toRecordFields (Separated { sepHead, sepTail }) = case sepHead of
  Left a -> do
    tail' <- traverse (traverse unLeft) sepTail
    pure (Left (Separated { sepHead: a, sepTail: tail' }))
  Right a -> do
    tail' <- traverse (traverse unRight) sepTail
    pure (Right (Separated { sepHead: a, sepTail: tail' }))
  where
  unLeft (Left tok)  = pure tok
  unLeft (Right tok) =
    unexpectedToks recordUpdateRange unexpectedRecordLabeled ErrRecordUpdateInCtr tok

  unRight (Right tok) = pure tok
  unRight (Left (RecordPun (Name { nameTok: tok }))) = do
    addFailure [tok] ErrRecordPunInUpdate
    pure (unexpectedRecordUpdate [tok])
  unRight (Left (RecordField _ tok _)) = do
    addFailure [tok] ErrRecordCtrInUpdate
    pure (unexpectedRecordUpdate [tok])

checkFundeps :: forall a. ClassHead a -> Parser Unit
checkFundeps (ClassHead ch) = case ch.clsFundeps of
  Nothing -> pure unit
  Just (Tuple _ fundeps) -> do
    let
      varName (TypeVarKinded (Wrapped w)) =
        case w.wrpValue of
          Labeled { lblLabel: Tuple _ n } -> case n of Name { nameValue: Ident getIdent } -> getIdent
      varName (TypeVarName (Tuple _ n)) = case n of Name { nameValue: Ident getIdent } -> getIdent
      names = map varName ch.clsVars
      check n = case n of
        Name { nameTok: tok, nameValue: Ident getIdent } ->
          unless (Array.elem getIdent names) (addFailure [tok] ErrUnknownFundep)
    Foldable.traverse_ (checkFundep check) fundeps
  where
  checkFundep check fd = case fd of
    FundepDetermined _ bs -> Foldable.traverse_ check bs
    FundepDetermines as _ bs -> do
      Foldable.traverse_ check as
      Foldable.traverse_ check bs

data TmpModuleDecl a
  = TmpImport (ImportDecl a)
  | TmpChain (Separated (Declaration a))

toModuleDecls
  :: forall a
   . Monoid a
  => Array (TmpModuleDecl a)
  -> Parser (Tuple (Array (ImportDecl a)) (Array (Declaration a)))
toModuleDecls = goImport []
  where
  goImport acc arr = case Array.uncons arr of
    Just { head: TmpImport x, tail: xs } -> goImport (Array.cons x acc) xs
    _ -> map (Tuple (Array.reverse acc)) (goDecl [] arr)

  goDecl acc arr = case Array.uncons arr of
    Nothing -> pure (Array.reverse acc)
    Just { head: TmpChain (Separated { sepHead: x, sepTail: [] }), tail: xs } ->
      goDecl (Array.cons x acc) xs
    Just { head: TmpChain (Separated { sepHead: DeclInstanceChain a (Separated { sepHead: h, sepTail: t }), sepTail: t' }), tail: xs } -> do
      Tuple a' instances <- goChain (getInstName h) a [] t'
      goDecl (Array.cons (DeclInstanceChain a' (Separated { sepHead: h, sepTail: t <> instances })) acc) xs
    Just { head: TmpChain (Separated { sepTail: t }), tail: xs } -> do
      traverse_ (\(Tuple tok _) -> addFailure [tok] ErrElseInDecl) t
      goDecl acc xs
    Just { head: TmpImport imp, tail: xs } -> do
      _ <- unexpectedToks importDeclRange (const unit) ErrImportInDecl imp
      goDecl acc xs

  goChain _ ann acc [] = pure (Tuple ann (Array.reverse acc))
  goChain name ann acc arr = case Array.uncons arr of
    Nothing -> pure (Tuple ann (Array.reverse acc))
    Just { head: Tuple tok (DeclInstanceChain a (Separated { sepHead: h, sepTail: t })), tail: xs } ->
      if eqInstName (getInstName h) name
        then goChain name (ann <> a) (Array.reverse (Array.cons (Tuple tok h) t) <> acc) xs
        else do
          addFailure [qualTok (getInstName h)] ErrInstanceNameMismatch
          goChain name ann acc xs
    Just { head: Tuple tok _, tail: xs } -> do
      addFailure [tok] ErrElseInDecl
      goChain name ann acc xs

  getInstName inst = case inst of
    Instance { instHead: InstanceHead { instClass } } -> instClass

  eqInstName (QualifiedName { qualModule: m1, qualName: n1 }) (QualifiedName { qualModule: m2, qualName: n2 }) =
    m1 == m2 && n1 == n2

  qualTok (QualifiedName { qualTok: t }) = t

  traverse_ :: forall f b. Applicative f => (b -> f Unit) -> Array b -> f Unit
  traverse_ f arr = case Array.uncons arr of
    Nothing -> pure unit
    Just { head: x, tail: xs } -> f x *> traverse_ f xs

checkNoWildcards :: forall a. Type a -> Parser Unit
checkNoWildcards ty = do
  let checks = everythingOnTypes (<>) checkTy ty
  traverse_ identity checks
  where
  checkTy (TypeWildcard _ a) = [addFailure [a] ErrWildcardInType]
  checkTy (TypeHole _ a)     = [addFailure [case a of Name { nameTok: t } -> t] ErrHoleInType]
  checkTy _                  = []

  traverse_ :: forall f b. Applicative f => (b -> f Unit) -> Array b -> f Unit
  traverse_ f arr = case Array.uncons arr of
    Nothing -> pure unit
    Just { head: x, tail: xs } -> f x *> traverse_ f xs

checkNoForalls :: forall a. Type a -> Parser Unit
checkNoForalls ty = do
  let checks = everythingOnTypes (<>) checkTy ty
  traverse_ identity checks
  where
  checkTy (TypeForall _ a _ _ _) = [addFailure [a] ErrToken]
  checkTy _                      = []

  traverse_ :: forall f b. Applicative f => (b -> f Unit) -> Array b -> f Unit
  traverse_ f arr = case Array.uncons arr of
    Nothing -> pure unit
    Just { head: x, tail: xs } -> f x *> traverse_ f xs

revert :: forall a. Parser a -> SourceToken -> Parser a
revert p lk = pushBack lk *> p

reservedNames :: Set String
reservedNames = Set.fromFoldable
  [ "ado"
  , "case"
  , "class"
  , "data"
  , "derive"
  , "do"
  , "else"
  , "false"
  , "forall"
  , "foreign"
  , "import"
  , "if"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "true"
  , "type"
  , "where"
  ]

isValidModuleNamespace :: String -> Boolean
isValidModuleNamespace s =
  not (String.contains (String.Pattern "_") s || String.contains (String.Pattern "'") s)

isLeftFatArrow :: String -> Boolean
isLeftFatArrow str = str == "<=" || str == "⇐"
