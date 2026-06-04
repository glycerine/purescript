-- | Utilities for calculating positions and ranges from CST nodes.
module Language.PureScript.CST.Positions
  ( advanceToken
  , advanceLeading
  , advanceTrailing
  , tokenDelta
  , qualDelta
  , multiLine
  , commentDelta
  , lineDelta
  , textDelta
  , applyDelta
  , sepLast
  , TokenRange
  , toSourceRange
  , widen
  , srcRange
  , nameRange
  , qualRange
  , wrappedRange
  , moduleRange
  , exportRange
  , importDeclRange
  , importRange
  , dataMembersRange
  , declRange
  , dataHeadRange
  , dataCtorRange
  , classHeadRange
  , classFundepRange
  , instanceRange
  , instanceHeadRange
  , instanceBindingRange
  , foreignRange
  , valueBindingFieldsRange
  , guardedRange
  , guardedExprRange
  , whereRange
  , typeRange
  , constraintRange
  , typeVarBindingRange
  , exprRange
  , letBindingRange
  , doStatementRange
  , binderRange
  , recordUpdateRange
  ) where

import Prelude

import Data.Array (last) as Array
import Data.Foldable (foldl)
import Data.List.NonEmpty (head, last) as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (toCharArray, length) as SCU
import Data.Tuple (Tuple(..), fst, snd)
import Data.Void (Void)
import Language.PureScript.CST.Types
  ( AdoBlock(..)
  , Binder(..)
  , CSTSourcePos(..)
  , CaseOf(..)
  , ClassFundep(..)
  , ClassHead(..)
  , Comment(..)
  , Constraint(..)
  , DataCtor(..)
  , DataHead(..)
  , DataMembers(..)
  , Declaration(..)
  , DoBlock(..)
  , DoStatement(..)
  , Export(..)
  , Expr(..)
  , FixityFields(..)
  , FixityOp(..)
  , Foreign(..)
  , Guarded(..)
  , GuardedExpr(..)
  , IfThenElse(..)
  , Import(..)
  , ImportDecl(..)
  , Instance(..)
  , InstanceBinding(..)
  , InstanceHead(..)
  , Label(..)
  , Labeled(..)
  , Lambda(..)
  , LetBinding(..)
  , LetIn(..)
  , LineFeed(..)
  , Module(..)
  , Name(..)
  , QualifiedName(..)
  , RecordAccessor(..)
  , RecordUpdate(..)
  , Role(..)
  , Separated(..)
  , SourceRange(..)
  , SourceStyle(..)
  , SourceToken(..)
  , Token(..)
  , TokenAnn(..)
  , Type(..)
  , TypeVarBinding(..)
  , ValueBindingFields(..)
  , Where(..)
  , Wrapped(..)
  )

advanceToken :: CSTSourcePos -> Token -> CSTSourcePos
advanceToken pos tok = applyDelta pos (tokenDelta tok)

advanceLeading :: CSTSourcePos -> Array (Comment LineFeed) -> CSTSourcePos
advanceLeading = foldl (\acc c -> applyDelta acc (commentDelta lineDelta c))

advanceTrailing :: CSTSourcePos -> Array (Comment Void) -> CSTSourcePos
advanceTrailing = foldl (\acc c -> applyDelta acc (commentDelta (\_ -> Tuple 0 0) c))

tokenDelta :: Token -> Tuple Int Int
tokenDelta tok = case tok of
  TokLeftParen             -> Tuple 0 1
  TokRightParen            -> Tuple 0 1
  TokLeftBrace             -> Tuple 0 1
  TokRightBrace            -> Tuple 0 1
  TokLeftSquare            -> Tuple 0 1
  TokRightSquare           -> Tuple 0 1
  TokLeftArrow ASCII       -> Tuple 0 2
  TokLeftArrow Unicode     -> Tuple 0 1
  TokRightArrow ASCII      -> Tuple 0 2
  TokRightArrow Unicode    -> Tuple 0 1
  TokRightFatArrow ASCII   -> Tuple 0 2
  TokRightFatArrow Unicode -> Tuple 0 1
  TokDoubleColon ASCII     -> Tuple 0 2
  TokDoubleColon Unicode   -> Tuple 0 1
  TokForall ASCII          -> Tuple 0 6
  TokForall Unicode        -> Tuple 0 1
  TokEquals                -> Tuple 0 1
  TokPipe                  -> Tuple 0 1
  TokTick                  -> Tuple 0 1
  TokDot                   -> Tuple 0 1
  TokComma                 -> Tuple 0 1
  TokUnderscore            -> Tuple 0 1
  TokBackslash             -> Tuple 0 1
  TokLowerName qual name   -> Tuple 0 (qualDelta qual + SCU.length name)
  TokUpperName qual name   -> Tuple 0 (qualDelta qual + SCU.length name)
  TokOperator qual sym     -> Tuple 0 (qualDelta qual + SCU.length sym)
  TokSymbolName qual sym   -> Tuple 0 (qualDelta qual + SCU.length sym + 2)
  TokSymbolArr Unicode     -> Tuple 0 3
  TokSymbolArr ASCII       -> Tuple 0 4
  TokHole hole             -> Tuple 0 (SCU.length hole + 1)
  TokChar raw _            -> Tuple 0 (SCU.length raw + 2)
  TokInt raw _             -> Tuple 0 (SCU.length raw)
  TokNumber raw _          -> Tuple 0 (SCU.length raw)
  TokString raw _          -> multiLine 1 (textDelta raw)
  TokRawString raw         -> multiLine 3 (textDelta raw)
  TokLayoutStart           -> Tuple 0 0
  TokLayoutSep             -> Tuple 0 0
  TokLayoutEnd             -> Tuple 0 0
  TokEof                   -> Tuple 0 0

qualDelta :: Array String -> Int
qualDelta = foldl (\acc s -> acc + SCU.length s + 1) 0

multiLine :: Int -> Tuple Int Int -> Tuple Int Int
multiLine n (Tuple 0 c) = Tuple 0 (c + n + n)
multiLine n (Tuple l c) = Tuple l (c + n)

commentDelta :: forall a. (a -> Tuple Int Int) -> Comment a -> Tuple Int Int
commentDelta k c = case c of
  Comment raw -> textDelta raw
  Space n     -> Tuple 0 n
  Line a      -> k a

lineDelta :: LineFeed -> Tuple Int Int
lineDelta _ = Tuple 1 1

textDelta :: String -> Tuple Int Int
textDelta s = foldl go (Tuple 0 0) (SCU.toCharArray s)
  where
  go (Tuple l _) '\n' = Tuple (l + 1) 1
  go (Tuple l c) _    = Tuple l (c + 1)

applyDelta :: CSTSourcePos -> Tuple Int Int -> CSTSourcePos
applyDelta (CSTSourcePos p) (Tuple 0 n) = CSTSourcePos p { srcColumn = p.srcColumn + n }
applyDelta (CSTSourcePos p) (Tuple k d) = CSTSourcePos { srcLine: p.srcLine + k, srcColumn: d }

sepLast :: forall a. Separated a -> a
sepLast (Separated { sepHead, sepTail }) = case Array.last sepTail of
  Nothing          -> sepHead
  Just (Tuple _ a) -> a

type TokenRange = Tuple SourceToken SourceToken

toSourceRange :: TokenRange -> SourceRange
toSourceRange (Tuple a b) = widen (srcRange a) (srcRange b)

widen :: SourceRange -> SourceRange -> SourceRange
widen (SourceRange { srcStart: s1 }) (SourceRange { srcEnd: e2 }) =
  SourceRange { srcStart: s1, srcEnd: e2 }

srcRange :: SourceToken -> SourceRange
srcRange (SourceToken { tokAnn: TokenAnn { tokRange } }) = tokRange

nameRange :: forall a. Name a -> TokenRange
nameRange (Name { nameTok }) = Tuple nameTok nameTok

qualRange :: forall a. QualifiedName a -> TokenRange
qualRange (QualifiedName { qualTok }) = Tuple qualTok qualTok

wrappedRange :: forall a. Wrapped a -> TokenRange
wrappedRange (Wrapped { wrpOpen, wrpClose }) = Tuple wrpOpen wrpClose

moduleRange :: forall a. Module a -> TokenRange
moduleRange (Module m) =
  case Array.last m.modDecls of
    Just d  -> Tuple m.modKeyword (snd (declRange d))
    Nothing -> case Array.last m.modImports of
      Just i  -> Tuple m.modKeyword (snd (importDeclRange i))
      Nothing -> Tuple m.modKeyword m.modWhere

exportRange :: forall a. Export a -> TokenRange
exportRange e = case e of
  ExportValue _ a         -> nameRange a
  ExportOp _ a            -> nameRange a
  ExportType _ a (Just b) -> Tuple (getTok a) (snd (dataMembersRange b))
  ExportType _ a Nothing  -> nameRange a
  ExportTypeOp _ a b      -> Tuple a (getTok b)
  ExportClass _ a b       -> Tuple a (getTok b)
  ExportModule _ a b      -> Tuple a (getTok b)
  where
  getTok :: forall x. Name x -> SourceToken
  getTok (Name { nameTok: t }) = t

importDeclRange :: forall a. ImportDecl a -> TokenRange
importDeclRange (ImportDecl imp) =
  case imp.impQual of
    Just (Tuple _ modName) -> Tuple imp.impKeyword (getTok modName)
    Nothing -> case imp.impNames of
      Just (Tuple _ imports) -> Tuple imp.impKeyword (getClose imports)
      Nothing -> Tuple imp.impKeyword (getTok imp.impModule)
  where
  getTok (Name { nameTok: t }) = t
  getClose (Wrapped { wrpClose: c }) = c

importRange :: forall a. Import a -> TokenRange
importRange im = case im of
  ImportValue _ a         -> nameRange a
  ImportOp _ a            -> nameRange a
  ImportType _ a (Just b) -> Tuple (getTok a) (snd (dataMembersRange b))
  ImportType _ a Nothing  -> nameRange a
  ImportTypeOp _ a b      -> Tuple a (getTok b)
  ImportClass _ a b       -> Tuple a (getTok b)
  where
  getTok :: forall x. Name x -> SourceToken
  getTok (Name { nameTok: t }) = t

dataMembersRange :: forall a. DataMembers a -> TokenRange
dataMembersRange dm = case dm of
  DataAll _ a                                     -> Tuple a a
  DataEnumerated _ (Wrapped { wrpOpen: a, wrpClose: b }) -> Tuple a b

declRange :: forall a. Declaration a -> TokenRange
declRange d = case d of
  DeclData _ hd (Just (Tuple _ cs)) ->
    Tuple (fst (dataHeadRange hd)) (snd (dataCtorRange (sepLast cs)))
  DeclData _ hd Nothing ->
    dataHeadRange hd
  DeclType _ a _ b ->
    Tuple (fst (dataHeadRange a)) (snd (typeRange b))
  DeclNewtype _ a _ _ b ->
    Tuple (fst (dataHeadRange a)) (snd (typeRange b))
  DeclClass _ hd (Just (Tuple _ ts)) ->
    Tuple (fst (classHeadRange hd)) (snd (typeRange (getVal (NEL.last ts))))
  DeclClass _ hd Nothing ->
    classHeadRange hd
  DeclInstanceChain _ a ->
    Tuple (fst (instanceRange (sepHead a))) (snd (instanceRange (sepLast a)))
  DeclDerive _ a _ b ->
    Tuple a (snd (instanceHeadRange b))
  DeclKindSignature _ a (Labeled { lblValue: b }) ->
    Tuple a (snd (typeRange b))
  DeclSignature _ (Labeled { lblLabel: a, lblValue: b }) ->
    Tuple (getTok a) (snd (typeRange b))
  DeclValue _ a -> valueBindingFieldsRange a
  DeclFixity _ (FixityFields { fxtKeyword: Tuple a _, fxtOp: FixityValue _ _ b }) ->
    Tuple a (getTok b)
  DeclFixity _ (FixityFields { fxtKeyword: Tuple a _, fxtOp: FixityType _ _ _ b }) ->
    Tuple a (getTok b)
  DeclForeign _ a _ b ->
    Tuple a (snd (foreignRange b))
  DeclRole _ a _ _ b ->
    Tuple a (roleTokOf (NEL.last b))
  where
  getTok :: forall x. Name x -> SourceToken
  getTok (Name { nameTok: t }) = t
  getVal (Labeled { lblValue: v }) = v
  sepHead (Separated { sepHead: h }) = h
  roleTokOf (Role { roleTok: t }) = t

dataHeadRange :: forall a. DataHead a -> TokenRange
dataHeadRange (DataHead { dataHdKeyword: kw, dataHdName: name, dataHdVars: vars }) =
  case Array.last vars of
    Nothing -> Tuple kw (getTok name)
    Just v  -> Tuple kw (snd (typeVarBindingRange v))
  where
  getTok (Name { nameTok: t }) = t

dataCtorRange :: forall a. DataCtor a -> TokenRange
dataCtorRange (DataCtor { dataCtorName: name, dataCtorFields: fields }) =
  case Array.last fields of
    Nothing -> nameRange name
    Just f  -> Tuple (getTok name) (snd (typeRange f))
  where
  getTok (Name { nameTok: t }) = t

classHeadRange :: forall a. ClassHead a -> TokenRange
classHeadRange (ClassHead { clsKeyword: kw, clsName: name, clsVars: vars, clsFundeps: fdeps }) =
  case fdeps of
    Just (Tuple _ fs) ->
      Tuple kw (snd (classFundepRange (sepLast fs)))
    Nothing -> case Array.last vars of
      Nothing -> Tuple kw (snd (nameRange name))
      Just v  -> Tuple kw (snd (typeVarBindingRange v))

classFundepRange :: ClassFundep -> TokenRange
classFundepRange fd = case fd of
  FundepDetermined arr bs ->
    Tuple arr (getTok (NEL.last bs))
  FundepDetermines as_ _ bs ->
    Tuple (getTok (NEL.head as_)) (getTok (NEL.last bs))
  where
  getTok (Name { nameTok: t }) = t

instanceRange :: forall a. Instance a -> TokenRange
instanceRange (Instance { instHead: hd, instBody: bd }) =
  case bd of
    Just (Tuple _ ts) ->
      Tuple (fst start) (snd (instanceBindingRange (NEL.last ts)))
    Nothing -> start
  where
  start = instanceHeadRange hd

instanceHeadRange :: forall a. InstanceHead a -> TokenRange
instanceHeadRange (InstanceHead { instKeyword: kw, instClass: cls, instTypes: types }) =
  case Array.last types of
    Nothing -> Tuple kw (getQualTok cls)
    Just ty -> Tuple kw (snd (typeRange ty))
  where
  getQualTok (QualifiedName { qualTok: t }) = t

instanceBindingRange :: forall a. InstanceBinding a -> TokenRange
instanceBindingRange ib = case ib of
  InstanceBindingSignature _ (Labeled { lblLabel: a, lblValue: b }) ->
    Tuple (getTok a) (snd (typeRange b))
  InstanceBindingName _ a -> valueBindingFieldsRange a
  where
  getTok (Name { nameTok: t }) = t

foreignRange :: forall a. Foreign a -> TokenRange
foreignRange fr = case fr of
  ForeignValue (Labeled { lblLabel: a, lblValue: b }) ->
    Tuple (getTok a) (snd (typeRange b))
  ForeignData a (Labeled { lblValue: b }) ->
    Tuple a (snd (typeRange b))
  ForeignKind a b ->
    Tuple a (getTok b)
  where
  getTok :: forall x. Name x -> SourceToken
  getTok (Name { nameTok: t }) = t

valueBindingFieldsRange :: forall a. ValueBindingFields a -> TokenRange
valueBindingFieldsRange (ValueBindingFields { valName, valGuarded }) =
  Tuple (getTok valName) (snd (guardedRange valGuarded))
  where
  getTok (Name { nameTok: t }) = t

guardedRange :: forall a. Guarded a -> TokenRange
guardedRange g = case g of
  Unconditional a b ->
    Tuple a (snd (whereRange b))
  Guarded as_ ->
    Tuple (fst (guardedExprRange (NEL.head as_))) (snd (guardedExprRange (NEL.last as_)))

guardedExprRange :: forall a. GuardedExpr a -> TokenRange
guardedExprRange (GuardedExpr { grdBar, grdWhere }) =
  Tuple grdBar (snd (whereRange grdWhere))

whereRange :: forall a. Where a -> TokenRange
whereRange (Where { whereExpr, whereBindings }) =
  case whereBindings of
    Just (Tuple _ ls) ->
      Tuple (fst (exprRange whereExpr)) (snd (letBindingRange (NEL.last ls)))
    Nothing -> exprRange whereExpr

typeRange :: forall a. Type a -> TokenRange
typeRange ty = case ty of
  TypeVar _ a              -> nameRange a
  TypeConstructor _ a      -> qualRange a
  TypeWildcard _ a         -> Tuple a a
  TypeHole _ a             -> nameRange a
  TypeString _ a _         -> Tuple a a
  TypeInt _ a b _          -> Tuple (fromMaybe b a) b
  TypeRow _ a              -> wrappedRange a
  TypeRecord _ a           -> wrappedRange a
  TypeForall _ a _ _ b     -> Tuple a (snd (typeRange b))
  TypeKinded _ a _ b       -> Tuple (fst (typeRange a)) (snd (typeRange b))
  TypeApp _ a b            -> Tuple (fst (typeRange a)) (snd (typeRange b))
  TypeOp _ a _ b           -> Tuple (fst (typeRange a)) (snd (typeRange b))
  TypeOpName _ a           -> qualRange a
  TypeArr _ a _ b          -> Tuple (fst (typeRange a)) (snd (typeRange b))
  TypeArrName _ a          -> Tuple a a
  TypeConstrained _ a _ b  -> Tuple (fst (constraintRange a)) (snd (typeRange b))
  TypeParens _ a           -> wrappedRange a
  TypeUnaryRow _ a b       -> Tuple a (snd (typeRange b))

constraintRange :: forall a. Constraint a -> TokenRange
constraintRange c = case c of
  Constraint _ name args ->
    case Array.last args of
      Nothing -> qualRange name
      Just t  -> Tuple (getQualTok name) (snd (typeRange t))
  ConstraintParens _ wrp -> wrappedRange wrp
  where
  getQualTok (QualifiedName { qualTok: t }) = t

typeVarBindingRange :: forall a. TypeVarBinding a -> TokenRange
typeVarBindingRange tvb = case tvb of
  TypeVarKinded a -> wrappedRange a
  TypeVarName (Tuple atSign a) ->
    Tuple (fromMaybe (getTok a) atSign) (getTok a)
  where
  getTok (Name { nameTok: t }) = t

exprRange :: forall a. Expr a -> TokenRange
exprRange e = case e of
  ExprHole _ a              -> nameRange a
  ExprSection _ a           -> Tuple a a
  ExprIdent _ a             -> qualRange a
  ExprConstructor _ a       -> qualRange a
  ExprBoolean _ a _         -> Tuple a a
  ExprChar _ a _            -> Tuple a a
  ExprString _ a _          -> Tuple a a
  ExprNumber _ a _          -> Tuple a a
  ExprArray _ a             -> wrappedRange a
  ExprRecord _ a            -> wrappedRange a
  ExprParens _ a            -> wrappedRange a
  ExprTyped _ a _ b         -> Tuple (fst (exprRange a)) (snd (typeRange b))
  ExprInfix _ a _ b         -> Tuple (fst (exprRange a)) (snd (exprRange b))
  ExprOp _ a _ b            -> Tuple (fst (exprRange a)) (snd (exprRange b))
  ExprOpName _ a            -> qualRange a
  ExprNegate _ a b          -> Tuple a (snd (exprRange b))
  ExprRecordAccessor _ (RecordAccessor { recExpr, recPath }) ->
    Tuple (fst (exprRange recExpr)) (getLblTok (sepLast recPath))
  ExprRecordUpdate _ a b    -> Tuple (fst (exprRange a)) (snd (wrappedRange b))
  ExprApp _ a b             -> Tuple (fst (exprRange a)) (snd (exprRange b))
  ExprVisibleTypeApp _ a _ b -> Tuple (fst (exprRange a)) (snd (typeRange b))
  ExprLambda _ (Lambda { lmbSymbol, lmbBody }) ->
    Tuple lmbSymbol (snd (exprRange lmbBody))
  ExprIf _ (IfThenElse { iteIf, iteFalse }) ->
    Tuple iteIf (snd (exprRange iteFalse))
  ExprCase _ (CaseOf { caseKeyword, caseBranches }) ->
    Tuple caseKeyword (snd (guardedRange (snd (NEL.last caseBranches))))
  ExprLet _ (LetIn { letKeyword, letBody }) ->
    Tuple letKeyword (snd (exprRange letBody))
  ExprDo _ (DoBlock { doKeyword, doStatements }) ->
    Tuple doKeyword (snd (doStatementRange (NEL.last doStatements)))
  ExprAdo _ (AdoBlock { adoKeyword, adoResult }) ->
    Tuple adoKeyword (snd (exprRange adoResult))
  where
  getLblTok (Label { lblTok: t }) = t

letBindingRange :: forall a. LetBinding a -> TokenRange
letBindingRange lb = case lb of
  LetBindingSignature _ (Labeled { lblLabel: a, lblValue: b }) ->
    Tuple (getTok a) (snd (typeRange b))
  LetBindingName _ a -> valueBindingFieldsRange a
  LetBindingPattern _ a _ b ->
    Tuple (fst (binderRange a)) (snd (whereRange b))
  where
  getTok (Name { nameTok: t }) = t

doStatementRange :: forall a. DoStatement a -> TokenRange
doStatementRange ds = case ds of
  DoLet a bs  -> Tuple a (snd (letBindingRange (NEL.last bs)))
  DoDiscard a -> exprRange a
  DoBind a _ b -> Tuple (fst (binderRange a)) (snd (exprRange b))

binderRange :: forall a. Binder a -> TokenRange
binderRange b = case b of
  BinderWildcard _ a         -> Tuple a a
  BinderVar _ a              -> nameRange a
  BinderNamed _ a _ b'       -> Tuple (getTok a) (snd (binderRange b'))
  BinderConstructor _ a bs   ->
    case Array.last bs of
      Nothing -> qualRange a
      Just b' -> Tuple (getQualTok a) (snd (binderRange b'))
  BinderBoolean _ a _        -> Tuple a a
  BinderChar _ a _           -> Tuple a a
  BinderString _ a _         -> Tuple a a
  BinderNumber _ a b' _      -> Tuple (fromMaybe b' a) b'
  BinderArray _ a            -> wrappedRange a
  BinderRecord _ a           -> wrappedRange a
  BinderParens _ a           -> wrappedRange a
  BinderTyped _ a _ b'       -> Tuple (fst (binderRange a)) (snd (typeRange b'))
  BinderOp _ a _ b'          -> Tuple (fst (binderRange a)) (snd (binderRange b'))
  where
  getTok (Name { nameTok: t }) = t
  getQualTok (QualifiedName { qualTok: t }) = t

recordUpdateRange :: forall a. RecordUpdate a -> TokenRange
recordUpdateRange ru = case ru of
  RecordUpdateLeaf a _ b ->
    Tuple (getLblTok a) (snd (exprRange b))
  RecordUpdateBranch a (Wrapped { wrpClose: b }) ->
    Tuple (getLblTok a) b
  where
  getLblTok (Label { lblTok: t }) = t
