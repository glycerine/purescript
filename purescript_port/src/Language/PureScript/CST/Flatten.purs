-- | Flatten CST to a sequence of SourceTokens (for printing/round-tripping).
module Language.PureScript.CST.Flatten
  ( flattenModule
  , flattenType
  , flattenBinder
  , flattenExpr
  , flattenDeclaration
  , flattenImportDecl
  ) where

import Prelude

import Data.Array (concatMap) as Array
import Data.Foldable (foldMap)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Language.PureScript.CST.Positions (advanceLeading, moduleRange, srcRange)
import Language.PureScript.CST.Types
  ( AdoBlock(..)
  , Binder(..)
  , CaseOf(..)
  , ClassFundep(..)
  , ClassHead(..)
  , Comment
  , Constraint(..)
  , CSTSourcePos(..)
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
  , LineFeed
  , Module(..)
  , Name(..)
  , OneOrDelimited(..)
  , PatternGuard(..)
  , QualifiedName(..)
  , RecordAccessor(..)
  , RecordLabeled(..)
  , RecordUpdate(..)
  , Role(..)
  , Row(..)
  , Separated(..)
  , SourceRange(..)
  , SourceToken(..)
  , Token(..)
  , TokenAnn(..)
  , Type(..)
  , TypeVarBinding(..)
  , ValueBindingFields(..)
  , Where(..)
  , Wrapped(..)
  )

type Tokens = Array SourceToken

tok :: SourceToken -> Tokens
tok t = [t]

flattenModule :: forall a. Module a -> Tokens
flattenModule m@(Module md) =
  tok md.modKeyword
    <> flattenName md.modNamespace
    <> foldMap (flattenWrapped (flattenSeparated flattenExport)) md.modExports
    <> tok md.modWhere
    <> foldMap flattenImportDecl md.modImports
    <> foldMap flattenDeclaration md.modDecls
    <> [eofTok]
  where
  Tuple _ endTkn = moduleRange m
  eofPos = advanceLeading (case srcRange endTkn of SourceRange { srcEnd: p } -> p) md.modTrailingComments
  eofRange = SourceRange { srcStart: eofPos, srcEnd: eofPos }
  eofTok = SourceToken
    { tokAnn: TokenAnn
        { tokRange: eofRange
        , tokLeadingComments: md.modTrailingComments
        , tokTrailingComments: []
        }
    , tokValue: TokEof
    }

flattenDataHead :: forall a. DataHead a -> Tokens
flattenDataHead (DataHead { dataHdKeyword, dataHdName, dataHdVars }) =
  tok dataHdKeyword
    <> flattenName dataHdName
    <> foldMap flattenTypeVarBinding dataHdVars

flattenDataCtor :: forall a. DataCtor a -> Tokens
flattenDataCtor (DataCtor { dataCtorName, dataCtorFields }) =
  flattenName dataCtorName <> foldMap flattenType dataCtorFields

flattenClassHead :: forall a. ClassHead a -> Tokens
flattenClassHead (ClassHead ch) =
  tok ch.clsKeyword
    <> foldMap (\(Tuple f g) -> flattenOneOrDelimited flattenConstraint f <> tok g) ch.clsSuper
    <> flattenName ch.clsName
    <> foldMap flattenTypeVarBinding ch.clsVars
    <> foldMap (\(Tuple f g) -> tok f <> flattenSeparated flattenClassFundep g) ch.clsFundeps

flattenClassFundep :: ClassFundep -> Tokens
flattenClassFundep fd = case fd of
  FundepDetermined a b -> tok a <> foldMap flattenName b
  FundepDetermines a b c -> foldMap flattenName a <> tok b <> foldMap flattenName c

flattenInstance :: forall a. Instance a -> Tokens
flattenInstance (Instance inst) =
  flattenInstanceHead inst.instHead
    <> foldMap (\(Tuple c d) -> tok c <> foldMap flattenInstanceBinding d) inst.instBody

flattenInstanceHead :: forall a. InstanceHead a -> Tokens
flattenInstanceHead (InstanceHead ih) =
  tok ih.instKeyword
    <> foldMap (\(Tuple n s) -> flattenName n <> tok s) ih.instNameSep
    <> foldMap (\(Tuple g h) -> flattenOneOrDelimited flattenConstraint g <> tok h) ih.instConstraints
    <> flattenQualifiedName ih.instClass
    <> foldMap flattenType ih.instTypes

flattenInstanceBinding :: forall a. InstanceBinding a -> Tokens
flattenInstanceBinding ib = case ib of
  InstanceBindingSignature _ a -> flattenLabeled flattenName flattenType a
  InstanceBindingName _ a      -> flattenValueBindingFields a

flattenValueBindingFields :: forall a. ValueBindingFields a -> Tokens
flattenValueBindingFields (ValueBindingFields vb) =
  flattenName vb.valName
    <> foldMap flattenBinder vb.valBinders
    <> flattenGuarded vb.valGuarded

flattenBinder :: forall a. Binder a -> Tokens
flattenBinder b = case b of
  BinderWildcard _ a       -> tok a
  BinderVar _ a            -> flattenName a
  BinderNamed _ a bnd c    -> flattenName a <> tok bnd <> flattenBinder c
  BinderConstructor _ a bs -> flattenQualifiedName a <> foldMap flattenBinder bs
  BinderBoolean _ a _      -> tok a
  BinderChar _ a _         -> tok a
  BinderString _ a _       -> tok a
  BinderNumber _ a bnd _   -> maybe [] tok a <> tok bnd
  BinderArray _ a          -> flattenWrapped (foldMap (flattenSeparated flattenBinder)) a
  BinderRecord _ a         ->
    flattenWrapped (foldMap (flattenSeparated (flattenRecordLabeled flattenBinder))) a
  BinderParens _ a         -> flattenWrapped flattenBinder a
  BinderTyped _ a bnd c   -> flattenBinder a <> tok bnd <> flattenType c
  BinderOp _ a bnd c      -> flattenBinder a <> flattenQualifiedName bnd <> flattenBinder c

flattenRecordLabeled :: forall a. (a -> Tokens) -> RecordLabeled a -> Tokens
flattenRecordLabeled f rl = case rl of
  RecordPun a          -> flattenName a
  RecordField a bnd c  -> flattenLabel a <> tok bnd <> f c

flattenRecordAccessor :: forall a. RecordAccessor a -> Tokens
flattenRecordAccessor (RecordAccessor ra) =
  flattenExpr ra.recExpr <> tok ra.recDot <> flattenSeparated flattenLabel ra.recPath

flattenRecordUpdate :: forall a. RecordUpdate a -> Tokens
flattenRecordUpdate ru = case ru of
  RecordUpdateLeaf a bnd c -> flattenLabel a <> tok bnd <> flattenExpr c
  RecordUpdateBranch a b   -> flattenLabel a <> flattenWrapped (flattenSeparated flattenRecordUpdate) b

flattenLambda :: forall a. Lambda a -> Tokens
flattenLambda (Lambda lmb) =
  tok lmb.lmbSymbol
    <> foldMap flattenBinder lmb.lmbBinders
    <> tok lmb.lmbArr
    <> flattenExpr lmb.lmbBody

flattenIfThenElse :: forall a. IfThenElse a -> Tokens
flattenIfThenElse (IfThenElse ite) =
  tok ite.iteIf
    <> flattenExpr ite.iteCond
    <> tok ite.iteThen
    <> flattenExpr ite.iteTrue
    <> tok ite.iteElse
    <> flattenExpr ite.iteFalse

flattenCaseOf :: forall a. CaseOf a -> Tokens
flattenCaseOf (CaseOf c) =
  tok c.caseKeyword
    <> flattenSeparated flattenExpr c.caseHead
    <> tok c.caseOf
    <> foldMap (\(Tuple e f) -> flattenSeparated flattenBinder e <> flattenGuarded f) c.caseBranches

flattenLetIn :: forall a. LetIn a -> Tokens
flattenLetIn (LetIn li) =
  tok li.letKeyword
    <> foldMap flattenLetBinding li.letBindings
    <> tok li.letIn
    <> flattenExpr li.letBody

flattenDoBlock :: forall a. DoBlock a -> Tokens
flattenDoBlock (DoBlock db) =
  tok db.doKeyword <> foldMap flattenDoStatement db.doStatements

flattenAdoBlock :: forall a. AdoBlock a -> Tokens
flattenAdoBlock (AdoBlock ab) =
  tok ab.adoKeyword
    <> foldMap flattenDoStatement ab.adoStatements
    <> tok ab.adoIn
    <> flattenExpr ab.adoResult

flattenDoStatement :: forall a. DoStatement a -> Tokens
flattenDoStatement ds = case ds of
  DoLet a b  -> tok a <> foldMap flattenLetBinding b
  DoDiscard a -> flattenExpr a
  DoBind a bnd c -> flattenBinder a <> tok bnd <> flattenExpr c

flattenExpr :: forall a. Expr a -> Tokens
flattenExpr e = case e of
  ExprHole _ a             -> flattenName a
  ExprSection _ a          -> tok a
  ExprIdent _ a            -> flattenQualifiedName a
  ExprConstructor _ a      -> flattenQualifiedName a
  ExprBoolean _ a _        -> tok a
  ExprChar _ a _           -> tok a
  ExprString _ a _         -> tok a
  ExprNumber _ a _         -> tok a
  ExprArray _ a            -> flattenWrapped (foldMap (flattenSeparated flattenExpr)) a
  ExprRecord _ a           ->
    flattenWrapped (foldMap (flattenSeparated (flattenRecordLabeled flattenExpr))) a
  ExprParens _ a           -> flattenWrapped flattenExpr a
  ExprTyped _ a bnd c      -> flattenExpr a <> tok bnd <> flattenType c
  ExprInfix _ a b c        -> flattenExpr a <> flattenWrapped flattenExpr b <> flattenExpr c
  ExprOp _ a b c           -> flattenExpr a <> flattenQualifiedName b <> flattenExpr c
  ExprOpName _ a           -> flattenQualifiedName a
  ExprNegate _ a b         -> tok a <> flattenExpr b
  ExprRecordAccessor _ a   -> flattenRecordAccessor a
  ExprRecordUpdate _ a b   -> flattenExpr a <> flattenWrapped (flattenSeparated flattenRecordUpdate) b
  ExprApp _ a b            -> flattenExpr a <> flattenExpr b
  ExprVisibleTypeApp _ a bnd c -> flattenExpr a <> tok bnd <> flattenType c
  ExprLambda _ a           -> flattenLambda a
  ExprIf _ a               -> flattenIfThenElse a
  ExprCase _ a             -> flattenCaseOf a
  ExprLet _ a              -> flattenLetIn a
  ExprDo _ a               -> flattenDoBlock a
  ExprAdo _ a              -> flattenAdoBlock a

flattenLetBinding :: forall a. LetBinding a -> Tokens
flattenLetBinding lb = case lb of
  LetBindingSignature _ a     -> flattenLabeled flattenName flattenType a
  LetBindingName _ a          -> flattenValueBindingFields a
  LetBindingPattern _ a bnd c -> flattenBinder a <> tok bnd <> flattenWhere c

flattenWhere :: forall a. Where a -> Tokens
flattenWhere (Where w) =
  flattenExpr w.whereExpr
    <> foldMap (\(Tuple c d) -> tok c <> foldMap flattenLetBinding d) w.whereBindings

flattenPatternGuard :: forall a. PatternGuard a -> Tokens
flattenPatternGuard (PatternGuard pg) =
  foldMap (\(Tuple c d) -> flattenBinder c <> tok d) pg.patBinder
    <> flattenExpr pg.patExpr

flattenGuardedExpr :: forall a. GuardedExpr a -> Tokens
flattenGuardedExpr (GuardedExpr ge) =
  tok ge.grdBar
    <> flattenSeparated flattenPatternGuard ge.grdPatterns
    <> tok ge.grdSep
    <> flattenWhere ge.grdWhere

flattenGuarded :: forall a. Guarded a -> Tokens
flattenGuarded g = case g of
  Unconditional a b -> tok a <> flattenWhere b
  Guarded a         -> foldMap flattenGuardedExpr a

flattenFixityFields :: FixityFields -> Tokens
flattenFixityFields (FixityFields { fxtKeyword: Tuple a _, fxtPrec: Tuple b _, fxtOp: c }) =
  tok a <> tok b <> flattenFixityOp c

flattenFixityOp :: FixityOp -> Tokens
flattenFixityOp fo = case fo of
  FixityValue a bnd c -> flattenQualifiedName a <> tok bnd <> flattenName c
  FixityType a b bnd c -> tok a <> flattenQualifiedName b <> tok bnd <> flattenName c

flattenForeign :: forall a. Foreign a -> Tokens
flattenForeign fr = case fr of
  ForeignValue a    -> flattenLabeled flattenName flattenType a
  ForeignData a b   -> tok a <> flattenLabeled flattenName flattenType b
  ForeignKind a b   -> tok a <> flattenName b

flattenRole :: Role -> Tokens
flattenRole (Role { roleTok: t }) = tok t

flattenDeclaration :: forall a. Declaration a -> Tokens
flattenDeclaration d = case d of
  DeclData _ a b ->
    flattenDataHead a
      <> foldMap (\(Tuple t cs) -> tok t <> flattenSeparated flattenDataCtor cs) b
  DeclType _ a b c ->
    flattenDataHead a <> tok b <> flattenType c
  DeclNewtype _ a b c d2 ->
    flattenDataHead a <> tok b <> flattenName c <> flattenType d2
  DeclClass _ a b ->
    flattenClassHead a
      <> foldMap (\(Tuple c d2) -> tok c <> foldMap (flattenLabeled flattenName flattenType) d2) b
  DeclInstanceChain _ a ->
    flattenSeparated flattenInstance a
  DeclDerive _ a b c ->
    tok a <> maybe [] tok b <> flattenInstanceHead c
  DeclKindSignature _ a b ->
    tok a <> flattenLabeled flattenName flattenType b
  DeclSignature _ a ->
    flattenLabeled flattenName flattenType a
  DeclFixity _ a ->
    flattenFixityFields a
  DeclForeign _ a b c ->
    tok a <> tok b <> flattenForeign c
  DeclRole _ a b c d2 ->
    tok a <> tok b <> flattenName c <> foldMap flattenRole d2
  DeclValue _ a ->
    flattenValueBindingFields a

flattenQualifiedName :: forall a. QualifiedName a -> Tokens
flattenQualifiedName (QualifiedName { qualTok: t }) = tok t

flattenName :: forall a. Name a -> Tokens
flattenName (Name { nameTok: t }) = tok t

flattenLabel :: Label -> Tokens
flattenLabel (Label { lblTok: t }) = tok t

flattenExport :: forall a. Export a -> Tokens
flattenExport ex = case ex of
  ExportValue _ n      -> flattenName n
  ExportOp _ n         -> flattenName n
  ExportType _ n dms   -> flattenName n <> foldMap flattenDataMembers dms
  ExportTypeOp _ t n   -> tok t <> flattenName n
  ExportClass _ t n    -> tok t <> flattenName n
  ExportModule _ t n   -> tok t <> flattenName n

flattenDataMembers :: forall a. DataMembers a -> Tokens
flattenDataMembers dm = case dm of
  DataAll _ t     -> tok t
  DataEnumerated _ ns -> flattenWrapped (foldMap (flattenSeparated flattenName)) ns

flattenImportDecl :: forall a. ImportDecl a -> Tokens
flattenImportDecl (ImportDecl imp) =
  tok imp.impKeyword
    <> flattenName imp.impModule
    <> foldMap (\(Tuple mt is) -> foldMap tok mt <> flattenWrapped (flattenSeparated flattenImport) is) imp.impNames
    <> foldMap (\(Tuple t n) -> tok t <> flattenName n) imp.impQual

flattenImport :: forall a. Import a -> Tokens
flattenImport im = case im of
  ImportValue _ n     -> flattenName n
  ImportOp _ n        -> flattenName n
  ImportType _ n dms  -> flattenName n <> foldMap flattenDataMembers dms
  ImportTypeOp _ t n  -> tok t <> flattenName n
  ImportClass _ t n   -> tok t <> flattenName n

flattenWrapped :: forall a. (a -> Tokens) -> Wrapped a -> Tokens
flattenWrapped k (Wrapped { wrpOpen: a, wrpValue: b, wrpClose: c }) =
  tok a <> k b <> tok c

flattenSeparated :: forall a. (a -> Tokens) -> Separated a -> Tokens
flattenSeparated k (Separated { sepHead: a, sepTail: b }) =
  k a <> foldMap (\(Tuple c d) -> tok c <> k d) b

flattenOneOrDelimited :: forall a. (a -> Tokens) -> OneOrDelimited a -> Tokens
flattenOneOrDelimited f od = case od of
  One a  -> f a
  Many a -> flattenWrapped (flattenSeparated f) a

flattenLabeled :: forall a b. (a -> Tokens) -> (b -> Tokens) -> Labeled a b -> Tokens
flattenLabeled ka kb (Labeled { lblLabel: a, lblSep: b, lblValue: c }) =
  ka a <> tok b <> kb c

flattenType :: forall a. Type a -> Tokens
flattenType ty = case ty of
  TypeVar _ a             -> flattenName a
  TypeConstructor _ a     -> flattenQualifiedName a
  TypeWildcard _ a        -> tok a
  TypeHole _ a            -> flattenName a
  TypeString _ a _        -> tok a
  TypeInt _ a b _         -> maybe [] tok a <> tok b
  TypeRow _ a             -> flattenWrapped flattenRow a
  TypeRecord _ a          -> flattenWrapped flattenRow a
  TypeForall _ a b c d    -> tok a <> foldMap flattenTypeVarBinding b <> tok c <> flattenType d
  TypeKinded _ a b c      -> flattenType a <> tok b <> flattenType c
  TypeApp _ a b           -> flattenType a <> flattenType b
  TypeOp _ a b c          -> flattenType a <> flattenQualifiedName b <> flattenType c
  TypeOpName _ a          -> flattenQualifiedName a
  TypeArr _ a b c         -> flattenType a <> tok b <> flattenType c
  TypeArrName _ a         -> tok a
  TypeConstrained _ a b c -> flattenConstraint a <> tok b <> flattenType c
  TypeParens _ a          -> flattenWrapped flattenType a
  TypeUnaryRow _ a b      -> tok a <> flattenType b

flattenRow :: forall a. Row a -> Tokens
flattenRow (Row { rowLabels, rowTail }) =
  foldMap (flattenSeparated (flattenLabeled (\(Label { lblTok: t }) -> tok t) flattenType)) rowLabels
    <> foldMap (\(Tuple a b) -> tok a <> flattenType b) rowTail

flattenTypeVarBinding :: forall a. TypeVarBinding a -> Tokens
flattenTypeVarBinding tvb = case tvb of
  TypeVarKinded a -> flattenWrapped (flattenLabeled go flattenType) a
  TypeVarName a   -> go a
  where
  go (Tuple a b) = maybe [] tok a <> flattenName b

flattenConstraint :: forall a. Constraint a -> Tokens
flattenConstraint c = case c of
  Constraint _ a b     -> flattenQualifiedName a <> foldMap flattenType b
  ConstraintParens _ a -> flattenWrapped flattenConstraint a
