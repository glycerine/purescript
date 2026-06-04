module Language.PureScript.CST.Convert
  ( convertType
  , convertVtaType
  , convertExpr
  , convertBinder
  , convertDeclaration
  , convertImportDecl
  , convertModule
  , sourcePos
  , sourceSpan
  , comment
  , comments
  ) where

import Prelude

import Control.Lazy (defer)
import Data.Array as Array
import Data.Array (mapWithIndex, length, null, zip, last) as Array
import Data.String.Common (toLower) as Str
import Data.Either (Either(..))
import Data.Foldable (foldl, class Foldable)
import Data.Array (toUnfoldable) as Array
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), isJust, fromJust, maybe)
import Data.Array (mapMaybe) as Array
import Data.String (drop, take, length) as Str
import Data.String (toCodePointArray, fromCodePointArray, codePointFromChar) as SCP
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Partial.Unsafe (unsafePartial)

import Language.PureScript.AST.Declarations
  ( Declaration(..)
  , DeclarationRef(..)
  , ImportDeclarationType(..)
  , TypeInstanceBody(..)
  , KindSignatureFor(..)
  , Guard(..)
  , GuardedExpr(..)
  , Expr(..)
  , WhereProvenance(..)
  , CaseAlternative(..)
  , DoNotationElement(..)
  , DataConstructorDeclaration(..)
  , RoleDeclarationData(..)
  , TypeDeclarationData(..)
  , ValueDeclarationData(..)
  , ValueFixity(..)
  , TypeFixity(..)
  , Module(..)
  , PathNode(..)
  , PathTree(..)
  , AssocList(..)
  ) as AST
import Language.PureScript.AST.Declarations.ChainId (mkChainId)
import Language.PureScript.AST.Operators (Associativity(..), Fixity(..)) as AST
import Language.PureScript.AST.SourcePos
  ( SourceAnn
  , SourcePos(..)
  , SourceSpan(..)
  , widenSourceAnn
  , widenSourceSpan
  )
import Language.PureScript.AST.Binders (Binder(..)) as AST
import Language.PureScript.Comments (Comment(..)) as C
import Language.PureScript.Environment (DataDeclType(..), NameKind(..), FunctionalDependency(..), tyRecord, tyFunction, kindRow) as Env
import Language.PureScript.Label (Label(..)) as PL
import Language.PureScript.Names
  ( Ident(..)
  , ModuleName(..)
  , OpName(..)
  , ProperName(..)
  , Qualified(..)
  , QualifiedBy(..)
  , ClassName
  , ConstructorName
  , TypeName
  , ValueOpName
  , TypeOpName
  , AnyOpName
  , byMaybeModuleName
  , runProperName
  , runOpName
  ) as N
import Language.PureScript.PSString (PSString, mkString, prettyPrintStringJS)
import Language.PureScript.Types
  ( Type(..)
  , Constraint(..)
  , WildcardData(..)
  , TypeVarVisibility(..)
  , SourceType
  , SourceConstraint
  , getAnnForType
  , setAnnForType
  ) as T

import Language.PureScript.CST.Positions
  ( toSourceRange
  , typeRange
  , constraintRange
  , exprRange
  , letBindingRange
  , doStatementRange
  , binderRange
  , declRange
  , importDeclRange
  , importRange
  , exportRange
  , instanceRange
  , instanceBindingRange
  , qualRange
  , moduleRange
  )
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.CST.Print (printToken)
import Language.PureScript.CST.Types
  ( AdoBlock(..)
  , Binder(..)
  , Ident(..)
  , CaseOf(..)
  , ClassFundep(..)
  , ClassHead(..)
  , Comment(..)
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
  , Fixity(..)
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

comment :: forall a. Comment a -> Maybe C.Comment
comment = case _ of
  Comment t
    | SCU.take 2 t == "{-" -> Just $ C.BlockComment $ SCU.take (SCU.length t - 4) (SCU.drop 2 t)
    | SCU.take 2 t == "--" -> Just $ C.LineComment $ SCU.drop 2 t
  _ -> Nothing

comments :: forall a. Array (Comment a) -> Array C.Comment
comments = Array.mapMaybe comment

sourcePos :: CSTSourcePos -> SourcePos
sourcePos (CSTSourcePos { srcLine, srcColumn }) = SourcePos { line: srcLine, column: srcColumn }

sourceSpan :: String -> SourceRange -> SourceSpan
sourceSpan name (SourceRange { srcStart, srcEnd }) =
  SourceSpan { name, start: sourcePos srcStart, end: sourcePos srcEnd }

widenLeft :: TokenAnn -> SourceAnn -> SourceAnn
widenLeft (TokenAnn { tokRange, tokLeadingComments }) (Tuple sp _) =
  Tuple (widenSourceSpan (sourceSpan (case sp of SourceSpan s -> s.name) tokRange) sp)
        (comments tokLeadingComments)
  where
  sourceSpan' n (SourceRange { srcStart, srcEnd }) =
    SourceSpan { name: n, start: sourcePos srcStart, end: sourcePos srcEnd }
  widenLeft' n range sp' = widenSourceSpan (sourceSpan' n range) sp'

sourceAnnCommented :: String -> SourceToken -> SourceToken -> SourceAnn
sourceAnnCommented fileName (SourceToken { tokAnn: TokenAnn ann1 }) (SourceToken { tokAnn: TokenAnn ann2 }) =
  Tuple
    (SourceSpan
      { name: fileName
      , start: sourcePos (case ann1.tokRange of SourceRange r -> r.srcStart)
      , end:   sourcePos (case ann2.tokRange of SourceRange r -> r.srcEnd)
      })
    (comments ann1.tokLeadingComments)

sourceAnn :: String -> SourceToken -> SourceToken -> SourceAnn
sourceAnn fileName (SourceToken { tokAnn: TokenAnn ann1 }) (SourceToken { tokAnn: TokenAnn ann2 }) =
  Tuple
    (SourceSpan
      { name: fileName
      , start: sourcePos (case ann1.tokRange of SourceRange r -> r.srcStart)
      , end:   sourcePos (case ann2.tokRange of SourceRange r -> r.srcEnd)
      })
    []

sourceName :: forall a. String -> Name a -> SourceAnn
sourceName fileName a = sourceAnnCommented fileName (nameTok a) (nameTok a)
  where
  nameTok (Name { nameTok: t }) = t

sourceQualName :: forall a. String -> QualifiedName a -> SourceAnn
sourceQualName fileName a = sourceAnnCommented fileName (qualTok a) (qualTok a)
  where
  qualTok (QualifiedName { qualTok: t }) = t

spanName :: SourceSpan -> String
spanName (SourceSpan s) = s.name

modName :: Token -> Maybe N.ModuleName
modName = case _ of
  TokLowerName as _ -> go as
  TokUpperName as _ -> go as
  TokSymbolName as _ -> go as
  TokOperator as _ -> go as
  _ -> Nothing
  where
  go [] = Nothing
  go ns = Just $ N.ModuleName $ Array.foldl (\acc s -> if acc == "" then s else acc <> "." <> s) "" ns

qualified :: forall a. QualifiedName a -> N.Qualified a
qualified (QualifiedName { qualModule, qualName }) =
  N.Qualified (N.byMaybeModuleName qualModule) qualName

ident :: Ident -> N.Ident
ident (Ident s) = N.Ident s

getIdent :: Ident -> String
getIdent (Ident s) = s

nameValue :: forall a. Name a -> a
nameValue (Name { nameValue: v }) = v

nameTok :: forall a. Name a -> SourceToken
nameTok (Name { nameTok: t }) = t

qualTok :: forall a. QualifiedName a -> SourceToken
qualTok (QualifiedName { qualTok: t }) = t

qualName :: forall a. QualifiedName a -> a
qualName (QualifiedName { qualName: n }) = n

tokAnn :: SourceToken -> TokenAnn
tokAnn (SourceToken { tokAnn: a }) = a

tokRange :: TokenAnn -> SourceRange
tokRange (TokenAnn { tokRange: r }) = r

sepHead :: forall a. Separated a -> a
sepHead (Separated { sepHead: h }) = h

sepToList :: forall a. Separated a -> Array a
sepToList (Separated { sepHead: h, sepTail: t }) = Array.cons h (map snd t)

wrpValue :: forall a. Wrapped a -> a
wrpValue (Wrapped { wrpValue: v }) = v

instKeyword :: forall a. InstanceHead a -> SourceToken
instKeyword (InstanceHead { instKeyword: kw }) = kw

instHead :: forall a. Instance a -> InstanceHead a
instHead (Instance { instHead: h }) = h

startSourcePos :: SourceToken -> SourcePos
startSourcePos st = sourcePos (case tokRange (tokAnn st) of SourceRange r -> r.srcStart)

-- ---------------------------------------------------------------------------
-- Convert types

convertType :: forall a. String -> Type a -> T.SourceType
convertType = convertType' false

convertVtaType :: forall a. String -> Type a -> T.SourceType
convertVtaType = convertType' true

convertType' :: forall a. Boolean -> String -> Type a -> T.SourceType
convertType' withinVta fileName = go
  where
  goRow :: Row a -> SourceToken -> T.SourceType
  goRow (Row { rowLabels, rowTail }) b =
    let
      rowTailTy = case rowTail of
        Just (Tuple _ ty) -> go ty
        Nothing -> T.REmpty (sourceAnnCommented fileName b b)
      rowCons (Labeled { lblLabel, lblValue: ty }) c =
        let ann = sourceAnnCommented fileName (lblTok lblLabel) (snd (typeRange ty))
        in T.RCons ann (lblName lblLabel) (go ty) c
    in case rowLabels of
      Just (Separated { sepHead: h, sepTail: t }) ->
        rowCons h (Array.foldl (\acc (Tuple _ x) -> rowCons x acc) rowTailTy (Array.reverse t))
      Nothing -> rowTailTy

  go = defer \_ -> case _ of
    TypeVar _ a ->
      T.TypeVar (sourceName fileName a) (getIdent (nameValue a))
    TypeConstructor _ a ->
      T.TypeConstructor (sourceQualName fileName a) (qualified a)
    TypeWildcard _ a ->
      T.TypeWildcard (sourceAnnCommented fileName a a) (if withinVta then T.IgnoredWildcard else T.UnnamedWildcard)
    TypeHole _ a ->
      T.TypeWildcard (sourceName fileName a) (T.HoleWildcard (getIdent (nameValue a)))
    TypeString _ a b ->
      T.TypeLevelString (sourceAnnCommented fileName a a) b
    TypeInt _ _ a b ->
      T.TypeLevelInt (sourceAnnCommented fileName a a) b
    TypeRow _ (Wrapped { wrpOpen: _, wrpValue: row, wrpClose: b }) ->
      goRow row b
    TypeRecord _ (Wrapped { wrpOpen: a, wrpValue: row, wrpClose: b }) ->
      let
        ann = sourceAnnCommented fileName a b
        annRec = sourceAnn fileName a a
      in T.TypeApp ann (annRec <$ Env.tyRecord) (goRow row b)
    TypeForall _ kw bindings _ ty ->
      let
        mkForAll a mb v t =
          let ann' = widenLeft (tokAnn (nameTok a)) (T.getAnnForType t)
          in T.ForAll ann' (maybe T.TypeVarInvisible (const T.TypeVarVisible) v) (getIdent (nameValue a)) mb t Nothing
        k tvb = case tvb of
          TypeVarKinded (Wrapped { wrpValue: Labeled { lblLabel: Tuple v a, lblValue: b } }) ->
            mkForAll a (Just (go b)) v
          TypeVarName (Tuple v a) ->
            mkForAll a Nothing v
        ty' = Array.foldr (\tvb acc -> k tvb acc) (go ty) (NEL.toUnfoldable bindings)
        ann = widenLeft (tokAnn kw) (T.getAnnForType ty')
      in T.setAnnForType ann ty'
    TypeKinded _ ty _ kd ->
      let
        ty' = go ty
        kd' = go kd
        ann = widenSourceAnn (T.getAnnForType ty') (T.getAnnForType kd')
      in T.KindedType ann ty' kd'
    TypeApp _ a b ->
      let
        a' = go a
        b' = go b
        ann = widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
      in T.TypeApp ann a' b'
    ty@(TypeOp _ _ _ _) ->
      let
        reassoc op b' a =
          let
            a'  = go a
            op' = T.TypeOp (sourceQualName fileName op) (qualified op)
            ann = widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
          in T.BinaryNoParensType ann op' (go a) b'
        loop k = case _ of
          TypeOp _ a op b -> loop (reassoc op (k b)) a
          expr' -> k expr'
      in loop go ty
    TypeOpName _ op ->
      let rng = qualRange op
      in T.TypeOp (uncurry (sourceAnnCommented fileName) rng) (qualified op)
    TypeArr _ a arr b ->
      let
        a' = go a
        b' = go b
        arr' = sourceAnnCommented fileName arr arr <$ Env.tyFunction
        ann = widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
      in T.TypeApp ann (T.TypeApp ann arr' a') b'
    TypeArrName _ a ->
      sourceAnnCommented fileName a a <$ Env.tyFunction
    TypeConstrained _ a _ b ->
      let
        a' = convertConstraint withinVta fileName a
        b' = go b
        ann = widenSourceAnn (case a' of T.Constraint c -> c.constraintAnn) (T.getAnnForType b')
      in T.ConstrainedType ann a' b'
    TypeParens _ (Wrapped { wrpOpen: a, wrpValue: ty, wrpClose: b }) ->
      T.ParensInType (sourceAnnCommented fileName a b) (go ty)
    ty@(TypeUnaryRow _ _ a) ->
      let
        a' = go a
        rng = typeRange ty
        ann = uncurry (sourceAnnCommented fileName) rng
      in T.setAnnForType ann (Env.kindRow a')

  lblTok :: Label -> SourceToken
  lblTok (Label l) = l.lblTok

  lblName :: Label -> PL.Label
  lblName (Label l) = PL.Label l.lblName

convertConstraint :: forall a. Boolean -> String -> Constraint a -> T.SourceConstraint
convertConstraint withinVta fileName = go
  where
  go cst = case cst of
    cst'@(Constraint _ name args) ->
      let ann = uncurry (sourceAnnCommented fileName) (constraintRange cst')
      in T.Constraint
        { constraintAnn: ann
        , constraintClass: qualified name
        , constraintKindArgs: []
        , constraintArgs: map (convertType' withinVta fileName) args
        , constraintData: Nothing
        }
    ConstraintParens _ (Wrapped { wrpValue: c }) -> go c

-- ---------------------------------------------------------------------------
-- Convert guarded exprs / where / let

convertGuarded :: forall a. String -> Guarded a -> Array AST.GuardedExpr
convertGuarded fileName = case _ of
  Unconditional _ x -> [AST.GuardedExpr [] (convertWhere fileName x)]
  Guarded gs ->
    map (\(GuardedExpr { grdBar: _, grdPatterns: ps, grdSep: _, grdWhere: x }) ->
      AST.GuardedExpr (map p (sepToList ps)) (convertWhere fileName x))
      (NEL.toUnfoldable gs)
  where
  go = convertExpr fileName
  p (PatternGuard { patBinder: Nothing, patExpr: x }) = AST.ConditionGuard (go x)
  p (PatternGuard { patBinder: Just (Tuple b _), patExpr: x }) = AST.PatternGuard (convertBinder fileName b) (go x)

convertWhere :: forall a. String -> Where a -> AST.Expr
convertWhere fileName = case _ of
  Where { whereExpr: expr, whereBindings: Nothing } -> convertExpr fileName expr
  Where { whereExpr: expr, whereBindings: Just (Tuple _ bs) } ->
    let ann = uncurry (sourceAnnCommented fileName) (exprRange expr)
    in uncurry AST.PositionedValue ann
        (AST.Let AST.FromWhere (map (convertLetBinding fileName) (NEL.toUnfoldable bs)) (convertExpr fileName expr))

convertLetBinding :: forall a. String -> LetBinding a -> AST.Declaration
convertLetBinding fileName = case _ of
  LetBindingSignature _ lbl ->
    convertSignature fileName lbl
  binding@(LetBindingName _ fields) ->
    let ann = uncurry (sourceAnnCommented fileName) (letBindingRange binding)
    in convertValueBindingFields fileName ann fields
  binding@(LetBindingPattern _ a _ b) ->
    let ann = uncurry (sourceAnnCommented fileName) (letBindingRange binding)
    in AST.BoundValueDeclaration ann (convertBinder fileName a) (convertWhere fileName b)

-- ---------------------------------------------------------------------------
-- Convert expressions

convertExpr :: forall a. String -> Expr a -> AST.Expr
convertExpr fileName = go
  where
  positioned = uncurry AST.PositionedValue

  goDoStatement :: DoStatement a -> AST.DoNotationElement
  goDoStatement stmt = case stmt of
    DoLet t as ->
      let ann = uncurry (sourceAnnCommented fileName) (doStatementRange stmt)
      in uncurry AST.PositionedDoNotationElement ann
          (AST.DoNotationLet (map (convertLetBinding fileName) (NEL.toUnfoldable as)))
    DoDiscard a ->
      let ann = uncurry (sourceAnn fileName) (doStatementRange stmt)
      in uncurry AST.PositionedDoNotationElement ann (AST.DoNotationValue (go a))
    DoBind a _ b ->
      let
        ann = uncurry (sourceAnn fileName) (doStatementRange stmt)
        a' = convertBinder fileName a
        b' = go b
      in uncurry AST.PositionedDoNotationElement ann (AST.DoNotationBind a' b')

  go = defer \_ -> case _ of
    ExprHole _ a ->
      positioned (sourceName fileName a) (AST.Hole (getIdent (nameValue a)))
    ExprSection _ a ->
      positioned (sourceAnnCommented fileName a a) AST.AnonymousArgument
    ExprIdent _ a ->
      let ann = sourceQualName fileName a
      in positioned ann (AST.Var (fst ann) (qualified (map ident a)))
    ExprConstructor _ a ->
      let ann = sourceQualName fileName a
      in positioned ann (AST.Constructor (fst ann) (qualified a))
    ExprBoolean _ a b ->
      let ann = sourceAnnCommented fileName a a
      in positioned ann (AST.Literal (fst ann) (BooleanLiteral b))
    ExprChar _ a b ->
      let ann = sourceAnnCommented fileName a a
      in positioned ann (AST.Literal (fst ann) (CharLiteral b))
    ExprString _ a b ->
      let ann = sourceAnnCommented fileName a a
      in positioned ann (AST.Literal (fst ann) (StringLiteral b))
    ExprNumber _ a b ->
      let ann = sourceAnnCommented fileName a a
      in positioned ann (AST.Literal (fst ann) (NumericLiteral b))
    ExprArray _ (Wrapped { wrpOpen: a, wrpValue: bs, wrpClose: c }) ->
      let
        ann = sourceAnnCommented fileName a c
        vals = case bs of
          Just (Separated { sepHead: x, sepTail: xs }) -> Array.cons (go x) (map (go <<< snd) xs)
          Nothing -> []
      in positioned ann (AST.Literal (fst ann) (ArrayLiteral vals))
    ExprRecord z (Wrapped { wrpOpen: a, wrpValue: bs, wrpClose: c }) ->
      let
        ann = sourceAnnCommented fileName a c
        lbl = case _ of
          RecordPun f -> Tuple (mkString (getIdent (nameValue f)))
            (go (ExprIdent z (QualifiedName { qualTok: nameTok f, qualModule: Nothing, qualName: nameValue f })))
          RecordField f _ v -> Tuple (lblName' f) (go v)
        vals = case bs of
          Just (Separated { sepHead: x, sepTail: xs }) -> Array.cons (lbl x) (map (lbl <<< snd) xs)
          Nothing -> []
      in positioned ann (AST.Literal (fst ann) (ObjectLiteral vals))
    ExprParens _ (Wrapped { wrpOpen: a, wrpValue: b, wrpClose: c }) ->
      positioned (sourceAnnCommented fileName a c) (AST.Parens (go b))
    expr@(ExprTyped _ a _ b) ->
      let
        a' = go a
        b' = convertType fileName b
        ann = Tuple (sourceSpan fileName (toSourceRange (exprRange expr))) []
      in positioned ann (AST.TypedValue true a' b')
    expr@(ExprInfix _ a (Wrapped { wrpValue: b }) c) ->
      let ann = Tuple (sourceSpan fileName (toSourceRange (exprRange expr))) []
      in positioned ann (AST.BinaryNoParens (go b) (go a) (go c))
    expr@(ExprOp _ _ _ _) ->
      let
        ann = uncurry (sourceAnn fileName) (exprRange expr)
        reassoc op b a =
          let op' = AST.Op (sourceSpan fileName (toSourceRange (qualRange op))) (qualified op)
          in AST.BinaryNoParens op' (go a) b
        loop k = case _ of
          ExprOp _ a op b -> loop (reassoc op (k b)) a
          expr' -> k expr'
      in positioned ann (loop go expr)
    ExprOpName _ op ->
      let
        rng = qualRange op
        op' = AST.Op (sourceSpan fileName (toSourceRange rng)) (qualified op)
      in positioned (uncurry (sourceAnnCommented fileName) rng) op'
    expr@(ExprNegate _ _ b) ->
      let ann = uncurry (sourceAnnCommented fileName) (exprRange expr)
      in positioned ann (AST.UnaryMinus (fst ann) (go b))
    expr@(ExprRecordAccessor _ (RecordAccessor { recExpr: a, recDot: _, recPath: Separated { sepHead: h, sepTail: t } })) ->
      let
        ann = uncurry (sourceAnnCommented fileName) (exprRange expr)
        field x f = AST.Accessor (lblName' f) x
      in positioned ann (Array.foldl (\x (Tuple _ f) -> field x f) (field (go a) h) t)
    expr@(ExprRecordUpdate _ a b) ->
      let
        ann = uncurry (sourceAnnCommented fileName) (exprRange expr)
        k (RecordUpdateLeaf f _ x) = Tuple (lblName' f) (AST.Leaf (go x))
        k (RecordUpdateBranch f xs) = Tuple (lblName' f) (AST.Branch (toTree xs))
        toTree (Wrapped { wrpValue: xs }) = AST.PathTree (AST.AssocList (map k (sepToList xs)))
      in positioned ann (AST.ObjectUpdateNested (go a) (toTree b))
    expr@(ExprApp _ a b) ->
      let ann = uncurry (sourceAnn fileName) (exprRange expr)
      in positioned ann (AST.App (go a) (go b))
    expr@(ExprVisibleTypeApp _ a _ b) ->
      let ann = uncurry (sourceAnn fileName) (exprRange expr)
      in positioned ann (AST.VisibleTypeApp (go a) (convertVtaType fileName b))
    expr@(ExprLambda _ (Lambda { lmbSymbol: _, lmbBinders: as, lmbArr: _, lmbBody: b })) ->
      let ann = uncurry (sourceAnnCommented fileName) (exprRange expr)
      in positioned ann
          (AST.Abs (convertBinder fileName (NEL.head as))
            (Array.foldr (AST.Abs <<< convertBinder fileName) (go b) (Array.fromFoldable (NEL.tail as))))
    expr@(ExprIf _ (IfThenElse { iteIf: _, iteCond: a, iteThen: _, iteTrue: b, iteElse: _, iteFalse: c })) ->
      let ann = uncurry (sourceAnnCommented fileName) (exprRange expr)
      in positioned ann (AST.IfThenElse (go a) (go b) (go c))
    expr@(ExprCase _ (CaseOf { caseKeyword: _, caseHead: as, caseOf: _, caseBranches: bs })) ->
      let
        ann = uncurry (sourceAnnCommented fileName) (exprRange expr)
        as' = map go (sepToList as)
        bs' = map (\(Tuple binders guarded) ->
          AST.CaseAlternative
            { caseAlternativeBinders: map (convertBinder fileName) (sepToList binders)
            , caseAlternativeResult: convertGuarded fileName guarded
            })
          (NEL.toUnfoldable bs)
      in positioned ann (AST.Case as' bs')
    expr@(ExprLet _ (LetIn { letKeyword: _, letBindings: as, letIn: _, letBody: b })) ->
      let ann = uncurry (sourceAnnCommented fileName) (exprRange expr)
      in positioned ann (AST.Let AST.FromLet (map (convertLetBinding fileName) (NEL.toUnfoldable as)) (go b))
    expr@(ExprDo _ (DoBlock { doKeyword: kw, doStatements: stmts })) ->
      let ann = uncurry (sourceAnnCommented fileName) (exprRange expr)
      in positioned ann (AST.Do (modName (tokValue kw)) (map goDoStatement (NEL.toUnfoldable stmts)))
    expr@(ExprAdo _ (AdoBlock { adoKeyword: kw, adoStatements: stms, adoIn: _, adoResult: a })) ->
      let ann = uncurry (sourceAnnCommented fileName) (exprRange expr)
      in positioned ann (AST.Ado (modName (tokValue kw)) (map goDoStatement stms) (go a))

  lblName' :: Label -> PSString
  lblName' (Label l) = l.lblName

  tokValue :: SourceToken -> Token
  tokValue (SourceToken { tokValue: v }) = v

-- ---------------------------------------------------------------------------
-- Convert binders

convertBinder :: forall a. String -> Binder a -> AST.Binder
convertBinder fileName = go
  where
  positioned = uncurry AST.PositionedBinder

  go = defer \_ -> case _ of
    BinderWildcard _ a ->
      positioned (sourceAnnCommented fileName a a) AST.NullBinder
    BinderVar _ a ->
      let ann = sourceName fileName a
      in positioned ann (AST.VarBinder (fst ann) (ident (nameValue a)))
    binder@(BinderNamed _ a _ b) ->
      let ann = uncurry (sourceAnnCommented fileName) (binderRange binder)
      in positioned ann (AST.NamedBinder (fst ann) (ident (nameValue a)) (go b))
    binder@(BinderConstructor _ a bs) ->
      let ann = uncurry (sourceAnnCommented fileName) (binderRange binder)
      in positioned ann (AST.ConstructorBinder (fst ann) (qualified a) (map go bs))
    BinderBoolean _ a b ->
      let ann = sourceAnnCommented fileName a a
      in positioned ann (AST.LiteralBinder (fst ann) (BooleanLiteral b))
    BinderChar _ a b ->
      let ann = sourceAnnCommented fileName a a
      in positioned ann (AST.LiteralBinder (fst ann) (CharLiteral b))
    BinderString _ a b ->
      let ann = sourceAnnCommented fileName a a
      in positioned ann (AST.LiteralBinder (fst ann) (StringLiteral b))
    BinderNumber _ n a b ->
      let
        ann = sourceAnnCommented fileName a a
        b' = if isJust n then bimap negate negate b else b
      in positioned ann (AST.LiteralBinder (fst ann) (NumericLiteral b'))
    BinderArray _ (Wrapped { wrpOpen: a, wrpValue: bs, wrpClose: c }) ->
      let
        ann = sourceAnnCommented fileName a c
        vals = case bs of
          Just (Separated { sepHead: x, sepTail: xs }) -> Array.cons (go x) (map (go <<< snd) xs)
          Nothing -> []
      in positioned ann (AST.LiteralBinder (fst ann) (ArrayLiteral vals))
    BinderRecord z (Wrapped { wrpOpen: a, wrpValue: bs, wrpClose: c }) ->
      let
        ann = sourceAnnCommented fileName a c
        lbl = case _ of
          RecordPun f ->
            Tuple (mkString (getIdent (nameValue f)))
              (go (BinderVar z f))
          RecordField f _ v -> Tuple (lblName' f) (go v)
        vals = case bs of
          Just (Separated { sepHead: x, sepTail: xs }) -> Array.cons (lbl x) (map (lbl <<< snd) xs)
          Nothing -> []
      in positioned ann (AST.LiteralBinder (fst ann) (ObjectLiteral vals))
    BinderParens _ (Wrapped { wrpOpen: a, wrpValue: b, wrpClose: c }) ->
      positioned (sourceAnnCommented fileName a c) (AST.ParensInBinder (go b))
    binder@(BinderTyped _ a _ b) ->
      let
        a' = go a
        b' = convertType fileName b
        ann = Tuple (sourceSpan fileName (toSourceRange (binderRange binder))) []
      in positioned ann (AST.TypedBinder b' a')
    binder@(BinderOp _ _ _ _) ->
      let
        ann = uncurry (sourceAnn fileName) (binderRange binder)
        reassoc op b a =
          let op' = AST.OpBinder (sourceSpan fileName (toSourceRange (qualRange op))) (qualified op)
          in AST.BinaryNoParensBinder op' (go a) b
        loop k = case _ of
          BinderOp _ a op b -> loop (reassoc op (k b)) a
          binder' -> k binder'
      in positioned ann (loop go binder)

  lblName' :: Label -> PSString
  lblName' (Label l) = l.lblName

bimap :: forall a b c d. (a -> b) -> (c -> d) -> Either a c -> Either b d
bimap f _ (Left a) = Left (f a)
bimap _ g (Right c) = Right (g c)

-- ---------------------------------------------------------------------------
-- Convert declarations

convertDeclaration :: forall a. String -> Declaration a -> Array AST.Declaration
convertDeclaration fileName decl = case decl of
  DeclData _ (DataHead { dataHdKeyword: _, dataHdName: a, dataHdVars: vars }) bd ->
    let
      ctrs :: SourceToken -> DataCtor a -> Array (Tuple SourceToken (DataCtor a)) -> Array AST.DataConstructorDeclaration
      ctrs st (DataCtor { dataCtorAnn: _, dataCtorName: name, dataCtorFields: fields }) tl =
        Array.cons
          (AST.DataConstructorDeclaration
            { dataCtorAnn: sourceAnnCommented fileName st (nameTok name)
            , dataCtorName: nameValue name
            , dataCtorFields: Array.mapWithIndex (\i ty -> Tuple (N.Ident ("value" <> show i)) ty) (map (convertType fileName) fields)
            })
          (case Array.uncons tl of
            Nothing -> []
            Just { head: Tuple st' ctor, tail: tl' } -> ctrs st' ctor tl')
    in
      [AST.DataDeclaration ann Env.Data (nameValue a) (map goTypeVar vars)
        (case bd of
          Nothing -> []
          Just (Tuple st (Separated { sepHead: hd, sepTail: t })) -> ctrs st hd t)]
  DeclType _ (DataHead { dataHdKeyword: _, dataHdName: a, dataHdVars: vars }) _ bd ->
    [AST.TypeSynonymDeclaration ann (nameValue a) (map goTypeVar vars) (convertType fileName bd)]
  DeclNewtype _ (DataHead { dataHdKeyword: _, dataHdName: a, dataHdVars: vars }) st x ys ->
    let
      ctrs = [AST.DataConstructorDeclaration
        { dataCtorAnn: sourceAnnCommented fileName st (snd (declRange decl))
        , dataCtorName: nameValue x
        , dataCtorFields: [Tuple (N.Ident "value0") (convertType fileName ys)]
        }]
    in [AST.DataDeclaration ann Env.Newtype (nameValue a) (map goTypeVar vars) ctrs]
  DeclClass _ (ClassHead { clsKeyword: _, clsSuper: sup, clsName: name, clsVars: vars, clsFundeps: fdeps }) bd ->
    let
      goTyVar tvb = case tvb of
        TypeVarKinded (Wrapped { wrpValue: Labeled { lblLabel: Tuple _ a } }) -> nameValue a
        TypeVarName (Tuple _ a) -> nameValue a
      vars' = Array.mapWithIndex (\i tvb -> Tuple (goTyVar tvb) i) vars
      goName n = case Array.findIndex (\(Tuple s _) -> s == n) vars' of
        Just _ -> unsafePartial $ fromJust $ map snd (Array.find (\(Tuple s _) -> s == n) vars')
        Nothing -> 0
      goFundep (FundepDetermined _ bs) =
        Env.FunctionalDependency { fdDeterminers: [], fdDetermined: map (goName <<< nameValue) (NEL.toUnfoldable bs) }
      goFundep (FundepDetermines as _ bs) =
        Env.FunctionalDependency
          { fdDeterminers: map (goName <<< nameValue) (NEL.toUnfoldable as)
          , fdDetermined: map (goName <<< nameValue) (NEL.toUnfoldable bs)
          }
      goSig (Labeled { lblLabel: n, lblSep: _, lblValue: ty }) =
        let
          ty' = convertType fileName ty
          ann' = widenLeft (tokAnn (nameTok n)) (T.getAnnForType ty')
        in AST.TypeDeclaration (AST.TypeDeclarationData
          { tydeclSourceAnn: ann'
          , tydeclIdent: ident (nameValue n)
          , tydeclType: ty'
          })
    in [AST.TypeClassDeclaration ann
        (nameValue name)
        (map goTypeVar vars)
        (convertConstraint false fileName <$> maybe [] (Array.fromFoldable <<< fst) sup)
        (goFundep <$> maybe [] (Array.toUnfoldable <<< sepToList <<< snd) fdeps)
        (goSig <$> maybe [] (NEL.toUnfoldable <<< snd) bd)]
  DeclInstanceChain _ insts ->
    let
      chainId = mkChainId fileName (startSourcePos (instKeyword (instHead (sepHead insts))))
      goInst ix inst@(Instance { instHead: (InstanceHead { instKeyword: _, instNameSep: nameSep, instConstraints: ctrs, instClass: cls, instTypes: args }), instBody: bd }) =
        let
          ann' = uncurry (sourceAnnCommented fileName) (instanceRange inst)
          clsAnn = findInstanceAnn cls args
        in AST.TypeInstanceDeclaration ann' clsAnn chainId ix
            (mkPartialInstanceName nameSep cls args)
            (convertConstraint false fileName <$> maybe [] (Array.fromFoldable <<< fst) ctrs)
            (qualified cls)
            (map (convertType fileName) args)
            (AST.ExplicitInstance (goInstanceBinding <$> maybe [] (NEL.toUnfoldable <<< snd) bd))
    in Array.mapWithIndex (\ix inst -> goInst ix inst) (sepToList insts)
  DeclDerive _ _ new (InstanceHead { instKeyword: kw, instNameSep: nameSep, instConstraints: ctrs, instClass: cls, instTypes: args }) ->
    let
      chainId = mkChainId fileName (startSourcePos kw)
      name' = mkPartialInstanceName nameSep cls args
      instTy = if isJust new then AST.NewtypeInstance else AST.DerivedInstance
      clsAnn = findInstanceAnn cls args
    in [AST.TypeInstanceDeclaration ann clsAnn chainId 0 name'
        (convertConstraint false fileName <$> maybe [] (Array.fromFoldable <<< fst) ctrs)
        (qualified cls)
        (map (convertType fileName) args)
        instTy]
  DeclKindSignature _ kw (Labeled { lblLabel: name, lblSep: _, lblValue: ty }) ->
    let
      kindFor = case tokValue kw of
        TokLowerName [] "data"    -> AST.DataSig
        TokLowerName [] "newtype" -> AST.NewtypeSig
        TokLowerName [] "type"    -> AST.TypeSynonymSig
        TokLowerName [] "class"   -> AST.ClassSig
        tok -> unsafePartial $ error ("Invalid kind signature keyword " <> printToken tok)
    in [AST.KindDeclaration ann kindFor (nameValue name) (convertType fileName ty)]
  DeclSignature _ lbl ->
    [convertSignature fileName lbl]
  DeclValue _ fields ->
    [convertValueBindingFields fileName ann fields]
  DeclFixity _ (FixityFields { fxtKeyword: Tuple _ kw, fxtPrec: Tuple _ prec, fxtOp: fxop }) ->
    let
      assoc = case kw of
        Infix  -> AST.Infix
        Infixl -> AST.Infixl
        Infixr -> AST.Infixr
      fixity = AST.Fixity assoc prec
    in [AST.FixityDeclaration ann $ case fxop of
        FixityValue name _ op ->
          Left $ AST.ValueFixity fixity (map (first ident) (qualified name)) (nameValue op)
        FixityType _ name _ op ->
          Right $ AST.TypeFixity fixity (qualified name) (nameValue op)]
  DeclForeign _ _ _ frn ->
    [case frn of
      ForeignValue (Labeled { lblLabel: a, lblSep: _, lblValue: b }) ->
        AST.ExternDeclaration ann (ident (nameValue a)) (convertType fileName b)
      ForeignData _ (Labeled { lblLabel: a, lblSep: _, lblValue: b }) ->
        AST.ExternDataDeclaration ann (nameValue a) (convertType fileName b)
      ForeignKind _ a ->
        AST.DataDeclaration ann Env.Data (nameValue a) [] []]
  DeclRole _ _ _ name roles ->
    [AST.RoleDeclaration $
      AST.RoleDeclarationData
        { rdeclSourceAnn: ann
        , rdeclIdent: nameValue name
        , rdeclRoles: map (\(Role { roleValue: rv }) -> rv) (NEL.toUnfoldable roles)
        }]
  where
  ann = uncurry (sourceAnnCommented fileName) (declRange decl)

  tokValue :: SourceToken -> Token
  tokValue (SourceToken { tokValue: v }) = v

  mkPartialInstanceName
    :: forall b
     . Maybe (Tuple (Name Ident) SourceToken)
    -> QualifiedName (N.ProperName N.ClassName)
    -> Array (Type b)
    -> Either String N.Ident
  mkPartialInstanceName nameSep cls args =
    maybe (Left genName) (Right <<< ident <<< nameValue <<< fst) nameSep
    where
    genName :: String
    genName = Str.take 25 (className <> typeArgs)

    className :: String
    className =
      case SCU.uncons (N.runProperName (qualName cls)) of
        Nothing -> ""
        Just { head: c, tail: rest } -> SCU.singleton (lowercaseChar c) <> rest

    lowercaseChar :: Char -> Char
    lowercaseChar c =
      case SCU.uncons (Str.toLower (SCU.singleton c)) of
        Nothing -> c
        Just { head: c' } -> c'

    typeArgs :: String
    typeArgs = Array.foldl (\acc t -> acc <> argName t) "" args

    argName :: forall b. Type b -> String
    argName = case _ of
      TypeVar _ _ -> ""
      TypeConstructor _ qn -> N.runProperName (qualName qn)
      TypeOpName _ qn -> N.runOpName (qualName qn)
      TypeString _ _ ps -> prettyPrintStringJS ps
      TypeInt _ _ _ nt -> show nt
      TypeHole _ _ -> ""
      TypeParens _ t -> argName (wrpValue t)
      TypeKinded _ t1 _ _ -> argName t1
      TypeRecord _ _ -> "Record"
      TypeRow _ _ -> "Row"
      TypeArrName _ _ -> "Function"
      TypeWildcard _ _ -> "_"
      TypeForall _ _ _ _ _ -> ""
      TypeApp _ t1 t2 -> argName t1 <> argName t2
      TypeOp _ t1 op t2 -> argName t1 <> N.runOpName (qualName op) <> argName t2
      TypeArr _ t1 _ t2 -> argName t1 <> "Function" <> argName t2
      TypeConstrained _ _ _ _ -> ""
      TypeUnaryRow _ _ _ -> "Row"

  goTypeVar :: forall b. TypeVarBinding b -> Tuple String (Maybe T.SourceType)
  goTypeVar = case _ of
    TypeVarKinded (Wrapped { wrpValue: Labeled { lblLabel: Tuple _ x, lblValue: y } }) ->
      Tuple (getIdent (nameValue x)) (Just (convertType fileName y))
    TypeVarName (Tuple _ x) ->
      Tuple (getIdent (nameValue x)) Nothing

  goInstanceBinding :: forall b. InstanceBinding b -> AST.Declaration
  goInstanceBinding = case _ of
    InstanceBindingSignature _ lbl ->
      convertSignature fileName lbl
    binding@(InstanceBindingName _ fields) ->
      let ann' = uncurry (sourceAnnCommented fileName) (instanceBindingRange binding)
      in convertValueBindingFields fileName ann' fields

  findInstanceAnn :: forall b. QualifiedName (N.ProperName N.ClassName) -> Array (Type b) -> SourceAnn
  findInstanceAnn cls args =
    uncurry (sourceAnnCommented fileName) $
      if Array.null args then qualRange cls
      else case Array.last args of
        Nothing -> qualRange cls
        Just lastArg -> Tuple (fst (qualRange cls)) (snd (typeRange lastArg))

foreign import error :: forall a. String -> a

convertSignature :: forall a. String -> Labeled (Name Ident) (Type a) -> AST.Declaration
convertSignature fileName (Labeled { lblLabel: a, lblSep: _, lblValue: b }) =
  let
    b' = convertType fileName b
    ann = widenLeft (tokAnn (nameTok a)) (T.getAnnForType b')
  in AST.TypeDeclaration (AST.TypeDeclarationData
    { tydeclSourceAnn: ann
    , tydeclIdent: ident (nameValue a)
    , tydeclType: b'
    })

convertValueBindingFields :: forall a. String -> SourceAnn -> ValueBindingFields a -> AST.Declaration
convertValueBindingFields fileName ann (ValueBindingFields { valName: a, valBinders: bs, valGuarded: c }) =
  let
    bs' = map (convertBinder fileName) bs
    cs' = convertGuarded fileName c
  in AST.ValueDeclaration (AST.ValueDeclarationData
    { valdeclSourceAnn: ann
    , valdeclIdent: ident (nameValue a)
    , valdeclName: Env.Public
    , valdeclBinders: bs'
    , valdeclExpression: cs'
    })

convertImportDecl
  :: forall a
   . String
  -> ImportDecl a
  -> Tuple SourceAnn (Tuple N.ModuleName (Tuple AST.ImportDeclarationType (Maybe N.ModuleName)))
convertImportDecl fileName decl@(ImportDecl { impKeyword: _, impModule: modName, impNames: mbNames, impQual: mbQual }) =
  let
    ann = uncurry (sourceAnnCommented fileName) (importDeclRange decl)
    importTy = case mbNames of
      Nothing -> AST.Implicit
      Just (Tuple hiding (Wrapped { wrpValue: imps })) ->
        let imps' = map (convertImport fileName) (sepToList imps)
        in if isJust hiding then AST.Hiding imps' else AST.Explicit imps'
  in Tuple ann (Tuple (nameValue modName) (Tuple importTy (map (nameValue <<< snd) mbQual)))

convertImport :: forall a. String -> Import a -> AST.DeclarationRef
convertImport fileName imp = case imp of
  ImportValue _ a ->
    AST.ValueRef ann (ident (nameValue a))
  ImportOp _ a ->
    AST.ValueOpRef ann (nameValue a)
  ImportType _ a mb ->
    let
      ctrs = case mb of
        Nothing -> Just []
        Just (DataAll _ _) -> Nothing
        Just (DataEnumerated _ (Wrapped { wrpValue: Nothing })) -> Just []
        Just (DataEnumerated _ (Wrapped { wrpValue: Just idents })) ->
          Just (map nameValue (sepToList idents))
    in AST.TypeRef ann (nameValue a) ctrs
  ImportTypeOp _ _ a ->
    AST.TypeOpRef ann (nameValue a)
  ImportClass _ _ a ->
    AST.TypeClassRef ann (nameValue a)
  where
  ann = sourceSpan fileName (toSourceRange (importRange imp))

convertExport :: forall a. String -> Export a -> AST.DeclarationRef
convertExport fileName export = case export of
  ExportValue _ a ->
    AST.ValueRef ann (ident (nameValue a))
  ExportOp _ a ->
    AST.ValueOpRef ann (nameValue a)
  ExportType _ a mb ->
    let
      ctrs = case mb of
        Nothing -> Just []
        Just (DataAll _ _) -> Nothing
        Just (DataEnumerated _ (Wrapped { wrpValue: Nothing })) -> Just []
        Just (DataEnumerated _ (Wrapped { wrpValue: Just idents })) ->
          Just (map nameValue (sepToList idents))
    in AST.TypeRef ann (nameValue a) ctrs
  ExportTypeOp _ _ a ->
    AST.TypeOpRef ann (nameValue a)
  ExportClass _ _ a ->
    AST.TypeClassRef ann (nameValue a)
  ExportModule _ _ a ->
    AST.ModuleRef ann (nameValue a)
  where
  ann = sourceSpan fileName (toSourceRange (exportRange export))

convertModule :: forall a. String -> Module a -> AST.Module
convertModule fileName module'@(Module { modKeyword: _, modNamespace: modNamespace, modExports: exps, modWhere: _, modImports: imps, modDecls: decls, modTrailingComments: _ }) =
  let
    ann = uncurry (sourceAnnCommented fileName) (moduleRange module')
    imps' = map importCtr imps
    decls' = Array.concatMap (convertDeclaration fileName) decls
    exps' = map (map (convertExport fileName) <<< sepToList <<< wrpValue) exps
  in uncurry AST.Module ann (nameValue modNamespace) (imps' <> decls') exps'
  where
  importCtr imp =
    let Tuple a (Tuple b (Tuple c d)) = convertImportDecl fileName imp
    in AST.ImportDeclaration a b c d

first :: forall a b c. (a -> b) -> Either a c -> Either b c
first f (Left a) = Left (f a)
first _ (Right c) = Right c
