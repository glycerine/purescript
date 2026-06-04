module Language.PureScript.CST.Parser
  ( parseType
  , parseExpr
  , parseDecl
  , parseIdent
  , parseOperator
  , parseModule
  , parseImportDeclP
  , parseDeclP
  , parseExprP
  , parseTypeP
  , parseModuleNameP
  , parseQualIdentP
  , parse
  , PartialResult(..)
  ) where

import Prelude

import Control.Lazy (defer)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (cons, foldl, length, null, reverse, toUnfoldable) as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (fromFoldable, singleton, toUnfoldable) as NEL
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Language.PureScript.CST.Errors
  ( ParserError
  , ParserErrorType(..)
  , ParserWarning
  )
import Language.PureScript.CST.Flatten (flattenType)
import Language.PureScript.CST.Monad
  ( LexResult
  , Parser
  , ParserM(..)
  , ParserState(..)
  , addFailure
  , addWarning
  , munch
  , oneOf
  , parseFail
  , parseFail'
  , pushBack
  , runParser
  , runTokenParser
  , tryPrefix
  )
import Language.PureScript.CST.Positions (toSourceRange, whereRange, guardedRange)
import Language.PureScript.CST.Types
  ( AdoBlock(..)
  , Binder(..)
  , CaseOf(..)
  , ClassFundep(..)
  , Ident(..)
  , ClassHead(..)
  , Constraint(..)
  , DataCtor(..)
  , DataHead(..)
  , DataMembers(..)
  , Declaration(..)
  , DoBlock(..)
  , DoStatement(..)
  , Export(..)
  , Expr(..)
  , Fixity(..)
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
import Language.PureScript.CST.Utils
  ( QualifiedOpName
  , QualifiedProperName
  , ProperName
  , OpName
  , TmpModuleDecl(..)
  , checkFundeps
  , checkNoWildcards
  , checkNoForalls
  , getOpName
  , getProperName
  , getQualifiedOpName
  , getQualifiedProperName
  , isConstrained
  , isLeftFatArrow
  , lblTok
  , placeholder
  , qualifiedOpName
  , qualifiedProperName
  , opName
  , properName
  , separated
  , toBinderConstructor
  , toBoolean
  , toChar
  , toConstraint
  , toInt
  , toLabel
  , toModuleDecls
  , toName
  , toNumber
  , toQualifiedName
  , toRecordFields
  , toString
  , unexpectedName
  , upperToModuleName
  )
import Language.PureScript.CST.Lexer (lexModule)
import Language.PureScript.Names (Ident(..), ModuleName(..), OpName(..), ProperName(..), TypeName, ClassName) as N
import Language.PureScript.PSString (PSString)
import Language.PureScript.Roles (Role(..)) as R
import Partial.Unsafe (unsafePartial)

-- ---------------------------------------------------------------------------
-- PartialResult

data PartialResult a = PartialResult
  { resPartial :: a
  , resFull    :: Tuple (Array ParserWarning) (Either (NonEmptyList ParserError) a)
  }

instance functorPartialResult :: Functor PartialResult where
  map f (PartialResult r) = PartialResult
    { resPartial: f r.resPartial
    , resFull: map (map f) r.resFull
    }

-- ---------------------------------------------------------------------------
-- Lexer bridge

lexer :: forall a. (SourceToken -> Parser a) -> Parser a
lexer k = munch >>= k

-- ---------------------------------------------------------------------------
-- Low-level token matchers

tokValue :: SourceToken -> Token
tokValue (SourceToken { tokValue: v }) = v

tryTok :: (Token -> Boolean) -> Parser (Maybe SourceToken)
tryTok pred = do
  t <- munch
  if pred (tokValue t)
    then pure (Just t)
    else pushBack t *> pure Nothing

expectTok :: (Token -> Boolean) -> ParserErrorType -> Parser SourceToken
expectTok pred err = do
  t <- munch
  if pred (tokValue t)
    then pure t
    else parseFail t err

tokEof :: Token -> Boolean
tokEof TokEof = true
tokEof _      = false

tokLayoutStart :: Token -> Boolean
tokLayoutStart TokLayoutStart = true
tokLayoutStart _ = false

tokLayoutEnd :: Token -> Boolean
tokLayoutEnd TokLayoutEnd = true
tokLayoutEnd _ = false

tokLayoutSep :: Token -> Boolean
tokLayoutSep TokLayoutSep = true
tokLayoutSep _ = false

tokLeftParen :: Token -> Boolean
tokLeftParen TokLeftParen = true
tokLeftParen _ = false

tokRightParen :: Token -> Boolean
tokRightParen TokRightParen = true
tokRightParen _ = false

tokLeftBrace :: Token -> Boolean
tokLeftBrace TokLeftBrace = true
tokLeftBrace _ = false

tokRightBrace :: Token -> Boolean
tokRightBrace TokRightBrace = true
tokRightBrace _ = false

tokLeftSquare :: Token -> Boolean
tokLeftSquare TokLeftSquare = true
tokLeftSquare _ = false

tokRightSquare :: Token -> Boolean
tokRightSquare TokRightSquare = true
tokRightSquare _ = false

tokComma :: Token -> Boolean
tokComma TokComma = true
tokComma _ = false

tokPipe :: Token -> Boolean
tokPipe TokPipe = true
tokPipe _ = false

tokDot :: Token -> Boolean
tokDot TokDot = true
tokDot _ = false

tokBackslash :: Token -> Boolean
tokBackslash TokBackslash = true
tokBackslash _ = false

tokUnderscore :: Token -> Boolean
tokUnderscore TokUnderscore = true
tokUnderscore _ = false

tokTick :: Token -> Boolean
tokTick TokTick = true
tokTick _ = false

tokEquals :: Token -> Boolean
tokEquals TokEquals = true
tokEquals _ = false

tokDoubleColon :: Token -> Boolean
tokDoubleColon (TokDoubleColon _) = true
tokDoubleColon _ = false

tokLeftArrow :: Token -> Boolean
tokLeftArrow (TokLeftArrow _) = true
tokLeftArrow _ = false

tokRightArrow :: Token -> Boolean
tokRightArrow (TokRightArrow _) = true
tokRightArrow _ = false

tokRightFatArrow :: Token -> Boolean
tokRightFatArrow (TokRightFatArrow _) = true
tokRightFatArrow _ = false

tokForall :: Token -> Boolean
tokForall (TokForall _) = true
tokForall _ = false

tokColon :: Token -> Boolean
tokColon (TokOperator [] ":") = true
tokColon _ = false

tokLeftFatArrow :: Token -> Boolean
tokLeftFatArrow (TokOperator [] sym) = isLeftFatArrow sym
tokLeftFatArrow _ = false

tokSymbolArr :: Token -> Boolean
tokSymbolArr (TokSymbolArr _) = true
tokSymbolArr _ = false

tokDotDot :: Token -> Boolean
tokDotDot (TokSymbolName [] "..") = true
tokDotDot _ = false

tokMinus :: Token -> Boolean
tokMinus (TokOperator [] "-") = true
tokMinus _ = false

tokAt :: Token -> Boolean
tokAt (TokOperator [] "@") = true
tokAt _ = false

tokLowerName :: Array String -> String -> Token -> Boolean
tokLowerName qual name (TokLowerName q n) = q == qual && n == name
tokLowerName _ _ _ = false

tokLowerNameAny :: Array String -> Token -> Boolean
tokLowerNameAny qual (TokLowerName q _) = q == qual
tokLowerNameAny _ _ = false

tokKeyword :: String -> Token -> Boolean
tokKeyword kw (TokLowerName [] n) = n == kw
tokKeyword _ _ = false

tokKeywordAny :: String -> Token -> Boolean
tokKeywordAny kw (TokLowerName _ n) = n == kw
tokKeywordAny _ _ = false

tokUpperName :: Token -> Boolean
tokUpperName (TokUpperName _ _) = true
tokUpperName _ = false

tokUpperNameUnqual :: Token -> Boolean
tokUpperNameUnqual (TokUpperName [] _) = true
tokUpperNameUnqual _ = false

tokOperator :: Token -> Boolean
tokOperator (TokOperator [] _) = true
tokOperator _ = false

tokOperatorAny :: Token -> Boolean
tokOperatorAny (TokOperator _ _) = true
tokOperatorAny _ = false

tokSymbolName :: Token -> Boolean
tokSymbolName (TokSymbolName [] _) = true
tokSymbolName _ = false

tokSymbolNameAny :: Token -> Boolean
tokSymbolNameAny (TokSymbolName _ _) = true
tokSymbolNameAny _ = false

tokHole :: Token -> Boolean
tokHole (TokHole _) = true
tokHole _ = false

tokLitChar :: Token -> Boolean
tokLitChar (TokChar _ _) = true
tokLitChar _ = false

tokLitString :: Token -> Boolean
tokLitString (TokString _ _) = true
tokLitString (TokRawString _) = true
tokLitString _ = false

tokLitInt :: Token -> Boolean
tokLitInt (TokInt _ _) = true
tokLitInt _ = false

tokLitNumber :: Token -> Boolean
tokLitNumber (TokNumber _ _) = true
tokLitNumber _ = false

-- ---------------------------------------------------------------------------
-- Combinators

expect :: (Token -> Boolean) -> Parser SourceToken
expect pred = do
  t <- munch
  if pred (tokValue t) then pure t else parseFail t ErrToken

many1 :: forall a. Parser a -> Parser (NonEmptyList a)
many1 pa = do
  x <- pa
  go [x]
  where
  go acc = do
    mb <- tryOnce pa
    case mb of
      Just x  -> go (Array.cons x acc)
      Nothing -> pure (unsafePartial fromJust (NEL.fromFoldable (Array.reverse acc)))

tryOnce :: forall a. Parser a -> Parser (Maybe a)
tryOnce pa =
  let parsers = unsafePartial fromJust $ NEL.fromFoldable
        [ map Just pa
        , pure Nothing
        ]
  in oneOf parsers

-- | Stack-safe many0: uses tailRec + direct CPS instantiation so each iteration
-- | runs at bounded depth rather than O(N) accumulated CPS frames.
many0 :: forall a. Parser a -> Parser (Array a)
many0 pa = Parser \initSt _ ksucc ->
  let Parser tryOnceK = tryOnce pa
      runOnce st = tryOnceK st (\st' _ -> Tuple st' Nothing) (\st' mb -> Tuple st' mb)
      step (Tuple st acc) = case runOnce st of
        Tuple st' (Just x) -> Loop (Tuple st' (Array.cons x acc))
        Tuple st' Nothing  -> Done (Tuple st' (Array.reverse acc))
      Tuple finalSt result = tailRec step (Tuple initSt [])
  in ksucc finalSt result

-- | Stack-safe sep1Acc: uses tailRec + direct CPS instantiation.
-- | The first element has placeholder as separator (reversed accumulator).
sep1Acc :: forall a. Parser a -> Parser SourceToken -> Parser (Array (Tuple SourceToken a))
sep1Acc pa ps = do
  x <- pa
  Parser \st kerr ksucc ->
    let Parser tryPsK = tryOnce ps
        Parser paK_   = pa
        runTryPs st' = tryPsK st' (\st'' _ -> Tuple st'' Nothing) (\st'' mb -> Tuple st'' mb)
        runPa    st' = paK_   st' (\st'' err -> Tuple st'' (Left err)) (\st'' item -> Tuple st'' (Right item))
        step (Tuple st' acc) = case runTryPs st' of
          Tuple st'' Nothing    -> Done (Tuple st'' (Right acc))
          Tuple st'' (Just sep) -> case runPa st'' of
            Tuple st''' (Right item) -> Loop (Tuple st''' (Array.cons (Tuple sep item) acc))
            Tuple st''' (Left err)   -> Done (Tuple st''' (Left err))
        Tuple finalSt result = tailRec step (Tuple st [Tuple placeholder x])
    in case result of
      Right finalAcc -> ksucc finalSt finalAcc
      Left err       -> kerr finalSt err

parseSep :: forall a. Parser a -> Parser SourceToken -> Parser (Separated a)
parseSep pa ps = do
  acc <- sep1Acc pa ps
  pure (separated acc)

delim :: forall a. (Token -> Boolean) -> (Token -> Boolean) -> (Token -> Boolean) -> Parser a -> Parser (Wrapped (Maybe (Separated a)))
delim open close sep pa = do
  o <- expect open
  mbSep <- tryOnce (parseSep pa (expect sep))
  c <- expect close
  pure (Wrapped { wrpOpen: o, wrpValue: mbSep, wrpClose: c })

delim1 :: forall a. (Token -> Boolean) -> (Token -> Boolean) -> (Token -> Boolean) -> Parser a -> Parser (Wrapped (Separated a))
delim1 open close sep pa = do
  o <- expect open
  s <- parseSep pa (expect sep)
  c <- expect close
  pure (Wrapped { wrpOpen: o, wrpValue: s, wrpClose: c })

-- ---------------------------------------------------------------------------
-- Module name

parseModuleName :: Parser (Name N.ModuleName)
parseModuleName = do
  t <- expect tokUpperName
  upperToModuleName t

parseQualProperName :: Parser QualifiedProperName
parseQualProperName = do
  t <- expect tokUpperName
  qualifiedProperName <$> toQualifiedName N.ProperName t

parseProperName :: Parser ProperName
parseProperName = do
  t <- expect tokUpperNameUnqual
  properName <$> toName N.ProperName t

parseQualIdent :: Parser (QualifiedName Ident)
parseQualIdent = do
  t <- munch
  case tokValue t of
    TokLowerName _ _          -> toQualifiedName Ident t
    TokLowerName [] "as"      -> toQualifiedName Ident t
    TokLowerName [] "hiding"  -> toQualifiedName Ident t
    TokLowerName [] "role"    -> toQualifiedName Ident t
    TokLowerName [] "nominal" -> toQualifiedName Ident t
    TokLowerName [] "representational" -> toQualifiedName Ident t
    TokLowerName [] "phantom" -> toQualifiedName Ident t
    _ -> parseFail t ErrToken

parseIdent :: Parser (Name Ident)
parseIdent = do
  t <- munch
  case tokValue t of
    TokLowerName [] _               -> toName Ident t
    TokLowerName [] "as"            -> toName Ident t
    TokLowerName [] "hiding"        -> toName Ident t
    TokLowerName [] "role"          -> toName Ident t
    TokLowerName [] "nominal"       -> toName Ident t
    TokLowerName [] "representational" -> toName Ident t
    TokLowerName [] "phantom"       -> toName Ident t
    _ -> parseFail t ErrToken

parseQualOp :: Parser QualifiedOpName
parseQualOp = do
  t <- munch
  case tokValue t of
    TokOperator _ _     -> qualifiedOpName <$> toQualifiedName N.OpName t
    TokSymbolName _ _   -> qualifiedOpName <$> toQualifiedName N.OpName t
    TokOperator [] sym | isLeftFatArrow sym -> qualifiedOpName <$> toQualifiedName N.OpName t
    TokOperator [] "-"  -> qualifiedOpName <$> toQualifiedName N.OpName t
    TokOperator [] ":"  -> qualifiedOpName <$> toQualifiedName N.OpName t
    _ -> parseFail t ErrToken

parseOp :: Parser OpName
parseOp = do
  t <- munch
  case tokValue t of
    TokOperator [] _     -> opName <$> toName N.OpName t
    TokOperator [] sym | isLeftFatArrow sym -> opName <$> toName N.OpName t
    TokOperator [] "-"   -> opName <$> toName N.OpName t
    TokOperator [] ":"   -> opName <$> toName N.OpName t
    _ -> parseFail t ErrToken

parseQualSymbol :: Parser QualifiedOpName
parseQualSymbol = do
  t <- munch
  case tokValue t of
    TokSymbolName _ _   -> qualifiedOpName <$> toQualifiedName N.OpName t
    TokSymbolName [] ".." -> qualifiedOpName <$> toQualifiedName N.OpName t
    _ -> parseFail t ErrToken

parseSymbol :: Parser OpName
parseSymbol = do
  t <- munch
  case tokValue t of
    TokSymbolName [] _   -> opName <$> toName N.OpName t
    TokSymbolName [] ".." -> opName <$> toName N.OpName t
    _ -> parseFail t ErrToken

parseLabel :: Parser Label
parseLabel = do
  t <- munch
  case tokValue t of
    TokLowerName [] _   -> pure (toLabel t)
    TokString _ _       -> pure (toLabel t)
    TokRawString _      -> pure (toLabel t)
    TokLowerName [] kw | kw `isKeyword` keywordsAllowedAsLabel -> pure (toLabel t)
    _ -> parseFail t ErrToken
  where
  keywordsAllowedAsLabel =
    [ "ado", "as", "case", "class", "data", "derive", "do", "else"
    , "false", "forall", "foreign", "hiding", "import", "if", "in"
    , "infix", "infixl", "infixr", "instance", "let", "module", "newtype"
    , "nominal", "of", "phantom", "representational", "role", "then"
    , "true", "type", "where"
    ]
  isKeyword s arr = Array.length (Array.foldl (\a k -> if k == s then Array.cons k a else a) [] arr) > 0

parseHole :: Parser (Name Ident)
parseHole = do
  t <- expect tokHole
  toName Ident t

parseString :: Parser (Tuple SourceToken PSString)
parseString = do
  t <- munch
  case tokValue t of
    TokString _ _  -> pure (toString t)
    TokRawString _ -> pure (toString t)
    _ -> parseFail t ErrToken

parseChar :: Parser (Tuple SourceToken Char)
parseChar = do
  t <- expect tokLitChar
  pure (toChar t)

parseNumber :: Parser (Tuple SourceToken (Either Int Number))
parseNumber = do
  t <- munch
  case tokValue t of
    TokInt _ _    -> pure (toNumber t)
    TokNumber _ _ -> pure (toNumber t)
    _ -> parseFail t ErrToken

parseInt :: Parser (Tuple SourceToken Int)
parseInt = do
  t <- expect tokLitInt
  pure (toInt t)

parseBoolean :: Parser (Tuple SourceToken Boolean)
parseBoolean = do
  t <- munch
  case tokValue t of
    TokLowerName [] "true"  -> pure (toBoolean t)
    TokLowerName [] "false" -> pure (toBoolean t)
    _ -> parseFail t ErrToken

-- ---------------------------------------------------------------------------
-- Types

parseType :: Parser (Type Unit)
parseType = defer \_ -> do
  ty <- parseType1
  mbKind <- tryOnce (expect tokDoubleColon)
  case mbKind of
    Nothing  -> pure ty
    Just sep -> TypeKinded unit ty sep <$> parseType

parseType1 :: Parser (Type Unit)
parseType1 = do
  mbForall <- tryOnce (expect tokForall)
  case mbForall of
    Just fTok -> do
      vars <- many1 parseTypeVarBinding
      dot  <- expect tokDot
      ty   <- parseType1
      pure (TypeForall unit fTok vars dot ty)
    Nothing -> parseType2

parseType2 :: Parser (Type Unit)
parseType2 = defer \_ -> do
  ty <- parseType3
  mbNext <- tryOnce (expect tokRightArrow)
  case mbNext of
    Just arr -> TypeArr unit ty arr <$> parseType1
    Nothing -> do
      mbFat <- tryOnce (expect tokRightFatArrow)
      case mbFat of
        Just fat -> do
          cs <- toConstraint ty
          TypeConstrained unit cs fat <$> parseType1
        Nothing -> pure ty

parseType3 :: Parser (Type Unit)
parseType3 = do
  ty <- parseType4
  go ty
  where
  go ty = do
    mbOp <- tryOnce parseQualOp
    case mbOp of
      Just op -> do
        ty2 <- parseType4
        go (TypeOp unit ty (getQualifiedOpName op) ty2)
      Nothing -> pure ty

parseType4 :: Parser (Type Unit)
parseType4 = do
  mbMinus <- tryOnce (expect tokMinus)
  case mbMinus of
    Just minus -> do
      Tuple t i <- parseInt
      pure (TypeInt unit (Just minus) t (negate i))
    Nothing -> parseType5

parseType5 :: Parser (Type Unit)
parseType5 = do
  ty <- parseTypeAtom
  go ty
  where
  go ty = do
    mbArg <- tryOnce parseTypeAtom
    case mbArg of
      Just arg -> go (TypeApp unit ty arg)
      Nothing  -> pure ty

parseTypeAtom :: Parser (Type Unit)
parseTypeAtom = do
  t <- munch
  case tokValue t of
    TokUnderscore    -> pure (TypeWildcard unit t)
    TokHole _        -> pushBack t *> (TypeHole unit <$> parseHole)
    TokLowerName [] _ -> pushBack t *> (TypeVar unit <$> parseIdent)
    TokLowerName [] "as"  -> pushBack t *> (TypeVar unit <$> parseIdent)
    TokLowerName [] "hiding" -> pushBack t *> (TypeVar unit <$> parseIdent)
    TokLowerName [] "role" -> pushBack t *> (TypeVar unit <$> parseIdent)
    TokLowerName [] "nominal" -> pushBack t *> (TypeVar unit <$> parseIdent)
    TokLowerName [] "representational" -> pushBack t *> (TypeVar unit <$> parseIdent)
    TokLowerName [] "phantom" -> pushBack t *> (TypeVar unit <$> parseIdent)
    TokUpperName _ _ -> pushBack t *> do
      qpn <- parseQualProperName
      pure (TypeConstructor unit (getQualifiedProperName qpn))
    TokSymbolName _ _ -> pushBack t *> do
      qon <- parseQualSymbol
      pure (TypeOpName unit (getQualifiedOpName qon))
    TokSymbolName [] ".." -> pushBack t *> do
      qon <- parseQualSymbol
      pure (TypeOpName unit (getQualifiedOpName qon))
    TokString _ _ -> pushBack t *> do
      Tuple tok str <- parseString
      pure (TypeString unit tok str)
    TokRawString _ -> pushBack t *> do
      Tuple tok str <- parseString
      pure (TypeString unit tok str)
    TokInt _ _ -> pushBack t *> do
      Tuple tok i <- parseInt
      pure (TypeInt unit Nothing tok i)
    TokSymbolArr _ -> pure (TypeArrName unit t)
    TokLeftBrace -> do
      r <- parseRow
      c <- expect tokRightBrace
      pure (TypeRecord unit (Wrapped { wrpOpen: t, wrpValue: r, wrpClose: c }))
    TokLeftParen -> do
      mbRow <- tryOnce parseRow
      case mbRow of
        Just r@(Row rr) | isJust rr.rowLabels || isJust rr.rowTail -> do
          c <- expect tokRightParen
          pure (TypeRow unit (Wrapped { wrpOpen: t, wrpValue: r, wrpClose: c }))
        _ -> do
          -- Empty row or non-row: check for () or parenthesized type
          mbClose <- tryOnce (expect tokRightParen)
          case mbClose of
            Just c ->
              pure (TypeRow unit (Wrapped { wrpOpen: t, wrpValue: Row { rowLabels: Nothing, rowTail: Nothing }, wrpClose: c }))
            Nothing -> do
              ty <- parseType
              c <- expect tokRightParen
              pure (TypeParens unit (Wrapped { wrpOpen: t, wrpValue: ty, wrpClose: c }))
    _ -> parseFail t ErrToken

finishTypeParens :: SourceToken -> Type Unit -> Parser (Type Unit)
finishTypeParens o ty = do
  c <- expect tokRightParen
  pure (TypeParens unit (Wrapped { wrpOpen: o, wrpValue: ty, wrpClose: c }))

-- Parse a row (may be empty)
parseRow :: Parser (Row Unit)
parseRow = do
  mbPipe <- tryOnce (expect tokPipe)
  case mbPipe of
    Just pipe -> do
      ty <- parseType
      pure (Row { rowLabels: Nothing, rowTail: Just (Tuple pipe ty) })
    Nothing -> do
      mbLabels <- tryOnce (parseSep parseRowLabel (expect tokComma))
      case mbLabels of
        Nothing -> pure (Row { rowLabels: Nothing, rowTail: Nothing })
        Just lbls -> do
          mbPipe2 <- tryOnce (expect tokPipe)
          case mbPipe2 of
            Nothing -> pure (Row { rowLabels: Just lbls, rowTail: Nothing })
            Just pipe -> do
              ty <- parseType
              pure (Row { rowLabels: Just lbls, rowTail: Just (Tuple pipe ty) })

parseRowLabel :: Parser (Labeled Label (Type Unit))
parseRowLabel = do
  lbl <- parseLabel
  sep <- expect tokDoubleColon
  ty  <- parseType
  pure (Labeled { lblLabel: lbl, lblSep: sep, lblValue: ty })

parseTypeKindedAtom :: Parser (Type Unit)
parseTypeKindedAtom = do
  t <- munch
  case tokValue t of
    TokUnderscore  -> pure (TypeWildcard unit t)
    TokHole _      -> pushBack t *> (TypeHole unit <$> parseHole)
    TokUpperName _ _ -> pushBack t *> do
      qpn <- parseQualProperName
      pure (TypeConstructor unit (getQualifiedProperName qpn))
    TokSymbolName _ _ -> pushBack t *> do
      qon <- parseQualSymbol
      pure (TypeOpName unit (getQualifiedOpName qon))
    TokInt _ _ -> pushBack t *> do
      Tuple tok i <- parseInt
      pure (TypeInt unit Nothing tok i)
    TokLeftBrace -> do
      r <- parseRow
      c <- expect tokRightBrace
      pure (TypeRecord unit (Wrapped { wrpOpen: t, wrpValue: r, wrpClose: c }))
    TokLeftParen -> do
      ty <- parseType1
      mbKind <- tryOnce (expect tokDoubleColon)
      case mbKind of
        Just sep -> do
          kind <- parseType
          c <- expect tokRightParen
          pure (TypeParens unit (Wrapped { wrpOpen: t, wrpValue: TypeKinded unit ty sep kind, wrpClose: c }))
        Nothing -> do
          c <- expect tokRightParen
          pure (TypeParens unit (Wrapped { wrpOpen: t, wrpValue: ty, wrpClose: c }))
    _ -> parseFail t ErrToken

parseTypeVarBinding :: Parser (TypeVarBinding Unit)
parseTypeVarBinding = do
  t <- munch
  case tokValue t of
    TokOperator [] "@" -> do
      nm <- parseIdent
      pure (TypeVarName (Tuple (Just t) nm))
    TokLeftParen -> do
      mbAt <- tryOnce (expect tokAt)
      nm <- parseIdent
      sep <- expect tokDoubleColon
      kind <- parseType
      checkNoWildcards kind
      c <- expect tokRightParen
      pure (TypeVarKinded (Wrapped
        { wrpOpen: t
        , wrpValue: Labeled { lblLabel: Tuple mbAt nm, lblSep: sep, lblValue: kind }
        , wrpClose: c
        }))
    _ -> pushBack t *> do
      nm <- parseIdent
      pure (TypeVarName (Tuple Nothing nm))

parseTypeVarBindingPlain :: Parser (TypeVarBinding Unit)
parseTypeVarBindingPlain = do
  t <- munch
  case tokValue t of
    TokLeftParen -> do
      nm <- parseIdent
      sep <- expect tokDoubleColon
      kind <- parseType
      checkNoWildcards kind
      c <- expect tokRightParen
      pure (TypeVarKinded (Wrapped
        { wrpOpen: t
        , wrpValue: Labeled { lblLabel: Tuple Nothing nm, lblSep: sep, lblValue: kind }
        , wrpClose: c
        }))
    _ -> pushBack t *> do
      nm <- parseIdent
      pure (TypeVarName (Tuple Nothing nm))

-- ---------------------------------------------------------------------------
-- Binders

parseBinder :: Parser (Binder Unit)
parseBinder = defer \_ -> do
  b <- parseBinder1
  mbSep <- tryOnce (expect tokDoubleColon)
  case mbSep of
    Nothing  -> pure b
    Just sep -> BinderTyped unit b sep <$> parseType

parseBinder1 :: Parser (Binder Unit)
parseBinder1 = do
  b <- parseBinder2
  go b
  where
  go b = do
    mbOp <- tryOnce parseQualOp
    case mbOp of
      Just op -> do
        b2 <- parseBinder2
        go (BinderOp unit b (getQualifiedOpName op) b2)
      Nothing -> pure b

parseBinder2 :: Parser (Binder Unit)
parseBinder2 = do
  mbMinus <- tryOnce (expect tokMinus)
  case mbMinus of
    Just minus -> do
      Tuple tok num <- parseNumber
      pure (BinderNumber unit (Just minus) tok num)
    Nothing -> do
      atoms <- many1 parseBinderAtom
      toBinderConstructor atoms

parseBinderAtom :: Parser (Binder Unit)
parseBinderAtom = do
  t <- munch
  case tokValue t of
    TokUnderscore    -> pure (BinderWildcard unit t)
    TokHole _        -> parseFail t ErrToken
    TokLowerName [] _ -> do
      nm <- pushBack t *> parseIdent
      mbAt <- tryOnce (expect tokAt)
      case mbAt of
        Just at -> BinderNamed unit nm at <$> parseBinderAtom
        Nothing -> pure (BinderVar unit nm)
    TokLowerName [] "as" -> pushBack t *> do
      nm <- parseIdent
      pure (BinderVar unit nm)
    TokUpperName _ _ -> pushBack t *> do
      qpn <- parseQualProperName
      pure (BinderConstructor unit (getQualifiedProperName qpn) [])
    TokLowerName [] "true"  -> pushBack t *> do
      Tuple tok b <- parseBoolean
      pure (BinderBoolean unit tok b)
    TokLowerName [] "false" -> pushBack t *> do
      Tuple tok b <- parseBoolean
      pure (BinderBoolean unit tok b)
    TokChar _ _   -> pushBack t *> do
      Tuple tok c <- parseChar
      pure (BinderChar unit tok c)
    TokString _ _ -> pushBack t *> do
      Tuple tok str <- parseString
      pure (BinderString unit tok str)
    TokRawString _ -> pushBack t *> do
      Tuple tok str <- parseString
      pure (BinderString unit tok str)
    TokInt _ _ -> pushBack t *> do
      Tuple tok num <- parseNumber
      pure (BinderNumber unit Nothing tok num)
    TokNumber _ _ -> pushBack t *> do
      Tuple tok num <- parseNumber
      pure (BinderNumber unit Nothing tok num)
    TokLeftSquare -> do
      inner <- delim (const true) tokRightSquare tokComma parseBinder
      pure (BinderArray unit (Wrapped { wrpOpen: t, wrpValue: (case inner of Wrapped w -> w.wrpValue), wrpClose: (case inner of Wrapped w -> w.wrpClose) }))
    TokLeftBrace -> do
      inner <- delim (const true) tokRightBrace tokComma parseRecordBinder
      pure (BinderRecord unit (Wrapped { wrpOpen: t, wrpValue: (case inner of Wrapped w -> w.wrpValue), wrpClose: (case inner of Wrapped w -> w.wrpClose) }))
    TokLeftParen -> do
      b <- parseBinder
      c <- expect tokRightParen
      pure (BinderParens unit (Wrapped { wrpOpen: t, wrpValue: b, wrpClose: c }))
    _ -> parseFail t ErrToken

parseRecordBinder :: Parser (RecordLabeled (Binder Unit))
parseRecordBinder = do
  lbl <- parseLabel
  t <- munch
  case tokValue t of
    TokOperator [] ":" -> do
      b <- parseBinder
      pure (RecordField lbl t b)
    TokEquals -> do
      b <- parseBinder
      addFailure [t] ErrRecordUpdateInCtr
      pure (RecordPun (unexpectedName (lblTok lbl)))
    _ -> do
      pushBack t
      nm <- toName Ident (lblTok lbl)
      pure (RecordPun nm)

-- ---------------------------------------------------------------------------
-- Expressions

parseExprWhere :: Parser (Where Unit)
parseExprWhere = defer \_ -> do
  expr <- parseExpr
  mbWhere <- tryOnce (expect (tokKeyword "where"))
  case mbWhere of
    Nothing -> pure (Where { whereExpr: expr, whereBindings: Nothing })
    Just kwWhere -> do
      _ <- expect tokLayoutStart
      bindings <- parseSep parseLetBinding (expect tokLayoutSep)
      _ <- expect tokLayoutEnd
      case toNonEmpty (sepToArray bindings) of
        Nothing -> parseFail' [] ErrEmptyDo
        Just ne -> pure (Where { whereExpr: expr, whereBindings: Just (Tuple kwWhere ne) })

sepToArray :: forall a. Separated a -> Array a
sepToArray (Separated { sepHead: hd, sepTail: tl }) =
  Array.cons hd (map snd tl)

toNonEmpty :: forall a. Array a -> Maybe (NonEmptyList a)
toNonEmpty = NEL.fromFoldable

parseExpr :: Parser (Expr Unit)
parseExpr = defer \_ -> do
  e <- parseExpr1
  mbKind <- tryOnce (expect tokDoubleColon)
  case mbKind of
    Nothing  -> pure e
    Just sep -> ExprTyped unit e sep <$> parseType

parseExpr1 :: Parser (Expr Unit)
parseExpr1 = do
  e <- parseExpr2
  go e
  where
  go e = do
    mbOp <- tryOnce parseQualOp
    case mbOp of
      Just op -> do
        e2 <- parseExpr2
        go (ExprOp unit e (getQualifiedOpName op) e2)
      Nothing -> pure e

parseExpr2 :: Parser (Expr Unit)
parseExpr2 = do
  e <- parseExpr3
  go e
  where
  go e = do
    mbTick <- tryOnce (expect tokTick)
    case mbTick of
      Just tick -> do
        bt <- parseExprBacktick
        tick2 <- expect tokTick
        e2 <- parseExpr3
        go (ExprInfix unit e (Wrapped { wrpOpen: tick, wrpValue: bt, wrpClose: tick2 }) e2)
      Nothing -> pure e

parseExprBacktick :: Parser (Expr Unit)
parseExprBacktick = do
  e <- parseExpr3
  go e
  where
  go e = do
    mbOp <- tryOnce parseQualOp
    case mbOp of
      Just op -> do
        e2 <- parseExpr3
        go (ExprOp unit e (getQualifiedOpName op) e2)
      Nothing -> pure e

parseExpr3 :: Parser (Expr Unit)
parseExpr3 = do
  mbMinus <- tryOnce (expect tokMinus)
  case mbMinus of
    Just minus -> ExprNegate unit minus <$> parseExpr3
    Nothing    -> parseExpr4

parseExpr4 :: Parser (Expr Unit)
parseExpr4 = do
  e <- parseExpr5
  go e
  where
  go e = do
    t <- munch
    case tokValue t of
      TokOperator [] "@" -> do
        ty <- parseTypeAtom
        go (ExprVisibleTypeApp unit e t ty)
      _ -> do
        pushBack t
        mbArg <- tryOnce parseExpr5
        case mbArg of
          Just arg ->
            -- Record application/updates can introduce a function application
            -- associated to the right, so we need to correct it.
            case arg of
              ExprApp _ lhs rhs ->
                go (ExprApp unit (ExprApp unit e lhs) rhs)
              _ -> go (ExprApp unit e arg)
          Nothing -> pure e

parseExpr5 :: Parser (Expr Unit)
parseExpr5 = do
  t <- munch
  case tokValue t of
    TokLowerName [] "if" -> do
      cond <- parseExpr
      thenTok <- expect (tokKeyword "then")
      thenE <- parseExpr
      elseTok <- expect (tokKeyword "else")
      elseE <- parseExpr
      pure (ExprIf unit (IfThenElse { iteIf: t, iteCond: cond, iteThen: thenTok, iteTrue: thenE, iteElse: elseTok, iteFalse: elseE }))
    TokLowerName _ "do" -> do
      parseDoBlock' t
    TokLowerName _ "ado" -> do
      Tuple adoTok stmts <- parseAdoBlock' t
      inTok <- expect (tokKeyword "in")
      e <- parseExpr
      pure (ExprAdo unit (AdoBlock { adoKeyword: adoTok, adoStatements: stmts, adoIn: inTok, adoResult: e }))
    TokBackslash -> do
      binders <- many1 parseBinderAtom
      arr <- expect tokRightArrow
      e <- parseExpr
      pure (ExprLambda unit (Lambda { lmbSymbol: t, lmbBinders: binders, lmbArr: arr, lmbBody: e }))
    TokLowerName [] "let" -> do
      _ <- expect tokLayoutStart
      bindings <- parseSep parseLetBinding (expect tokLayoutSep)
      _ <- expect tokLayoutEnd
      inTok <- expect (tokKeyword "in")
      e <- parseExpr
      case toNonEmpty (sepToArray bindings) of
        Nothing -> parseFail' [] ErrEmptyDo
        Just ne -> pure (ExprLet unit (LetIn { letKeyword: t, letBindings: ne, letIn: inTok, letBody: e }))
    TokLowerName [] "case" -> do
      exprs <- parseSep parseExpr (expect tokComma)
      ofTok <- expect (tokKeyword "of")
      _ <- expect tokLayoutStart
      branches <- parseSep parseCaseBranch (expect tokLayoutSep)
      _ <- expect tokLayoutEnd
      case toNonEmpty (sepToArray branches) of
        Nothing -> parseFail' [] ErrEmptyDo
        Just ne -> pure (ExprCase unit (CaseOf { caseKeyword: t, caseHead: exprs, caseOf: ofTok, caseBranches: ne }))
    _ -> pushBack t *> parseExpr6

parseExpr6 :: Parser (Expr Unit)
parseExpr6 = defer \_ -> do
  e <- parseExpr7
  t <- munch
  case tokValue t of
    TokLeftBrace -> do
      t2 <- munch
      case tokValue t2 of
        TokRightBrace -> pure (ExprApp unit e (ExprRecord unit (Wrapped { wrpOpen: t, wrpValue: Nothing, wrpClose: t2 })))
        _ -> do
          pushBack t2
          fields <- parseSep parseRecordUpdateOrLabel (expect tokComma)
          c <- expect tokRightBrace
          result <- toRecordFields fields
          case result of
            Left xs ->
              pure (ExprApp unit e (ExprRecord unit (Wrapped { wrpOpen: t, wrpValue: Just xs, wrpClose: c })))
            Right xs ->
              pure (ExprRecordUpdate unit e (Wrapped { wrpOpen: t, wrpValue: xs, wrpClose: c }))
    _ -> pushBack t *> pure e

parseExpr7 :: Parser (Expr Unit)
parseExpr7 = defer \_ -> do
  e <- parseExprAtom
  mbDot <- tryOnce (expect tokDot)
  case mbDot of
    Nothing -> pure e
    Just dot -> do
      labels <- parseSep parseLabel (expect tokDot)
      pure (ExprRecordAccessor unit (RecordAccessor { recExpr: e, recDot: dot, recPath: labels }))

parseExprAtom :: Parser (Expr Unit)
parseExprAtom = do
  t <- munch
  case tokValue t of
    TokUnderscore    -> pure (ExprSection unit t)
    TokHole _        -> pushBack t *> (ExprHole unit <$> parseHole)
    TokLowerName _ _ -> pushBack t *> (ExprIdent unit <$> parseQualIdent)
    TokUpperName _ _ -> pushBack t *> do
      qpn <- parseQualProperName
      pure (ExprConstructor unit (getQualifiedProperName qpn))
    TokSymbolName _ _ -> pushBack t *> do
      qon <- parseQualSymbol
      pure (ExprOpName unit (getQualifiedOpName qon))
    TokLowerName [] "true"  -> pushBack t *> do
      Tuple tok b <- parseBoolean
      pure (ExprBoolean unit tok b)
    TokLowerName [] "false" -> pushBack t *> do
      Tuple tok b <- parseBoolean
      pure (ExprBoolean unit tok b)
    TokChar _ _   -> pushBack t *> do
      Tuple tok c <- parseChar
      pure (ExprChar unit tok c)
    TokString _ _ -> pushBack t *> do
      Tuple tok str <- parseString
      pure (ExprString unit tok str)
    TokRawString _ -> pushBack t *> do
      Tuple tok str <- parseString
      pure (ExprString unit tok str)
    TokInt _ _ -> pushBack t *> do
      Tuple tok num <- parseNumber
      pure (ExprNumber unit tok num)
    TokNumber _ _ -> pushBack t *> do
      Tuple tok num <- parseNumber
      pure (ExprNumber unit tok num)
    TokLeftSquare -> do
      inner <- delim (const true) tokRightSquare tokComma parseExpr
      pure (ExprArray unit (Wrapped { wrpOpen: t, wrpValue: (case inner of Wrapped w -> w.wrpValue), wrpClose: (case inner of Wrapped w -> w.wrpClose) }))
    TokLeftBrace -> do
      inner <- delim (const true) tokRightBrace tokComma parseRecordLabel
      pure (ExprRecord unit (Wrapped { wrpOpen: t, wrpValue: (case inner of Wrapped w -> w.wrpValue), wrpClose: (case inner of Wrapped w -> w.wrpClose) }))
    TokLeftParen -> do
      e <- parseExpr
      c <- expect tokRightParen
      pure (ExprParens unit (Wrapped { wrpOpen: t, wrpValue: e, wrpClose: c }))
    _ -> parseFail t ErrToken

parseRecordLabel :: Parser (RecordLabeled (Expr Unit))
parseRecordLabel = do
  lbl <- parseLabel
  t <- munch
  case tokValue t of
    TokOperator [] ":" -> RecordField lbl t <$> parseExpr
    TokEquals -> do
      e <- parseExpr
      addFailure [t] ErrRecordUpdateInCtr
      pure (RecordPun (unexpectedName (lblTok lbl)))
    _ -> do
      pushBack t
      nm <- toName Ident (lblTok lbl)
      pure (RecordPun nm)

parseRecordUpdateOrLabel :: Parser (Either (RecordLabeled (Expr Unit)) (RecordUpdate Unit))
parseRecordUpdateOrLabel = do
  lbl <- parseLabel
  t <- munch
  case tokValue t of
    TokOperator [] ":" -> do
      e <- parseExpr
      pure (Left (RecordField lbl t e))
    TokEquals -> do
      e <- parseExpr
      pure (Right (RecordUpdateLeaf lbl t e))
    TokLeftBrace -> do
      fields <- parseSep parseRecordUpdate (expect tokComma)
      c <- expect tokRightBrace
      pure (Right (RecordUpdateBranch lbl (Wrapped { wrpOpen: t, wrpValue: fields, wrpClose: c })))
    _ -> do
      pushBack t
      nm <- toName Ident (lblTok lbl)
      pure (Left (RecordPun nm))

parseRecordUpdate :: Parser (RecordUpdate Unit)
parseRecordUpdate = do
  lbl <- parseLabel
  t <- munch
  case tokValue t of
    TokEquals -> do
      e <- parseExpr
      pure (RecordUpdateLeaf lbl t e)
    TokLeftBrace -> do
      fields <- parseSep parseRecordUpdate (expect tokComma)
      c <- expect tokRightBrace
      pure (RecordUpdateBranch lbl (Wrapped { wrpOpen: t, wrpValue: fields, wrpClose: c }))
    _ -> parseFail t ErrToken

-- ---------------------------------------------------------------------------
-- Do/Ado

parseDoBlock' :: SourceToken -> Parser (Expr Unit)
parseDoBlock' doTok = do
  _ <- expect tokLayoutStart
  stmts <- parseDoStatements
  case toNonEmpty stmts of
    Nothing -> parseFail' [] ErrEmptyDo
    Just ne -> pure (ExprDo unit (DoBlock { doKeyword: doTok, doStatements: ne }))

parseAdoBlock' :: SourceToken -> Parser (Tuple SourceToken (Array (DoStatement Unit)))
parseAdoBlock' adoTok = do
  t <- expect tokLayoutStart
  t2 <- munch
  case tokValue t2 of
    TokLayoutEnd -> pure (Tuple adoTok [])
    _ -> do
      pushBack t2
      stmts <- parseDoStatements
      pure (Tuple adoTok stmts)

parseDoStatements :: Parser (Array (DoStatement Unit))
parseDoStatements = go []
  where
  go acc = do
    t <- munch
    case tokValue t of
      TokLayoutEnd -> pure (Array.reverse acc)
      TokLayoutSep -> go acc
      TokLowerName [] "let" -> do
        _ <- expect tokLayoutStart
        bindings <- parseSep parseLetBinding (expect tokLayoutSep)
        _ <- expect tokLayoutEnd
        case toNonEmpty (sepToArray bindings) of
          Nothing -> parseFail' [] ErrEmptyDo
          Just ne -> do
            stmt <- pure (DoLet t ne)
            go (Array.cons stmt acc)
      _ -> do
        pushBack t
        stmt <- do
          result <- tryPrefix parseBinderAndArrow parseExpr
          pure (case result of
            Tuple (Just (Tuple binder arr)) expr ->
              DoBind binder arr expr
            Tuple Nothing expr ->
              DoDiscard expr)
        t2 <- munch
        case tokValue t2 of
          TokLayoutSep -> go (Array.cons stmt acc)
          TokLayoutEnd -> pure (Array.reverse (Array.cons stmt acc))
          _ -> parseFail t2 ErrToken

parseBinderAndArrow :: Parser (Tuple (Binder Unit) SourceToken)
parseBinderAndArrow = do
  b <- parseBinder
  arr <- expect tokLeftArrow
  pure (Tuple b arr)

parseCaseBranch :: Parser (Tuple (Separated (Binder Unit)) (Guarded Unit))
parseCaseBranch = do
  binders <- parseSep parseBinder1 (expect tokComma)
  g <- parseGuardedCase
  pure (Tuple binders g)

parseGuardedCase :: Parser (Guarded Unit)
parseGuardedCase = do
  t <- munch
  case tokValue t of
    TokRightArrow _ -> Unconditional t <$> parseExprWhere
    _ -> pushBack t *> do
      guards <- many1 parseGuardedCaseExpr
      pure (Guarded guards)

parseGuardedCaseExpr :: Parser (GuardedExpr Unit)
parseGuardedCaseExpr = defer \_ -> do
  Tuple pipe guards <- parseGuard
  arr <- expect tokRightArrow
  w <- parseExprWhere
  pure (GuardedExpr { grdBar: pipe, grdPatterns: guards, grdSep: arr, grdWhere: w })

parseGuard :: Parser (Tuple SourceToken (Separated (PatternGuard Unit)))
parseGuard = do
  pipe <- expect tokPipe
  guards <- parseGuardStatements
  pure (Tuple pipe guards)

parseGuardStatements :: Parser (Separated (PatternGuard Unit))
parseGuardStatements = defer \_ -> parseSep parsePatternGuard (expect tokComma)

parsePatternGuard :: Parser (PatternGuard Unit)
parsePatternGuard = defer \_ -> do
  result <- tryPrefix parseBinderAndArrow parseExpr1
  pure (case result of
    Tuple (Just (Tuple b arr)) e -> PatternGuard { patBinder: Just (Tuple b arr), patExpr: e }
    Tuple Nothing e              -> PatternGuard { patBinder: Nothing, patExpr: e })

-- ---------------------------------------------------------------------------
-- Let bindings

parseLetBinding :: Parser (LetBinding Unit)
parseLetBinding = do
  t <- munch
  case tokValue t of
    TokLowerName [] _ -> do
      pushBack t
      nm <- parseIdent
      t2 <- munch
      case tokValue t2 of
        TokDoubleColon _ -> do
          ty <- parseType
          pure (LetBindingSignature unit (Labeled { lblLabel: nm, lblSep: t2, lblValue: ty }))
        _ -> do
          pushBack t2
          binders <- many0 parseBinderAtom
          g <- parseGuardedDecl
          pure (LetBindingName unit (ValueBindingFields { valName: nm, valBinders: binders, valGuarded: g }))
    _ -> do
      pushBack t
      b <- parseBinder1
      eq <- expect tokEquals
      w <- parseExprWhere
      pure (LetBindingPattern unit b eq w)

parseGuardedDecl :: Parser (Guarded Unit)
parseGuardedDecl = do
  t <- munch
  case tokValue t of
    TokEquals -> Unconditional t <$> parseExprWhere
    _ -> pushBack t *> (Guarded <$> many1 parseGuardedDeclExpr)

parseGuardedDeclExpr :: Parser (GuardedExpr Unit)
parseGuardedDeclExpr = defer \_ -> do
  Tuple pipe guards <- parseGuard
  eq <- expect tokEquals
  w <- parseExprWhere
  pure (GuardedExpr { grdBar: pipe, grdPatterns: guards, grdSep: eq, grdWhere: w })

-- ---------------------------------------------------------------------------
-- Declarations

parseDecl :: Parser (Declaration Unit)
parseDecl = do
  t <- munch
  case tokValue t of
    TokLowerName [] "data" -> do
      pushBack t
      parseDataDecl
    TokLowerName [] "type" -> do
      pushBack t
      parseTypeDecl
    TokLowerName [] "newtype" -> do
      pushBack t
      parseNewtypeDecl
    TokLowerName [] "class" -> do
      pushBack t
      parseClassDecl
    TokLowerName [] "instance" -> do
      pushBack t
      parseInstanceDecl
    TokLowerName [] "derive" -> do
      pushBack t
      parseDeriveDecl
    TokLowerName [] "foreign" -> do
      pushBack t
      parseForeignDecl
    TokLowerName [] "infix"  -> pushBack t *> parseFixityDecl
    TokLowerName [] "infixl" -> pushBack t *> parseFixityDecl
    TokLowerName [] "infixr" -> pushBack t *> parseFixityDecl
    _ -> do
      pushBack t
      parseIdentDecl

parseDataDecl :: Parser (Declaration Unit)
parseDataDecl = do
  kwData <- expect (tokKeyword "data")
  mbPropName <- tryOnce parseProperName
  case mbPropName of
    Nothing -> parseFail' [] ErrToken
    Just pn -> do
      -- Check if this is a kind signature
      t <- munch
      case tokValue t of
        TokDoubleColon _ -> do
          ty <- parseType
          checkNoWildcards ty
          pure (DeclKindSignature unit kwData (Labeled { lblLabel: getProperName pn, lblSep: t, lblValue: ty }))
        _ -> do
          pushBack t
          dh <- parseDataHead' kwData pn
          mbCtors <- tryOnce (expect tokEquals)
          case mbCtors of
            Nothing -> pure (DeclData unit dh Nothing)
            Just eq -> do
              ctors <- parseSep parseDataCtor (expect tokPipe)
              pure (DeclData unit dh (Just (Tuple eq ctors)))

parseTypeDecl :: Parser (Declaration Unit)
parseTypeDecl = do
  kwType <- expect (tokKeyword "type")
  t <- munch
  case tokValue t of
    TokLowerName [] "role" -> do
      pn <- parseProperName
      roles <- many1 parseRole
      pure (DeclRole unit kwType t (getProperName pn) roles)
    _ -> do
      pushBack t
      mbPropName <- tryOnce parseProperName
      case mbPropName of
        Nothing -> parseFail' [] ErrToken
        Just pn -> do
          t2 <- munch
          case tokValue t2 of
            TokDoubleColon _ -> do
              ty <- parseType
              checkNoWildcards ty
              pure (DeclKindSignature unit kwType (Labeled { lblLabel: getProperName pn, lblSep: t2, lblValue: ty }))
            _ -> do
              pushBack t2
              dh <- parseDataHead' kwType pn
              eq <- expect tokEquals
              ty <- parseType
              checkNoWildcards ty
              pure (DeclType unit dh eq ty)

parseNewtypeDecl :: Parser (Declaration Unit)
parseNewtypeDecl = do
  kwNewtype <- expect (tokKeyword "newtype")
  mbPropName <- tryOnce parseProperName
  case mbPropName of
    Nothing -> parseFail' [] ErrToken
    Just pn -> do
      t <- munch
      case tokValue t of
        TokDoubleColon _ -> do
          ty <- parseType
          checkNoWildcards ty
          pure (DeclKindSignature unit kwNewtype (Labeled { lblLabel: getProperName pn, lblSep: t, lblValue: ty }))
        _ -> do
          pushBack t
          dh <- parseDataHead' kwNewtype pn
          eq <- expect tokEquals
          ctorName <- parseProperName
          ctorArg <- parseTypeAtom
          checkNoWildcards ctorArg
          pure (DeclNewtype unit dh eq (getProperName ctorName) ctorArg)

parseDataHead' :: SourceToken -> ProperName -> Parser (DataHead Unit)
parseDataHead' kw pn = do
  vars <- many0 parseTypeVarBindingPlain
  pure (DataHead { dataHdKeyword: kw, dataHdName: getProperName pn, dataHdVars: vars })

parseDataCtor :: Parser (DataCtor Unit)
parseDataCtor = do
  pn <- parseProperName
  args <- many0 parseTypeAtom
  for_ args checkNoWildcards
  pure (DataCtor { dataCtorAnn: unit, dataCtorName: getProperName pn, dataCtorFields: args })

parseClassDecl :: Parser (Declaration Unit)
parseClassDecl = do
  kwClass <- expect (tokKeyword "class")
  -- Try class signature first, then class head
  let tryClassSig = do
        pn <- parseProperName
        t <- munch
        case tokValue t of
          TokDoubleColon _ -> do
            ty <- parseType
            checkNoWildcards ty
            pure (DeclKindSignature unit kwClass (Labeled { lblLabel: getProperName pn, lblSep: t, lblValue: ty }))
          _ -> parseFail t ErrToken
      tryClassHead = do
        mbSuper <- tryOnce parseClassSuper
        Tuple name (Tuple vars fundeps) <- parseClassNameAndFundeps
        let hd = ClassHead
              { clsKeyword: kwClass
              , clsSuper: mbSuper
              , clsName: getProperName name
              , clsVars: vars
              , clsFundeps: fundeps
              }
        checkFundeps hd
        mbWhere <- tryOnce (expect (tokKeyword "where"))
        case mbWhere of
          Nothing -> pure (DeclClass unit hd Nothing)
          Just kwWhere -> do
            _ <- expect tokLayoutStart
            members <- parseSep parseClassMember (expect tokLayoutSep)
            _ <- expect tokLayoutEnd
            case toNonEmpty (sepToArray members) of
              Nothing -> parseFail' [] ErrEmptyDo
              Just ne -> pure (DeclClass unit hd (Just (Tuple kwWhere ne)))
  oneOf (unsafePartial fromJust (NEL.fromFoldable [tryClassSig, tryClassHead]))

parseClassSuper :: Parser (Tuple (OneOrDelimited (Constraint Unit)) SourceToken)
parseClassSuper = do
  cs <- parseConstraints
  fat <- expect tokLeftFatArrow
  pure (Tuple cs fat)

parseClassNameAndFundeps :: Parser (Tuple ProperName (Tuple (Array (TypeVarBinding Unit)) (Maybe (Tuple SourceToken (Separated ClassFundep)))))
parseClassNameAndFundeps = do
  pn <- parseProperName
  vars <- many0 parseTypeVarBindingPlain
  fundeps <- parseFundeps
  pure (Tuple pn (Tuple vars fundeps))

parseFundeps :: Parser (Maybe (Tuple SourceToken (Separated ClassFundep)))
parseFundeps = do
  mbPipe <- tryOnce (expect tokPipe)
  case mbPipe of
    Nothing -> pure Nothing
    Just pipe -> do
      fundeps <- parseSep parseFundep (expect tokComma)
      pure (Just (Tuple pipe fundeps))

parseFundep :: Parser ClassFundep
parseFundep = do
  t <- munch
  case tokValue t of
    TokRightArrow _ -> do
      idents <- many1 parseIdent
      pure (FundepDetermined t idents)
    _ -> do
      pushBack t
      idents1 <- many1 parseIdent
      arr <- expect tokRightArrow
      idents2 <- many1 parseIdent
      pure (FundepDetermines idents1 arr idents2)

parseClassMember :: Parser (Labeled (Name Ident) (Type Unit))
parseClassMember = do
  nm <- parseIdent
  sep <- expect tokDoubleColon
  ty <- parseType
  checkNoWildcards ty
  pure (Labeled { lblLabel: nm, lblSep: sep, lblValue: ty })

parseInstanceDecl :: Parser (Declaration Unit)
parseInstanceDecl = do
  ih <- parseInstHead
  mbWhere <- tryOnce (expect (tokKeyword "where"))
  case mbWhere of
    Nothing -> pure (DeclInstanceChain unit (Separated { sepHead: Instance { instHead: ih, instBody: Nothing }, sepTail: [] }))
    Just kwWhere -> do
      _ <- expect tokLayoutStart
      bindings <- parseSep parseInstBinding (expect tokLayoutSep)
      _ <- expect tokLayoutEnd
      case toNonEmpty (sepToArray bindings) of
        Nothing -> parseFail' [] ErrEmptyDo
        Just ne -> pure (DeclInstanceChain unit (Separated { sepHead: Instance { instHead: ih, instBody: Just (Tuple kwWhere ne) }, sepTail: [] }))

parseInstHead :: Parser (InstanceHead Unit)
parseInstHead = do
  kwInst <- expect (tokKeyword "instance")
  -- Try with name
  mbName <- tryOnce do
    nm <- parseIdent
    sep <- expect tokDoubleColon
    pure (Tuple nm sep)
  mbConstraints <- tryOnce parseClassSuper
  qpn <- parseQualProperName
  args <- many0 parseTypeAtom
  pure (InstanceHead
    { instKeyword: kwInst
    , instNameSep: mbName
    , instConstraints: mbConstraints
    , instClass: getQualifiedProperName qpn
    , instTypes: args
    })

parseInstBinding :: Parser (InstanceBinding Unit)
parseInstBinding = do
  t <- munch
  case tokValue t of
    TokLowerName [] _ -> do
      pushBack t
      nm <- parseIdent
      t2 <- munch
      case tokValue t2 of
        TokDoubleColon _ -> do
          ty <- parseType
          pure (InstanceBindingSignature unit (Labeled { lblLabel: nm, lblSep: t2, lblValue: ty }))
        _ -> do
          pushBack t2
          binders <- many0 parseBinderAtom
          g <- parseGuardedDecl
          pure (InstanceBindingName unit (ValueBindingFields { valName: nm, valBinders: binders, valGuarded: g }))
    _ -> parseFail t ErrToken

parseDeriveDecl :: Parser (Declaration Unit)
parseDeriveDecl = do
  kwDerive <- expect (tokKeyword "derive")
  mbNewtype <- tryOnce (expect (tokKeyword "newtype"))
  ih <- parseInstHead
  pure (DeclDerive unit kwDerive mbNewtype ih)

parseForeignDecl :: Parser (Declaration Unit)
parseForeignDecl = do
  kwForeign <- expect (tokKeyword "foreign")
  kwImport <- expect (tokKeyword "import")
  t <- munch
  case tokValue t of
    TokLowerName [] "data" -> do
      pn <- parseProperName
      sep <- expect tokDoubleColon
      ty <- parseType
      pure (DeclForeign unit kwForeign kwImport (ForeignData t (Labeled { lblLabel: getProperName pn, lblSep: sep, lblValue: ty })))
    _ -> do
      pushBack t
      nm <- parseIdent
      sep <- expect tokDoubleColon
      ty <- parseType
      when (isConstrained ty) do
        let toks = [kwForeign, kwImport, nameTok nm, sep] <> (Array.toUnfoldable (flattenType ty) :: Array SourceToken)
        addFailure toks ErrConstraintInForeignImportSyntax
      pure (DeclForeign unit kwForeign kwImport (ForeignValue (Labeled { lblLabel: nm, lblSep: sep, lblValue: ty })))
  where
  nameTok (Name { nameTok: t }) = t

parseFixityDecl :: Parser (Declaration Unit)
parseFixityDecl = do
  Tuple kwInfix fixity <- parseInfix
  Tuple tok prec <- parseInt
  t <- munch
  case tokValue t of
    TokLowerName [] "type" -> do
      qpn <- parseQualProperName
      asTok <- expect (tokKeyword "as")
      op <- parseOp
      pure (DeclFixity unit (FixityFields
        { fxtKeyword: Tuple kwInfix fixity
        , fxtPrec: Tuple tok prec
        , fxtOp: FixityType t (getQualifiedProperName qpn) asTok (getOpName op)
        }))
    TokUpperName _ _ -> do
      pushBack t
      qpn <- parseQualProperName
      asTok <- expect (tokKeyword "as")
      op <- parseOp
      pure (DeclFixity unit (FixityFields
        { fxtKeyword: Tuple kwInfix fixity
        , fxtPrec: Tuple tok prec
        , fxtOp: FixityValue (map Right (getQualifiedProperName qpn)) asTok (getOpName op)
        }))
    _ -> do
      pushBack t
      qi <- parseQualIdent
      asTok <- expect (tokKeyword "as")
      op <- parseOp
      pure (DeclFixity unit (FixityFields
        { fxtKeyword: Tuple kwInfix fixity
        , fxtPrec: Tuple tok prec
        , fxtOp: FixityValue (map Left qi) asTok (getOpName op)
        }))

parseInfix :: Parser (Tuple SourceToken Fixity)
parseInfix = do
  t <- munch
  case tokValue t of
    TokLowerName [] "infix"  -> pure (Tuple t Infix)
    TokLowerName [] "infixl" -> pure (Tuple t Infixl)
    TokLowerName [] "infixr" -> pure (Tuple t Infixr)
    _ -> parseFail t ErrToken

parseRole :: Parser Role
parseRole = do
  t <- munch
  case tokValue t of
    TokLowerName [] "nominal"          -> pure (Role { roleTok: t, roleValue: R.Nominal })
    TokLowerName [] "representational" -> pure (Role { roleTok: t, roleValue: R.Representational })
    TokLowerName [] "phantom"          -> pure (Role { roleTok: t, roleValue: R.Phantom })
    _ -> parseFail t ErrToken

parseIdentDecl :: Parser (Declaration Unit)
parseIdentDecl = do
  nm <- parseIdent
  t <- munch
  case tokValue t of
    TokDoubleColon _ -> do
      ty <- parseType
      pure (DeclSignature unit (Labeled { lblLabel: nm, lblSep: t, lblValue: ty }))
    _ -> do
      pushBack t
      binders <- many0 parseBinderAtom
      g <- parseGuardedDecl
      pure (DeclValue unit (ValueBindingFields { valName: nm, valBinders: binders, valGuarded: g }))

-- ---------------------------------------------------------------------------
-- Constraints

parseConstraints :: Parser (OneOrDelimited (Constraint Unit))
parseConstraints = do
  t <- munch
  case tokValue t of
    TokLeftParen -> do
      cs <- parseSep parseConstraint (expect tokComma)
      c <- expect tokRightParen
      pure (Many (Wrapped { wrpOpen: t, wrpValue: cs, wrpClose: c }))
    _ -> pushBack t *> (One <$> parseConstraint)

parseConstraint :: Parser (Constraint Unit)
parseConstraint = do
  t <- munch
  case tokValue t of
    TokLeftParen -> do
      c <- parseConstraint
      close <- expect tokRightParen
      pure (ConstraintParens unit (Wrapped { wrpOpen: t, wrpValue: c, wrpClose: close }))
    TokUpperName _ _ -> do
      pushBack t
      qpn <- parseQualProperName
      args <- many0 parseTypeAtom
      for_ args checkNoWildcards
      for_ args checkNoForalls
      pure (Constraint unit (getQualifiedProperName qpn) args)
    _ -> parseFail t ErrToken

-- ---------------------------------------------------------------------------
-- Module

parseModule :: Array LexResult -> Either (NonEmptyList ParserError) (PartialResult (Module Unit))
parseModule toks = do
  header <- headerRes
  pure (PartialResult
    { resPartial: header
    , resFull: parseFull header
    })
  where
  Tuple st headerRes = runParser (ParserState { parserBuff: toks, parserErrors: [], parserWarnings: [] }) parseModuleHeader

  parseFull header =
    let Tuple (ParserState ps) res = runParser st parseModuleBody
        warnings = ps.parserWarnings
    in Tuple warnings (map (\(Tuple decls trailing) ->
         case header of
           Module md -> Module md { modDecls = decls, modTrailingComments = trailing }
         ) res)

parseModuleHeader :: Parser (Module Unit)
parseModuleHeader = do
  kwModule <- expect (tokKeyword "module")
  name <- parseModuleName
  exports <- parseExports
  kwWhere <- expect (tokKeyword "where")
  _ <- expect tokLayoutStart
  imports <- parseModuleImports
  pure (Module
    { modAnn: unit
    , modKeyword: kwModule
    , modNamespace: name
    , modExports: exports
    , modWhere: kwWhere
    , modImports: imports
    , modDecls: []
    , modTrailingComments: []
    })

parseModuleBody :: Parser (Tuple (Array (Declaration Unit)) (Array _))
parseModuleBody = do
  Tuple _ decls <- parseModuleDecls
  t <- expect tokLayoutEnd
  let trailing = case t of SourceToken { tokAnn: TokenAnn { tokLeadingComments: lc } } -> lc
  pure (Tuple decls trailing)

parseModuleImports :: Parser (Array (ImportDecl Unit))
parseModuleImports = go []
  where
  go acc = do
    t <- munch
    case tokValue t of
      TokLowerName [] "import" -> do
        pushBack t
        imp <- parseImportDecl
        t2 <- munch
        case tokValue t2 of
          TokLayoutSep -> go (Array.cons imp acc)
          TokLayoutEnd -> do
            pushBack t2
            pure (Array.reverse (Array.cons imp acc))
          _ -> parseFail t2 ErrToken
      TokLayoutEnd -> do
        pushBack t
        pure (Array.reverse acc)
      TokLayoutSep -> go acc
      _ -> do
        pushBack t
        pure (Array.reverse acc)

parseModuleDecls :: Parser (Tuple (Array (ImportDecl Unit)) (Array (Declaration Unit)))
parseModuleDecls = do
  t <- munch
  case tokValue t of
    TokLayoutEnd -> pushBack t *> pure (Tuple [] [])
    _ -> do
      pushBack t
      decls <- parseSep parseModuleDecl (expect tokLayoutSep)
      toModuleDecls (Array.toUnfoldable (sepToArray decls))

parseModuleDecl :: Parser (TmpModuleDecl Unit)
parseModuleDecl = do
  t <- munch
  case tokValue t of
    TokLowerName [] "import" -> do
      pushBack t
      TmpImport <$> parseImportDecl
    _ -> do
      pushBack t
      d <- parseDecl
      ds <- many0 do
        elseT <- expect (tokKeyword "else")
        mbSep <- tryOnce (expect tokLayoutSep)
        _ <- pure mbSep -- ignore the separator
        parseDecl
      pure (TmpChain (Separated { sepHead: d, sepTail: map (\d' -> Tuple placeholder d') ds }))

parseExports :: Parser (Maybe (Wrapped (Separated (Export Unit))))
parseExports = do
  t <- munch
  case tokValue t of
    TokLeftParen -> do
      exports <- parseSep parseExport (expect tokComma)
      c <- expect tokRightParen
      pure (Just (Wrapped { wrpOpen: t, wrpValue: exports, wrpClose: c }))
    _ -> pushBack t *> pure Nothing

parseExport :: Parser (Export Unit)
parseExport = do
  t <- munch
  case tokValue t of
    TokLowerName [] "type" -> do
      sym <- parseSymbol
      pure (ExportTypeOp unit t (getOpName sym))
    TokLowerName [] "class" -> do
      pn <- parseProperName
      pure (ExportClass unit t (getProperName pn))
    TokLowerName [] "module" -> do
      name <- parseModuleName
      pure (ExportModule unit t name)
    TokLowerName [] _ -> pushBack t *> (ExportValue unit <$> parseIdent)
    TokSymbolName [] _ -> pushBack t *> do
      sym <- parseSymbol
      pure (ExportOp unit (getOpName sym))
    TokUpperName [] _ -> do
      pushBack t
      pn <- parseProperName
      mbMembers <- tryOnce parseDataMembers
      pure (ExportType unit (getProperName pn) mbMembers)
    _ -> parseFail t ErrToken

parseImportDecl :: Parser (ImportDecl Unit)
parseImportDecl = do
  kwImport <- expect (tokKeyword "import")
  name <- parseModuleName
  imports <- parseImports
  mbAs <- tryOnce do
    asTok <- expect (tokKeyword "as")
    asName <- parseModuleName
    pure (Tuple asTok asName)
  pure (ImportDecl { impAnn: unit, impKeyword: kwImport, impModule: name, impNames: imports, impQual: mbAs })

parseImports :: Parser (Maybe (Tuple (Maybe SourceToken) (Wrapped (Separated (Import Unit)))))
parseImports = do
  t <- munch
  case tokValue t of
    TokLeftParen -> do
      pushBack t
      imports <- delim1 tokLeftParen tokRightParen tokComma parseImport
      pure (Just (Tuple Nothing imports))
    TokLowerName [] "hiding" -> do
      o <- expect tokLeftParen
      imports <- parseSep parseImport (expect tokComma)
      c <- expect tokRightParen
      pure (Just (Tuple (Just t) (Wrapped { wrpOpen: o, wrpValue: imports, wrpClose: c })))
    _ -> pushBack t *> pure Nothing

parseImport :: Parser (Import Unit)
parseImport = do
  t <- munch
  case tokValue t of
    TokLowerName [] "type" -> do
      sym <- parseSymbol
      pure (ImportTypeOp unit t (getOpName sym))
    TokLowerName [] "class" -> do
      pn <- parseProperName
      pure (ImportClass unit t (getProperName pn))
    TokLowerName [] _ -> pushBack t *> (ImportValue unit <$> parseIdent)
    TokSymbolName [] _ -> pushBack t *> do
      sym <- parseSymbol
      pure (ImportOp unit (getOpName sym))
    TokUpperName [] _ -> do
      pushBack t
      pn <- parseProperName
      mbMembers <- tryOnce parseDataMembers
      pure (ImportType unit (getProperName pn) mbMembers)
    _ -> parseFail t ErrToken

parseDataMembers :: Parser (DataMembers Unit)
parseDataMembers = do
  t <- munch
  case tokValue t of
    TokSymbolName [] ".." ->
      -- (..) is lexed as a single TokSymbolName token
      pure (DataAll unit t)
    TokLeftParen -> do
      t2 <- munch
      case tokValue t2 of
        TokRightParen -> pure (DataEnumerated unit (Wrapped { wrpOpen: t, wrpValue: Nothing, wrpClose: t2 }))
        _ -> do
          pushBack t2
          names <- parseSep (map getProperName parseProperName) (expect tokComma)
          c <- expect tokRightParen
          pure (DataEnumerated unit (Wrapped { wrpOpen: t, wrpValue: Just names, wrpClose: c }))
    _ -> parseFail t ErrToken

-- ---------------------------------------------------------------------------
-- Public API

parse :: String -> Tuple (Array ParserWarning) (Either (NonEmptyList ParserError) (Module Unit))
parse src = case parseModule (lexModule src) of
  Left errs -> Tuple [] (Left errs)
  Right (PartialResult { resFull }) -> resFull

-- Partial result parsers for incremental use
parseImportDeclP :: Array LexResult -> Either (NonEmptyList ParserError) (Tuple (Array ParserWarning) (ImportDecl Unit))
parseImportDeclP = runTokenParser (parseImportDecl <* (pushBack =<< munch))

parseDeclP :: Array LexResult -> Either (NonEmptyList ParserError) (Tuple (Array ParserWarning) (Declaration Unit))
parseDeclP = runTokenParser (parseDecl <* (pushBack =<< munch))

parseExprP :: Array LexResult -> Either (NonEmptyList ParserError) (Tuple (Array ParserWarning) (Expr Unit))
parseExprP = runTokenParser (parseExpr <* (pushBack =<< munch))

parseTypeP :: Array LexResult -> Either (NonEmptyList ParserError) (Tuple (Array ParserWarning) (Type Unit))
parseTypeP = runTokenParser (parseType <* (pushBack =<< munch))

parseModuleNameP :: Array LexResult -> Either (NonEmptyList ParserError) (Tuple (Array ParserWarning) (Name N.ModuleName))
parseModuleNameP = runTokenParser (parseModuleName <* (pushBack =<< munch))

parseQualIdentP :: Array LexResult -> Either (NonEmptyList ParserError) (Tuple (Array ParserWarning) (QualifiedName Ident))
parseQualIdentP = runTokenParser (parseQualIdent <* (pushBack =<< munch))

parseOperator :: Parser OpName
parseOperator = parseOp
