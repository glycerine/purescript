module Language.PureScript.CST.Lexer
  ( lenient
  , lexModule
  , lex
  , lexTopLevel
  , lexWithState
  , isUnquotedKey
  ) where

import Prelude

import Control.Monad (join)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (cons, foldl, length, reverse, snoc, uncons) as Array
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.CodePoint.Unicode (isAlphaNum, isSymbol, isUpper, isLower, isAscii) as UCP
import Data.Enum (toEnum)
import Data.Int (round, toNumber) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Number (isFinite, pow) as Number
import Data.String (Pattern(..), contains, stripPrefix) as DS
import Data.String.CodePoints (codePointFromChar) as SCP
import Data.String.CodeUnits (countPrefix, drop, fromCharArray, length, singleton, take, toCharArray, uncons) as SCU
import Data.Tuple (Tuple(..))
import Data.Void (Void, absurd)
import Language.PureScript.CST.Errors (ParserErrorInfo(..), ParserErrorType(..))
import Language.PureScript.CST.Layout (LayoutDelim(..), insertLayout, lytToken, unwindLayout)
import Language.PureScript.CST.Monad
  ( LexResult
  , LexState(..)
  , ParserM(..)
  , throw
  )
import Language.PureScript.CST.Positions (advanceLeading, advanceToken, advanceTrailing, applyDelta, textDelta)
import Language.PureScript.CST.Types
  ( Comment(..)
  , CSTSourcePos(..)
  , LineFeed(..)
  , SourceRange(..)
  , SourceStyle(..)
  , SourceToken(..)
  , Token(..)
  , TokenAnn(..)
  )
import Language.PureScript.PSString (PSString, mkString)

-- | Stops at the first lexing error, replacing it with TokEof.
lenient :: Array LexResult -> Array LexResult
lenient = go
  where
  go arr = case Array.uncons arr of
    Nothing -> []
    Just { head: Right a, tail: as_ } -> Array.cons (Right a) (go as_)
    Just { head: Left (Tuple st _) } ->
      let pos = case st of LexState ls -> ls.lexPos
          ann = TokenAnn
            { tokRange: SourceRange { srcStart: pos, srcEnd: pos }
            , tokLeadingComments: case st of LexState ls -> ls.lexLeading
            , tokTrailingComments: []
            }
      in [Right (SourceToken { tokAnn: ann, tokValue: TokEof })]

lexModule :: String -> Array LexResult
lexModule = lex' shebangThenComments

lex :: String -> Array LexResult
lex = lex' comments

lex' :: (String -> Tuple (Array (Comment LineFeed)) String) -> String -> Array LexResult
lex' lexComments src =
  let Tuple leading src' = lexComments src
  in lexWithState (LexState
    { lexPos: advanceLeading (CSTSourcePos { srcLine: 1, srcColumn: 1 }) leading
    , lexLeading: leading
    , lexSource: src'
    , lexStack: [Tuple (CSTSourcePos { srcLine: 0, srcColumn: 0 }) LytRoot]
    })

lexTopLevel :: String -> Array LexResult
lexTopLevel src =
  let Tuple leading src' = comments src
      lexPos_ = advanceLeading (CSTSourcePos { srcLine: 1, srcColumn: 1 }) leading
      hd = Right (lytToken lexPos_ TokLayoutStart)
      tl = lexWithState (LexState
        { lexPos: lexPos_
        , lexLeading: leading
        , lexSource: src'
        , lexStack:
            [ Tuple lexPos_ LytWhere
            , Tuple (CSTSourcePos { srcLine: 0, srcColumn: 0 }) LytRoot
            ]
        })
  in Array.cons hd tl

-- | Iterative implementation using tailRec to avoid JS stack overflow on large files.
-- | Each step lexes one token, accumulates layout tokens, and loops — no recursion.
lexWithState :: LexState -> Array LexResult
lexWithState initState = tailRec step (Tuple initState [])
  where
  Parser lexK = tokenAndComments

  step :: Tuple LexState (Array LexResult) -> Step (Tuple LexState (Array LexResult)) (Array LexResult)
  step (Tuple state acc) =
    let LexState ls = state
        lexSource_ = ls.lexSource
        lexPos_    = ls.lexPos
        lexLeading_ = ls.lexLeading
        lexStack_   = ls.lexStack
    in lexK lexSource_
         (\lexSource' err ->
           let len1       = SCU.length lexSource_
               len2       = SCU.length lexSource'
               chunk      = SCU.take (max 0 (len1 - len2)) lexSource_
               chunkDelta = textDelta chunk
               pos        = applyDelta lexPos_ chunkDelta
               errState   = LexState ls { lexSource = lexSource' }
           in Done (acc <> [Left (Tuple errState (mkParserErrorInfo pos err))]))
         (\lexSource' tokAndComments ->
           case tokAndComments of
             Tuple TokEof _ ->
               Done (acc <> map Right (unwindLayout lexPos_ lexLeading_ lexStack_))
             Tuple tok (Tuple trailing lexLeading') ->
               let endPos   = advanceToken lexPos_ tok
                   lexPos'  = advanceLeading (advanceTrailing endPos trailing) lexLeading'
                   tokenAnn = TokenAnn
                     { tokRange:            SourceRange { srcStart: lexPos_, srcEnd: endPos }
                     , tokLeadingComments:  lexLeading_
                     , tokTrailingComments: trailing
                     }
                   Tuple lexStack' toks = insertLayout
                     (SourceToken { tokAnn: tokenAnn, tokValue: tok }) lexPos' lexStack_
                   state' = LexState ls
                     { lexPos    = lexPos'
                     , lexLeading = lexLeading'
                     , lexSource  = lexSource'
                     , lexStack   = lexStack'
                     }
               in Loop (Tuple state' (acc <> map Right toks)))

  mkParserErrorInfo pos err =
    ParserErrorInfo
      { errRange: SourceRange { srcStart: pos, srcEnd: applyDelta pos (Tuple 0 1) }
      , errToks: []
      , errStack: []
      , errType: err
      }

type Lexer = ParserM ParserErrorType String

next :: Lexer Unit
next = Parser \inp _ ksucc -> ksucc (SCU.drop 1 inp) unit

nextWhile :: (Char -> Boolean) -> Lexer String
nextWhile p = Parser \inp _ ksucc ->
  let n = SCU.countPrefix p inp
  in ksucc (SCU.drop n inp) (SCU.take n inp)

nextWhile' :: Int -> (Char -> Boolean) -> Lexer String
nextWhile' n p = Parser \inp _ ksucc ->
  let cnt = SCU.countPrefix p (SCU.take n inp)
  in ksucc (SCU.drop cnt inp) (SCU.take cnt inp)

peek :: Lexer (Maybe Char)
peek = Parser \inp _ ksucc ->
  ksucc inp (map _.head (SCU.uncons inp))

restore :: forall a. (ParserErrorType -> Boolean) -> Lexer a -> Lexer a
restore p (Parser k) = Parser \inp kerr ksucc ->
  k inp (\inp' err -> kerr (if p err then inp else inp') err) ksucc

tokenAndComments :: Lexer (Tuple Token (Tuple (Array (Comment Void)) (Array (Comment LineFeed))))
tokenAndComments = Tuple <$> token <*> breakComments

shebangThenComments :: String -> Tuple (Array (Comment LineFeed)) String
shebangThenComments src =
  let Tuple sbComs src1 = shebang src
      Tuple moreComs src2 = comments src1
  in Tuple (sbComs <> moreComs) src2

shebang :: String -> Tuple (Array (Comment LineFeed)) String
shebang src =
  let Parser k = breakShebang
  in k src (\_ _ -> Tuple [] src) (\inp a -> Tuple a inp)

comments :: String -> Tuple (Array (Comment LineFeed)) String
comments src =
  let Parser k = breakComments :: Lexer (Tuple (Array (Comment Void)) (Array (Comment LineFeed)))
  in k src (\_ _ -> Tuple [] src) (\inp (Tuple a b) -> Tuple (voidComments a <> b) inp)
  where
  voidComments :: Array (Comment Void) -> Array (Comment LineFeed)
  voidComments = map voidComment
  voidComment :: Comment Void -> Comment LineFeed
  voidComment (Comment s) = Comment s
  voidComment (Space n)   = Space n
  voidComment (Line v)    = absurd v

breakComments :: Lexer (Tuple (Array (Comment Void)) (Array (Comment LineFeed)))
breakComments = k0 []
  where
  k0 acc = do
    spaces <- nextWhile (\c -> c == ' ')
    lines_ <- nextWhile isLineFeed
    let acc' = if spaces == "" then acc else Array.cons (Space (SCU.length spaces)) acc
    if lines_ == ""
      then do
        mbComm <- comment
        case mbComm of
          Just comm -> k0 (Array.cons comm acc')
          Nothing   -> pure (Tuple (reverseArr acc') [])
      else
        k1 acc' (goWs [] (SCU.toCharArray lines_))

  k1 trl acc = do
    ws <- nextWhile (\c -> c == ' ' || isLineFeed c)
    let acc' = goWs acc (SCU.toCharArray ws)
    mbComm <- comment
    case mbComm of
      Just comm -> k1 trl (Array.cons comm acc')
      Nothing   -> pure (Tuple (reverseArr trl) (reverseArr acc'))

  goWs :: Array (Comment LineFeed) -> Array Char -> Array (Comment LineFeed)
  goWs a cs = case Array.uncons cs of
    Nothing -> a
    Just { head: '\r', tail: rest } -> case Array.uncons rest of
      Just { head: '\n', tail: rest2 } -> goWs (Array.cons (Line CRLF) a) rest2
      _ -> goWs (Array.cons (Line CRLF) a) rest
    Just { head: '\n', tail: rest } -> goWs (Array.cons (Line LF) a) rest
    Just { head: ' ', tail: rest }  -> goSpace a 1 rest
    Just { tail: rest } -> goWs a rest

  goSpace :: Array (Comment LineFeed) -> Int -> Array Char -> Array (Comment LineFeed)
  goSpace a n cs = case Array.uncons cs of
    Just { head: ' ', tail: rest } -> goSpace a (n + 1) rest
    _ -> goWs (Array.cons (Space n) a) cs

  isBlockCommentStart :: Lexer (Maybe Boolean)
  isBlockCommentStart = Parser \inp _ ksucc ->
    case SCU.uncons inp of
      Just { head: '-', tail: inp2 } ->
        case SCU.uncons inp2 of
          Just { head: '-', tail: inp3 } -> ksucc inp3 (Just false)
          _ -> ksucc inp Nothing
      Just { head: '{', tail: inp2 } ->
        case SCU.uncons inp2 of
          Just { head: '-', tail: inp3 } -> ksucc inp3 (Just true)
          _ -> ksucc inp Nothing
      _ -> ksucc inp Nothing

  comment :: forall lf. Lexer (Maybe (Comment lf))
  comment = isBlockCommentStart >>= \mbIsBlock ->
    case mbIsBlock of
      Just true  -> Just <$> blockComment "{-"
      Just false -> Just <$> lineComment "--"
      Nothing    -> pure Nothing

  blockComment :: forall lf. String -> Lexer (Comment lf)
  blockComment acc = do
    chs   <- nextWhile (\c -> c /= '-')
    dashes <- nextWhile (\c -> c == '-')
    if dashes == ""
      then pure (Comment (acc <> chs))
      else peek >>= \mbCh ->
        case mbCh of
          Just '}' -> next *> pure (Comment (acc <> chs <> dashes <> "}"))
          _        -> blockComment (acc <> chs <> dashes)

  reverseArr :: forall a. Array a -> Array a
  reverseArr = Array.reverse

breakShebang :: Lexer (Array (Comment LineFeed))
breakShebang = shebangComment >>= \mbComm ->
  case mbComm of
    Just comm -> k0 [comm]
    Nothing   -> pure []
  where
  k0 acc = lineFeedShebang >>= \mbLfSb ->
    case mbLfSb of
      Just (Tuple lf sb) -> do
        comm <- lineComment sb
        k0 (Array.cons comm (Array.cons lf acc))
      Nothing ->
        pure (Array.reverse acc)

  lineFeedShebang :: Lexer (Maybe (Tuple (Comment LineFeed) String))
  lineFeedShebang = Parser \inp _ ksucc ->
    case unconsLineFeed inp of
      Just (Tuple lf inp2) ->
        case unconsShebang inp2 of
          Just (Tuple sb inp3) -> ksucc inp3 (Just (Tuple lf sb))
          Nothing              -> ksucc inp Nothing
      Nothing -> ksucc inp Nothing

  unconsLineFeed :: String -> Maybe (Tuple (Comment LineFeed) String)
  unconsLineFeed inp =
    case SCU.uncons inp of
      Just { head: '\r', tail: inp2 } ->
        case SCU.uncons inp2 of
          Just { head: '\n', tail: inp3 } -> Just (Tuple (Line CRLF) inp3)
          _                               -> Just (Tuple (Line CRLF) inp2)
      Just { head: '\n', tail: inp2 } -> Just (Tuple (Line LF) inp2)
      _ -> Nothing

  unconsShebang :: String -> Maybe (Tuple String String)
  unconsShebang s = map (Tuple "#!") (DS.stripPrefix (DS.Pattern "#!") s)

  shebangComment :: Lexer (Maybe (Comment LineFeed))
  shebangComment = isShebang >>= maybe (pure Nothing) (\sb -> Just <$> lineComment sb)

  isShebang :: Lexer (Maybe String)
  isShebang = Parser \inp _ ksucc ->
    case unconsShebang inp of
      Just (Tuple sb inp3) -> ksucc inp3 (Just sb)
      Nothing              -> ksucc inp Nothing

lineComment :: forall lf. String -> Lexer (Comment lf)
lineComment acc = do
  comm <- nextWhile (\c -> c /= '\r' && c /= '\n')
  pure (Comment (acc <> comm))

token :: Lexer Token
token = peek >>= maybe (pure TokEof) k0
  where
  k0 ch1 = case ch1 of
    '('  -> next *> leftParen
    ')'  -> next *> pure TokRightParen
    '{'  -> next *> pure TokLeftBrace
    '}'  -> next *> pure TokRightBrace
    '['  -> next *> pure TokLeftSquare
    ']'  -> next *> pure TokRightSquare
    '`'  -> next *> pure TokTick
    ','  -> next *> pure TokComma
    '∷'  -> next *> orOperator1 (TokDoubleColon Unicode) ch1
    '←'  -> next *> orOperator1 (TokLeftArrow Unicode) ch1
    '→'  -> next *> orOperator1 (TokRightArrow Unicode) ch1
    '⇒'  -> next *> orOperator1 (TokRightFatArrow Unicode) ch1
    '∀'  -> next *> orOperator1 (TokForall Unicode) ch1
    '|'  -> next *> orOperator1 TokPipe ch1
    '.'  -> next *> orOperator1 TokDot ch1
    '\\' -> next *> orOperator1 TokBackslash ch1
    '<'  -> next *> orOperator2 (TokLeftArrow ASCII) ch1 '-'
    '-'  -> next *> orOperator2 (TokRightArrow ASCII) ch1 '>'
    '='  -> next *> orOperator2' TokEquals (TokRightFatArrow ASCII) ch1 '>'
    ':'  -> next *> orOperator2' (TokOperator [] ":") (TokDoubleColon ASCII) ch1 ':'
    '?'  -> next *> hole
    '\'' -> next *> char_
    '"'  -> next *> string_
    _ | isDigit ch1     -> restore (\e -> e == ErrNumberOutOfRange) (next *> number ch1)
      | isUpper' ch1    -> next *> upper [] ch1
      | isIdentStart ch1 -> next *> lower [] ch1
      | isSymbolChar ch1 -> next *> operator [] [ch1]
      | otherwise       -> throw (ErrLexeme (Just (SCU.singleton ch1)) [])

  orOperator1 :: Token -> Char -> Lexer Token
  orOperator1 tok_ ch1 = join (Parser \inp _ ksucc ->
    case SCU.uncons inp of
      Just { head: ch2, tail: inp2 } | isSymbolChar ch2 ->
        ksucc inp2 (operator [] [ch1, ch2])
      _ ->
        ksucc inp (pure tok_))

  orOperator2 :: Token -> Char -> Char -> Lexer Token
  orOperator2 tok_ ch1 ch2 = join (Parser \inp _ ksucc ->
    case SCU.uncons inp of
      Just { head: ch2', tail: inp2 } | ch2 == ch2' ->
        case SCU.uncons inp2 of
          Just { head: ch3, tail: inp3 } | isSymbolChar ch3 ->
            ksucc inp3 (operator [] [ch1, ch2, ch3])
          _ ->
            ksucc inp2 (pure tok_)
      _ ->
        ksucc inp (operator [] [ch1]))

  orOperator2' :: Token -> Token -> Char -> Char -> Lexer Token
  orOperator2' tok1 tok2 ch1 ch2 = join (Parser \inp _ ksucc ->
    case SCU.uncons inp of
      Just { head: ch2', tail: inp2 } | ch2 == ch2' ->
        case SCU.uncons inp2 of
          Just { head: ch3, tail: inp3 } | isSymbolChar ch3 ->
            ksucc inp3 (operator [] [ch1, ch2, ch3])
          _ ->
            ksucc inp2 (pure tok2)
      Just { head: ch2', tail: inp2 } | isSymbolChar ch2' ->
        ksucc inp2 (operator [] [ch1, ch2'])
      _ ->
        ksucc inp (pure tok1))

  leftParen :: Lexer Token
  leftParen = Parser \inp kerr ksucc ->
    let n   = SCU.countPrefix isSymbolChar inp
        chs = SCU.take n inp
        inp2 = SCU.drop n inp
    in if n == 0
      then ksucc inp TokLeftParen
      else case SCU.uncons inp2 of
        Just { head: ')', tail: inp3 } ->
          if chs == "→" then ksucc inp3 (TokSymbolArr Unicode)
          else if chs == "->" then ksucc inp3 (TokSymbolArr ASCII)
          else if isReservedSymbol chs then kerr inp ErrReservedSymbol
          else ksucc inp3 (TokSymbolName [] chs)
        _ -> ksucc inp TokLeftParen

  symbol :: Array String -> Lexer Token
  symbol qual = restore isReservedSymbolError do
    peek >>= \mbCh ->
      case mbCh of
        Just ch | isSymbolChar ch -> do
          chs <- nextWhile isSymbolChar
          mbClose <- peek
          case mbClose of
            Just ')' ->
              if isReservedSymbol chs
                then throw ErrReservedSymbol
                else next *> pure (TokSymbolName (Array.reverse qual) chs)
            Just ch2 -> throw (ErrLexeme (Just (SCU.singleton ch2)) [])
            Nothing  -> throw ErrEof
        Just ch -> throw (ErrLexeme (Just (SCU.singleton ch)) [])
        Nothing -> throw ErrEof

  operator :: Array String -> Array Char -> Lexer Token
  operator qual pre = do
    rest <- nextWhile isSymbolChar
    pure (TokOperator (Array.reverse qual) (SCU.fromCharArray pre <> rest))

  upper :: Array String -> Char -> Lexer Token
  upper qual pre = do
    rest <- nextWhile isIdentChar
    mbCh1 <- peek
    let name = SCU.singleton pre <> rest
    case mbCh1 of
      Just '.' -> do
        let qual' = Array.cons name qual
        _ <- next
        mbCh2 <- peek
        case mbCh2 of
          Just '('               -> next *> symbol qual'
          Just ch2 | isUpper' ch2    -> next *> upper qual' ch2
          Just ch2 | isIdentStart ch2 -> next *> lower qual' ch2
          Just ch2 | isSymbolChar ch2 -> next *> operator qual' [ch2]
          Just ch2               -> throw (ErrLexeme (Just (SCU.singleton ch2)) [])
          Nothing                -> throw ErrEof
      _ ->
        pure (TokUpperName (Array.reverse qual) name)

  lower :: Array String -> Char -> Lexer Token
  lower qual pre = do
    rest <- nextWhile isIdentChar
    case pre of
      '_' | rest == "" ->
        if Array.uncons qual == Nothing
          then pure TokUnderscore
          else throw (ErrLexeme (Just (SCU.singleton pre)) [])
      _ ->
        case SCU.singleton pre <> rest of
          "forall" | Array.uncons qual == Nothing -> pure (TokForall ASCII)
          name -> pure (TokLowerName (Array.reverse qual) name)

  hole :: Lexer Token
  hole = do
    name <- nextWhile isIdentChar
    if name == ""
      then operator [] ['?']
      else pure (TokHole name)

  char_ :: Lexer Token
  char_ = do
    Tuple raw ch <- peek >>= \mbCh ->
      case mbCh of
        Just '\\' -> do
          Tuple raw2 ch2 <- next *> escape
          pure (Tuple (SCU.singleton '\\' <> raw2) ch2)
        Just ch0  -> next *> pure (Tuple (SCU.singleton ch0) ch0)
        Nothing   -> throw ErrEof
    mbClose <- peek
    case mbClose of
      Just '\'' ->
        if toCharCode ch > 0xFFFF
          then throw ErrAstralCodePointInChar
          else next *> pure (TokChar raw ch)
      Just ch2 -> throw (ErrLexeme (Just (SCU.singleton ch2)) [])
      _        -> throw ErrEof

  string_ :: Lexer Token
  string_ = do
    quotes1 <- nextWhile' 7 (\c -> c == '"')
    case SCU.length quotes1 of
      0 -> do
        let go :: String -> String -> Lexer Token
            go raw acc = do
              chs <- nextWhile isNormalStringChar
              let raw' = raw <> chs
                  acc' = acc <> chs
              mbCh <- peek
              case mbCh of
                Just '"'  -> next *> pure (TokString raw' (mkString acc'))
                Just '\\' -> next *> goEscape (raw' <> "\\") acc'
                Just _    -> throw ErrLineFeedInString
                Nothing   -> throw ErrEof
            goEscape :: String -> String -> Lexer Token
            goEscape raw acc = do
              mbCh1 <- peek
              case mbCh1 of
                Just ch1 | isStringGapChar ch1 -> do
                  gap <- nextWhile isStringGapChar
                  mbCh2 <- peek
                  case mbCh2 of
                    Just '"'  -> next *> pure (TokString (raw <> gap) (mkString acc))
                    Just '\\' -> next *> go (raw <> gap <> "\\") acc
                    Just ch2  -> throw (ErrCharInGap ch2)
                    Nothing   -> throw ErrEof
                _ -> do
                  Tuple raw2 ch2 <- escape
                  go (raw <> raw2) (acc <> SCU.singleton ch2)
        go "" ""
      1 ->
        pure (TokString "" (mkString ""))
      n | n >= 5 ->
        pure (TokRawString (SCU.drop 5 quotes1))
      _ -> do
        let go :: String -> Lexer Token
            go acc = do
              chs    <- nextWhile (\c -> c /= '"')
              quotes2 <- nextWhile' 5 (\c -> c == '"')
              case SCU.length quotes2 of
                0       -> throw ErrEof
                qn | qn >= 3 -> pure (TokRawString (acc <> chs <> SCU.drop 3 quotes2))
                _       -> go (acc <> chs <> quotes2)
        go (SCU.drop 2 quotes1)

  escape :: Lexer (Tuple String Char)
  escape = peek >>= \mbCh ->
    case mbCh of
      Just 't'  -> next *> pure (Tuple "t" '\t')
      Just 'r'  -> next *> pure (Tuple "r" '\r')
      Just 'n'  -> next *> pure (Tuple "n" '\n')
      Just '"'  -> next *> pure (Tuple "\"" '"')
      Just '\'' -> next *> pure (Tuple "'" '\'')
      Just '\\' -> next *> pure (Tuple "\\" '\\')
      Just 'x'  -> next *> (Parser \inp kerr ksucc ->
        let chars   = SCU.toCharArray (SCU.take 6 inp)
            Tuple n acc = goHex 0 [] chars
        in if n <= 0x10FFFF
           then case toEnum n of
             Just ch -> ksucc (SCU.drop (Array.length acc) inp)
                          (Tuple ("x" <> SCU.fromCharArray (Array.reverse acc)) ch)
             Nothing -> kerr inp ErrCharEscape
           else kerr inp ErrCharEscape)
      _ -> throw ErrCharEscape

  goHex :: Int -> Array Char -> Array Char -> Tuple Int (Array Char)
  goHex n acc cs = case Array.uncons cs of
    Nothing -> Tuple n acc
    Just { head: c, tail: cs' }
      | isHexDigit c -> goHex (n * 16 + digitToInt c) (Array.cons c acc) cs'
      | otherwise    -> Tuple n acc

  number :: Char -> Lexer Token
  number ch1 = do
    mbCh2 <- peek
    case Tuple ch1 mbCh2 of
      Tuple '0' (Just 'x') -> next *> hexadecimal
      _ -> do
        mbInt <- integer1 ch1
        mbFrac <- fraction
        case Tuple mbInt mbFrac of
          Tuple (Just (Tuple rawInt intStr)) Nothing -> do
            let int_ = digitsToNumber intStr
            mbExp <- exponent
            case mbExp of
              Just (Tuple rawExp exp_) ->
                sciDouble (rawInt <> rawExp) (int_ * Number.pow 10.0 (Int.toNumber exp_))
              Nothing ->
                pure (TokInt rawInt (Int.round int_))
          Tuple (Just (Tuple rawInt intStr)) (Just (Tuple rawFrac fracStr)) -> do
            let Tuple mantissa exp0 = digitsToScientific intStr fracStr
            mbExp <- exponent
            case mbExp of
              Just (Tuple rawExp exp_) ->
                sciDouble (rawInt <> rawFrac <> rawExp)
                  (mantissa * Number.pow 10.0 (Int.toNumber (exp0 + exp_)))
              Nothing ->
                sciDouble (rawInt <> rawFrac)
                  (mantissa * Number.pow 10.0 (Int.toNumber exp0))
          Tuple Nothing (Just (Tuple rawFrac fracStr)) -> do
            let Tuple mantissa exp0 = digitsToScientific "" fracStr
            mbExp <- exponent
            case mbExp of
              Just (Tuple rawExp exp_) ->
                sciDouble (rawFrac <> rawExp)
                  (mantissa * Number.pow 10.0 (Int.toNumber (exp0 + exp_)))
              Nothing ->
                sciDouble rawFrac
                  (mantissa * Number.pow 10.0 (Int.toNumber exp0))
          Tuple Nothing Nothing -> do
            mbCh <- peek
            throw (ErrLexeme (map SCU.singleton mbCh) [])

  sciDouble :: String -> Number -> Lexer Token
  sciDouble raw n =
    if Number.isFinite n
      then pure (TokNumber raw n)
      else throw ErrNumberOutOfRange

  integer :: Lexer (Maybe (Tuple String String))
  integer = peek >>= \mbCh ->
    case mbCh of
      Just '0' -> next *> peek >>= \mbCh2 ->
        case mbCh2 of
          Just ch | isNumberChar ch -> throw ErrLeadingZero
          _ -> pure (Just (Tuple "0" "0"))
      Just ch | isDigit ch -> map Just digits
      _ -> pure Nothing

  integer1 :: Char -> Lexer (Maybe (Tuple String String))
  integer1 ch = case ch of
    '0' -> peek >>= \mbCh2 ->
      case mbCh2 of
        Just c | isNumberChar c -> throw ErrLeadingZero
        _ -> pure (Just (Tuple "0" "0"))
    _ | isDigit ch -> do
      Tuple raw chs <- digits
      pure (Just (Tuple (SCU.singleton ch <> raw) (SCU.singleton ch <> chs)))
    _ -> pure Nothing

  fraction :: Lexer (Maybe (Tuple String String))
  fraction = Parser \inp _ ksucc ->
    case SCU.uncons inp of
      Just { head: '.', tail: inp' } ->
        let n = SCU.countPrefix isNumberChar inp'
        in if n > 0
           then let raw = SCU.take n inp'
                    inp'' = SCU.drop n inp'
                    filt = SCU.fromCharArray (Array.foldl (\a c -> if c == '_' then a else Array.cons c a) [] (SCU.toCharArray raw))
                in ksucc inp'' (Just (Tuple ("." <> raw) filt))
           else ksucc inp Nothing
      _ -> ksucc inp Nothing

  digits :: Lexer (Tuple String String)
  digits = do
    raw <- nextWhile isNumberChar
    let chs = SCU.fromCharArray
              (Array.foldl (\a c -> if c /= '_' then Array.cons c a else a) [] (Array.reverse (SCU.toCharArray raw)))
    pure (Tuple raw chs)

  exponent :: Lexer (Maybe (Tuple String Int))
  exponent = peek >>= \mbCh ->
    case mbCh of
      Just 'e' -> do
        _ <- next
        mbSign <- peek
        Tuple neg sign <- case mbSign of
          Just '-' -> next *> pure (Tuple true "-")
          Just '+' -> next *> pure (Tuple false "+")
          _        -> pure (Tuple false "")
        mbIntR <- integer
        case mbIntR of
          Just (Tuple raw chs) ->
            let int_ = digitsToNumber chs
                int'' = if neg then negate int_ else int_
            in pure (Just (Tuple ("e" <> sign <> raw) (Int.round int'')))
          Nothing -> throw ErrExpectedExponent
      _ -> pure Nothing

  hexadecimal :: Lexer Token
  hexadecimal = do
    chs <- nextWhile isHexDigit
    if chs == ""
      then throw ErrExpectedHex
      else pure (TokInt ("0x" <> chs) (Int.round (digitsToNumberBase 16 chs)))

digitsToNumber :: String -> Number
digitsToNumber = digitsToNumberBase 10

digitsToNumberBase :: Int -> String -> Number
digitsToNumberBase b s =
  Array.foldl (\n c -> n * Int.toNumber b + Int.toNumber (digitToInt c)) 0.0
    (SCU.toCharArray s)

digitsToScientific :: String -> String -> Tuple Number Int
digitsToScientific intStr fracStr =
  go 0 (Array.reverse (SCU.toCharArray intStr)) (SCU.toCharArray fracStr)
  where
  go exp is [] =
    Tuple (Array.foldl (\n c -> n * 10.0 + Int.toNumber (digitToInt c)) 0.0 (Array.reverse is)) exp
  go exp is fs = case Array.uncons fs of
    Nothing -> Tuple (Array.foldl (\n c -> n * 10.0 + Int.toNumber (digitToInt c)) 0.0 (Array.reverse is)) exp
    Just { head: f, tail: fss } -> go (exp - 1) (Array.cons f is) fss

digitToInt :: Char -> Int
digitToInt c
  | c >= '0' && c <= '9' = toCharCode c - toCharCode '0'
  | c >= 'a' && c <= 'f' = toCharCode c - toCharCode 'a' + 10
  | c >= 'A' && c <= 'F' = toCharCode c - toCharCode 'A' + 10
  | otherwise = 0

isSymbolChar :: Char -> Boolean
isSymbolChar c =
  -- DS.contains checks substring membership (not prefix), matching Haskell's `elem`
  DS.contains (DS.Pattern (SCU.singleton c)) ":!#$%&*+./<=>?@\\^|-~"
  || (not isAscii c && UCP.isSymbol (SCP.codePointFromChar c))

isReservedSymbolError :: ParserErrorType -> Boolean
isReservedSymbolError e = e == ErrReservedSymbol

isReservedSymbol :: String -> Boolean
isReservedSymbol s = s == "::" || s == "∷" || s == "<-" || s == "←"
  || s == "->" || s == "→" || s == "=>" || s == "⇒" || s == "∀"
  || s == "|" || s == "." || s == "\\" || s == "="

isIdentStart :: Char -> Boolean
isIdentStart c = isLower' c || c == '_'

isIdentChar :: Char -> Boolean
isIdentChar c = UCP.isAlphaNum (SCP.codePointFromChar c) || c == '_' || c == '\''

isNumberChar :: Char -> Boolean
isNumberChar c = isDigit c || c == '_'

isNormalStringChar :: Char -> Boolean
isNormalStringChar c = c /= '"' && c /= '\\' && c /= '\r' && c /= '\n'

isStringGapChar :: Char -> Boolean
isStringGapChar c = c == ' ' || c == '\r' || c == '\n'

isLineFeed :: Char -> Boolean
isLineFeed c = c == '\r' || c == '\n'

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

isHexDigit :: Char -> Boolean
isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

isAscii :: Char -> Boolean
isAscii c = toCharCode c < 128

isUpper' :: Char -> Boolean
isUpper' c = UCP.isUpper (SCP.codePointFromChar c)

isLower' :: Char -> Boolean
isLower' c = UCP.isLower (SCP.codePointFromChar c)

isUnquotedKey :: String -> Boolean
isUnquotedKey t =
  case SCU.uncons t of
    Nothing -> false
    Just { head: hd, tail: tl } ->
      isIdentStart hd && SCU.countPrefix isIdentChar tl == SCU.length tl
