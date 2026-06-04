module Language.PureScript.CST.Monad
  ( LexResult
  , LexState(..)
  , ParserState(..)
  , ParserM(..)
  , Parser
  , runParser
  , runTokenParser
  , throw
  , parseError
  , mkParserError
  , addFailure
  , parseFail'
  , parseFail
  , addWarning
  , pushBack
  , tryPrefix
  , oneOf
  , manyDelimited
  , token
  , munch
  ) where

import Prelude

import Control.Lazy (class Lazy)
import Data.Array (cons, drop, head, reverse, sortBy, uncons) as Array
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (fromFoldable, head, sortBy, toUnfoldable) as NEL
import Data.Maybe (Maybe(..), fromJust)
import Data.Ord (comparing)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Language.PureScript.CST.Errors
  ( ParserError
  , ParserErrorInfo(..)
  , ParserErrorType(..)
  , ParserWarning
  , ParserWarningType
  )
import Language.PureScript.CST.Layout (LayoutStack)
import Language.PureScript.CST.Positions (widen)
import Language.PureScript.CST.Types
  ( CSTSourcePos(..)
  , Comment
  , LineFeed
  , SourceRange(..)
  , SourceToken(..)
  , Token(..)
  , TokenAnn(..)
  )

type LexResult = Either (Tuple LexState ParserError) SourceToken

data LexState = LexState
  { lexPos     :: CSTSourcePos
  , lexLeading :: Array (Comment LineFeed)
  , lexSource  :: String
  , lexStack   :: LayoutStack
  }

instance showLexState :: Show LexState where
  show _ = "<LexState>"

data ParserState = ParserState
  { parserBuff     :: Array LexResult
  , parserErrors   :: Array ParserError
  , parserWarnings :: Array ParserWarning
  }

instance showParserState :: Show ParserState where
  show _ = "<ParserState>"

-- | CPS-based parser monad: StateT ParserState (Except ParserError) a
newtype ParserM e s a =
  Parser (forall r. s -> (s -> e -> r) -> (s -> a -> r) -> r)

type Parser = ParserM ParserError ParserState

instance functorParserM :: Functor (ParserM e s) where
  map f (Parser k) =
    Parser \st kerr ksucc ->
      k st kerr (\st' a -> ksucc st' (f a))

instance applyParserM :: Apply (ParserM e s) where
  apply (Parser k1) (Parser k2) =
    Parser \st kerr ksucc ->
      k1 st kerr (\st' f ->
        k2 st' kerr (\st'' a ->
          ksucc st'' (f a)))

instance applicativeParserM :: Applicative (ParserM e s) where
  pure a = Parser \st _ k -> k st a

instance bindParserM :: Bind (ParserM e s) where
  bind (Parser k1) k2 =
    Parser \st kerr ksucc ->
      k1 st kerr (\st' a ->
        let Parser k3 = k2 a
        in k3 st' kerr ksucc)

instance monadParserM :: Monad (ParserM e s)

instance lazyParserM :: Lazy (ParserM e s a) where
  defer f = Parser \st kerr ksucc ->
    let Parser k = f unit
    in k st kerr ksucc

runParser
  :: forall a
   . ParserState
  -> Parser a
  -> Tuple ParserState (Either (NonEmptyList ParserError) a)
runParser st (Parser k) = k st left right
  where
  left st'@(ParserState ps) err =
    Tuple st' (Left
      (NEL.sortBy (comparing (\(ParserErrorInfo p) -> p.errRange))
        (unsafePartial (fromJust (NEL.fromFoldable (Array.cons err ps.parserErrors))))))

  right st'@(ParserState ps) res =
    case ps.parserErrors of
      [] -> Tuple st' (Right res)
      _  -> Tuple st' (Left
        (NEL.sortBy (comparing (\(ParserErrorInfo p) -> p.errRange))
          (unsafePartial (fromJust (NEL.fromFoldable ps.parserErrors)))))

runTokenParser
  :: forall a
   . Parser a
  -> Array LexResult
  -> Either (NonEmptyList ParserError) (Tuple (Array ParserWarning) a)
runTokenParser p buff =
  let
    initialState = ParserState
      { parserBuff: buff
      , parserErrors: []
      , parserWarnings: []
      }
    Tuple (ParserState ps) result = runParser initialState p
    warnings = ps.parserWarnings
  in
    map (\res -> Tuple warnings res) result

throw :: forall e s a. e -> ParserM e s a
throw e = Parser \st kerr _ -> kerr st e

parseError :: forall a. SourceToken -> Parser a
parseError tok = Parser \st kerr _ ->
  kerr st (ParserErrorInfo
    { errRange: case tok of SourceToken { tokAnn: TokenAnn { tokRange } } -> tokRange
    , errToks: [tok]
    , errStack: []
    , errType: ErrToken
    })

mkParserError :: forall a. LayoutStack -> Array SourceToken -> a -> ParserErrorInfo a
mkParserError stack toks ty =
  ParserErrorInfo
    { errRange: range
    , errToks: toks
    , errStack: stack
    , errType: ty
    }
  where
  range = case Tuple (Array.head toks) (lastTok toks) of
    Tuple (Just h) (Just l) -> widen
      (case h of SourceToken { tokAnn: TokenAnn { tokRange } } -> tokRange)
      (case l of SourceToken { tokAnn: TokenAnn { tokRange } } -> tokRange)
    _ -> SourceRange
      { srcStart: CSTSourcePos { srcLine: 0, srcColumn: 0 }
      , srcEnd:   CSTSourcePos { srcLine: 0, srcColumn: 0 }
      }

  lastTok :: Array SourceToken -> Maybe SourceToken
  lastTok = go Nothing
    where
    go acc arr = case Array.head arr of
      Nothing -> acc
      Just h  -> go (Just h) (tailArr arr)

  tailArr :: forall x. Array x -> Array x
  tailArr = Array.drop 1

addFailure :: Array SourceToken -> ParserErrorType -> Parser Unit
addFailure toks ty = Parser \(ParserState ps) _ ksucc ->
  ksucc (ParserState ps
    { parserErrors = Array.cons (mkParserError [] toks ty) ps.parserErrors })
  unit

parseFail' :: forall a. Array SourceToken -> ParserErrorType -> Parser a
parseFail' toks msg = Parser \st kerr _ -> kerr st (mkParserError [] toks msg)

parseFail :: forall a. SourceToken -> ParserErrorType -> Parser a
parseFail tok = parseFail' [tok]

addWarning :: Array SourceToken -> ParserWarningType -> Parser Unit
addWarning toks ty = Parser \(ParserState ps) _ ksucc ->
  ksucc (ParserState ps
    { parserWarnings = Array.cons (mkParserError [] toks ty) ps.parserWarnings })
  unit

pushBack :: SourceToken -> Parser Unit
pushBack tok = Parser \(ParserState ps) _ ksucc ->
  ksucc (ParserState ps { parserBuff = Array.cons (Right tok) ps.parserBuff })
  unit

tryPrefix :: forall a b. Parser a -> Parser b -> Parser (Tuple (Maybe a) b)
tryPrefix (Parser lhs) rhs = Parser \st kerr ksucc ->
  lhs st
    (\_ _ ->
      let Parser k = map (Tuple Nothing) rhs
      in k st kerr ksucc)
    (\st' res ->
      let Parser k = map (Tuple (Just res)) rhs
      in k st' kerr ksucc)

-- | Try each parser in order, stopping at the first success (short-circuit).
-- | This matches Haskell's lazy behavior where foldr1 short-circuits on first Right.
oneOf :: forall a. NonEmptyList (Parser a) -> Parser a
oneOf parsers = Parser \st kerr ksucc ->
  let
    prevErrs = case st of ParserState ps -> ps.parserErrors
    emptyErrs (ParserState ps) = ParserState ps { parserErrors = [] }
    addPrev' prev (ParserState ps) = ParserState ps { parserErrors = prev <> ps.parserErrors }

    -- Try parsers sequentially, short-circuiting on first success
    go :: Array (Parser a) -> Maybe (Tuple ParserState (NonEmptyList ParserError)) -> Tuple ParserState (Either (NonEmptyList ParserError) a)
    go arr bestErr = case Array.uncons arr of
      Nothing ->
        case bestErr of
          Just (Tuple st' errs) -> Tuple st' (Left errs)
          Nothing -> Tuple st (Left (NEL.sortBy (comparing errRange) (unsafePartial fromJust (NEL.fromFoldable []))))
      Just { head: p, tail: rest } ->
        case runParser (emptyErrs st) p of
          Tuple st' (Right a) -> Tuple st' (Right a)  -- SUCCESS: stop here
          Tuple st' (Left errs) ->
            let newBest = case bestErr of
                  Nothing -> Just (Tuple st' errs)
                  Just (Tuple _ bestErrs) ->
                    if errRange (NEL.head errs) > errRange (NEL.head bestErrs)
                    then Just (Tuple st' errs)
                    else bestErr
            in go rest newBest

    finalResult = go (NEL.toUnfoldable parsers :: Array (Parser a)) Nothing
  in
    case finalResult of
      Tuple st' (Left errs) ->
        kerr (addPrev' prevErrs st') (NEL.head errs)
      Tuple st' (Right res) ->
        ksucc (addPrev' prevErrs st') res
  where
  errRange (ParserErrorInfo p) = p.errRange

manyDelimited :: forall a. Token -> Token -> Token -> Parser a -> Parser (Array a)
manyDelimited open close sep p = do
  _   <- token open
  res <- go1
  _   <- token close
  pure res
  where
  go1 :: Parser (Array a)
  go1 = oneOf (unsafePartial (fromJust (NEL.fromFoldable
    [ go2 <<< Array.cons =<< p
    , pure []
    ])))

  go2 :: (Array a -> Array a) -> Parser (Array a)
  go2 k = oneOf (unsafePartial (fromJust (NEL.fromFoldable
    [ token sep *> (((\x -> go2 (k <<< Array.cons x)) =<< p))
    , pure (Array.reverse (k []))
    ])))

token :: Token -> Parser SourceToken
token t = do
  t' <- munch
  if (case t' of SourceToken { tokValue: v } -> v) == t
    then pure t'
    else parseError t'

munch :: Parser SourceToken
munch = Parser \state@(ParserState ps) kerr ksucc ->
  case ps.parserBuff of
    [] -> unsafeError "Empty input"
    _ ->
      case Array.head ps.parserBuff of
        Just (Right tok) ->
          ksucc (ParserState ps { parserBuff = Array.drop 1 ps.parserBuff }) tok
        Just (Left (Tuple _ err)) ->
          kerr state err
        Nothing ->
          unsafeError "Impossible: head of non-empty array is Nothing"

foreign import unsafeError :: forall a. String -> a
