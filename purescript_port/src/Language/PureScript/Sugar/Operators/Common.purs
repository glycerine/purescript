module Language.PureScript.Sugar.Operators.Common
  ( Chain
  , FromOp
  , Reapply
  , matchOperators
  , token
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array as Array
import Data.Array (catMaybes, mapMaybe)
import Data.Either (Either(..), either)
import Data.Identity (Identity)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst)
import Parsing (ParseState(..), Parser, ParserT, getParserT, runParser, stateParserT, fail)
import Parsing.Combinators (try, (<?>))
import Parsing.Expr (buildExprParser, Operator(..), Assoc(..)) as PE

import Language.PureScript.AST.Declarations (ErrorMessageHint(..))
import Language.PureScript.AST.Operators (Associativity(..))
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Errors
  ( ErrorMessage(..)
  , MultipleErrors(..)
  , SimpleErrorMessage(..)
  )
import Language.PureScript.Names (OpName, Qualified, eraseOpName)

type Chain a = Array (Either a a)

type FromOp nameType a = a -> Maybe (Tuple SourceSpan (Qualified (OpName nameType)))
type Reapply nameType a = SourceSpan -> Qualified (OpName nameType) -> a -> a -> a

toAssoc :: Associativity -> PE.Assoc
toAssoc Infixl = PE.AssocLeft
toAssoc Infixr = PE.AssocRight
toAssoc Infix  = PE.AssocNone

-- | Parse a single token from a Chain.
token :: forall a b. (Either a a -> Maybe b) -> Parser (Chain a) b
token f = do
  ParseState input pos _ <- getParserT
  case Array.uncons input of
    Nothing -> fail "unexpected end of input"
    Just { head: t, tail: rest } ->
      case f t of
        Nothing -> fail "unexpected token"
        Just v -> do
          stateParserT \_ -> Tuple unit (ParseState rest pos true)
          pure v

parseValue :: forall a. Parser (Chain a) a
parseValue = token (either Just (const Nothing)) <?> "expression"

parseOp
  :: forall nameType a
   . FromOp nameType a
  -> Parser (Chain a) (Tuple SourceSpan (Qualified (OpName nameType)))
parseOp fromOp = token (either (const Nothing) fromOp) <?> "operator"

matchOp
  :: forall nameType a
   . Eq nameType
  => FromOp nameType a
  -> Qualified (OpName nameType)
  -> Parser (Chain a) SourceSpan
matchOp fromOp op = do
  Tuple ss ident <- parseOp fromOp
  if ident == op
    then pure ss
    else fail "operator mismatch"

opTable
  :: forall nameType a
   . Eq nameType
  => Array (Array (Tuple (Qualified (OpName nameType)) Associativity))
  -> FromOp nameType a
  -> Reapply nameType a
  -> Array (Array (PE.Operator Identity (Chain a) a))
opTable ops fromOp reapply =
  map (map (\(Tuple name assoc) ->
    PE.Infix
      (try (matchOp fromOp name) >>= \ss -> pure (reapply ss name))
      (toAssoc assoc))) ops

matchOperators
  :: forall m a nameType
   . Eq nameType
  => Ord nameType
  => MonadError MultipleErrors m
  => (a -> Boolean)
  -> (a -> Maybe (Tuple a (Tuple a a)))
  -> FromOp nameType a
  -> Reapply nameType a
  -> (Array (Array (PE.Operator Identity (Chain a) a)) -> Array (Array (PE.Operator Identity (Chain a) a)))
  -> Array (Array (Tuple (Qualified (OpName nameType)) Associativity))
  -> a
  -> m a
matchOperators isBinOp extractOp fromOp reapply modOpTable ops = parseChains
  where
  parseChains :: a -> m a
  parseChains ty
    | isBinOp ty = bracketChain (extendChain ty)
    | otherwise = pure ty

  extendChain :: a -> Chain a
  extendChain ty = case extractOp ty of
    Just (Tuple op (Tuple l r)) ->
      Array.cons (Left l) (Array.cons (Right op) (extendChain r))
    Nothing -> [Left ty]

  bracketChain :: Chain a -> m a
  bracketChain chain =
    case runParser chain opParser of
      Right a -> pure a
      Left _  -> throwError (MultipleErrors (mkErrors chain))

  opParser :: Parser (Chain a) a
  opParser = PE.buildExprParser (modOpTable (opTable ops fromOp reapply)) parseValue

  mkErrors :: Chain a -> Array ErrorMessage
  mkErrors chain =
    let
      opInfo :: Map (Qualified (OpName nameType)) (Tuple Int Associativity)
      opInfo = Map.fromFoldable
        (Array.concatMap (\(Tuple n os) -> map (\(Tuple name assoc) -> Tuple name (Tuple n assoc)) os)
          (Array.mapWithIndex Tuple ops))

      opPrec :: Qualified (OpName nameType) -> Int
      opPrec name = fromMaybe 0 (map fst (Map.lookup name opInfo))

      opAssoc :: Qualified (OpName nameType) -> Associativity
      opAssoc name = fromMaybe Infix (map (\(Tuple _ a) -> a) (Map.lookup name opInfo))

      fromRight (Left _) = Nothing
      fromRight (Right x) = Just x

      chainOps :: Array (Tuple SourceSpan (Qualified (OpName nameType)))
      chainOps = mapMaybe fromOp (catMaybes (map fromRight chain))

      chainOpSpans :: Map (Qualified (OpName nameType)) (NonEmptyList SourceSpan)
      chainOpSpans = Array.foldl (\m (Tuple ss name) ->
        Map.alter (Just <<< case _ of
          Nothing  -> NEL.singleton ss
          Just sss -> NEL.cons ss sss) name m) Map.empty chainOps

      opUsages :: Qualified (OpName nameType) -> Int
      opUsages name = maybe 0 NEL.length (Map.lookup name chainOpSpans)

      allOps = Array.fromFoldable (Map.keys chainOpSpans)
      sortedOps = Array.sortBy (\a b -> compare (opPrec a) (opPrec b)) allOps

      precGrouped :: Array (NonEmptyList (Qualified (OpName nameType)))
      precGrouped = groupBy (\a b -> opPrec a == opPrec b) sortedOps

      assocGrouped :: Array (Array (NonEmptyList (Qualified (OpName nameType))))
      assocGrouped = map (\g ->
        groupBy (\a b -> opAssoc a == opAssoc b)
          (Array.sortBy (\a b -> compare (opAssoc a) (opAssoc b)) (Array.fromFoldable g)))
        precGrouped

      mixedAssoc :: Array (NonEmptyList (Qualified (OpName nameType)))
      mixedAssoc = Array.concatMap (\g ->
        if Array.length g > 1 then g else []) assocGrouped

      nonAssoc :: Array (NonEmptyList (Qualified (OpName nameType)))
      nonAssoc = Array.concatMap (\g ->
        Array.filter (\assocG ->
          opAssoc (NEL.head assocG) == Infix &&
          Array.foldl (+) 0 (map opUsages (Array.fromFoldable assocG)) > 1) g)
        assocGrouped
    in
      if Array.null nonAssoc && Array.null mixedAssoc
        then [ErrorMessage [] (InternalCompilerError "matchOperators" "cannot reorder operators")]
        else
          map (\grp ->
            mkPositionedError chainOpSpans grp
              (MixedAssociativityError (map (\name -> Tuple (eraseOpName <$> name) (opAssoc name)) grp)))
            mixedAssoc
          <> map (\grp ->
            mkPositionedError chainOpSpans grp
              (NonAssociativeError (map (map eraseOpName) grp)))
            nonAssoc

  groupBy :: forall a. (a -> a -> Boolean) -> Array a -> Array (NonEmptyList a)
  groupBy _ [] = []
  groupBy eq arr = case Array.uncons arr of
    Nothing -> []
    Just { head: x, tail: xs } ->
      let same = Array.takeWhile (eq x) xs
          rest = Array.dropWhile (eq x) xs
          nel = case NEL.fromFoldable (Array.cons x same) of
            Just nel' -> nel'
            Nothing -> NEL.singleton x
      in Array.cons nel (groupBy eq rest)

  mkPositionedError
    :: Map (Qualified (OpName nameType)) (NonEmptyList SourceSpan)
    -> NonEmptyList (Qualified (OpName nameType))
    -> SimpleErrorMessage
    -> ErrorMessage
  mkPositionedError chainOpSpans grp sem =
    let spans = Array.concatMap (\name -> maybe [] Array.fromFoldable (Map.lookup name chainOpSpans)) (Array.fromFoldable grp)
    in case NEL.fromFoldable spans of
         Just nel -> ErrorMessage [PositionedError nel] sem
         Nothing  -> ErrorMessage [] sem
