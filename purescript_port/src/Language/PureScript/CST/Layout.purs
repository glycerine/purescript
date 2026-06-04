-- | Layout algorithm for PureScript's indentation-sensitive syntax.
-- Inserts virtual layout tokens (TokLayoutStart, TokLayoutSep, TokLayoutEnd)
-- based on indentation.
module Language.PureScript.CST.Layout
  ( LayoutStack
  , LayoutDelim(..)
  , isIndented
  , isTopDecl
  , lytToken
  , insertLayout
  , unwindLayout
  ) where

import Prelude

import Data.Array (cons, find, snoc, uncons) as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Language.PureScript.CST.Types
  ( CSTSourcePos(..)
  , Comment
  , LineFeed
  , SourceRange(..)
  , SourceStyle(..)
  , SourceToken(..)
  , Token(..)
  , TokenAnn(..)
  )

type LayoutStack = Array (Tuple CSTSourcePos LayoutDelim)

data LayoutDelim
  = LytRoot
  | LytTopDecl
  | LytTopDeclHead
  | LytDeclGuard
  | LytCase
  | LytCaseBinders
  | LytCaseGuard
  | LytLambdaBinders
  | LytParen
  | LytBrace
  | LytSquare
  | LytIf
  | LytThen
  | LytProperty
  | LytForall
  | LytTick
  | LytLet
  | LytLetStmt
  | LytWhere
  | LytOf
  | LytDo
  | LytAdo

derive instance eqLayoutDelim  :: Eq LayoutDelim
derive instance ordLayoutDelim :: Ord LayoutDelim

instance showLayoutDelim :: Show LayoutDelim where
  show d = case d of
    LytRoot          -> "LytRoot"
    LytTopDecl       -> "LytTopDecl"
    LytTopDeclHead   -> "LytTopDeclHead"
    LytDeclGuard     -> "LytDeclGuard"
    LytCase          -> "LytCase"
    LytCaseBinders   -> "LytCaseBinders"
    LytCaseGuard     -> "LytCaseGuard"
    LytLambdaBinders -> "LytLambdaBinders"
    LytParen         -> "LytParen"
    LytBrace         -> "LytBrace"
    LytSquare        -> "LytSquare"
    LytIf            -> "LytIf"
    LytThen          -> "LytThen"
    LytProperty      -> "LytProperty"
    LytForall        -> "LytForall"
    LytTick          -> "LytTick"
    LytLet           -> "LytLet"
    LytLetStmt       -> "LytLetStmt"
    LytWhere         -> "LytWhere"
    LytOf            -> "LytOf"
    LytDo            -> "LytDo"
    LytAdo           -> "LytAdo"

isIndented :: LayoutDelim -> Boolean
isIndented d = case d of
  LytLet     -> true
  LytLetStmt -> true
  LytWhere   -> true
  LytOf      -> true
  LytDo      -> true
  LytAdo     -> true
  _          -> false

getCol :: CSTSourcePos -> Int
getCol (CSTSourcePos { srcColumn }) = srcColumn

getLine :: CSTSourcePos -> Int
getLine (CSTSourcePos { srcLine }) = srcLine

isTopDecl :: CSTSourcePos -> LayoutStack -> Boolean
isTopDecl tokPos stk = case stk of
  [Tuple lytPos LytWhere, Tuple _ LytRoot]
    | getCol tokPos == getCol lytPos -> true
  _ -> false

lytToken :: CSTSourcePos -> Token -> SourceToken
lytToken pos tok = SourceToken
  { tokAnn: TokenAnn
    { tokRange: SourceRange { srcStart: pos, srcEnd: pos }
    , tokLeadingComments: []
    , tokTrailingComments: []
    }
  , tokValue: tok
  }

-- | Main layout insertion function. Given the current source token, the
-- position of the next token, and the current layout stack, returns the
-- updated stack and the sequence of tokens to emit (including virtual tokens).
insertLayout
  :: SourceToken
  -> CSTSourcePos
  -> LayoutStack
  -> Tuple LayoutStack (Array SourceToken)
insertLayout src@(SourceToken { tokAnn, tokValue: tok }) nextPos stack =
  insert (Tuple stack [])
  where
  tokPos = case tokAnn of
    TokenAnn { tokRange: SourceRange { srcStart } } -> srcStart

  -- helpers for stack manipulation
  pushStack :: CSTSourcePos -> LayoutDelim -> Tuple LayoutStack (Array SourceToken) -> Tuple LayoutStack (Array SourceToken)
  pushStack lytPos lyt (Tuple stk acc) = Tuple (Array.cons (Tuple lytPos lyt) stk) acc

  popStack :: (LayoutDelim -> Boolean) -> Tuple LayoutStack (Array SourceToken) -> Tuple LayoutStack (Array SourceToken)
  popStack p state@(Tuple stk acc) = case Array.uncons stk of
    Just { head: Tuple _ lyt, tail: rest } | p lyt -> Tuple rest acc
    _ -> state

  insertToken :: SourceToken -> Tuple LayoutStack (Array SourceToken) -> Tuple LayoutStack (Array SourceToken)
  insertToken token (Tuple stk acc) = Tuple stk (Array.snoc acc token)

  insertEnd :: Tuple LayoutStack (Array SourceToken) -> Tuple LayoutStack (Array SourceToken)
  insertEnd = insertToken (lytToken tokPos TokLayoutEnd)

  collapse :: (CSTSourcePos -> LayoutDelim -> Boolean) -> Tuple LayoutStack (Array SourceToken) -> Tuple LayoutStack (Array SourceToken)
  collapse p (Tuple stk acc) = go stk acc
    where
    go stk' acc' = case Array.uncons stk' of
      Just { head: Tuple lytPos lyt, tail: rest }
        | p lytPos lyt ->
            go rest (if isIndented lyt then Array.snoc acc' (lytToken tokPos TokLayoutEnd) else acc')
      _ -> Tuple stk' acc'

  insertStart :: LayoutDelim -> Tuple LayoutStack (Array SourceToken) -> Tuple LayoutStack (Array SourceToken)
  insertStart lyt state@(Tuple stk _) =
    case Array.find (isIndented <<< snd) stk of
      Just (Tuple pos _) | getCol nextPos <= getCol pos -> state
      _ -> state # pushStack nextPos lyt # insertToken (lytToken nextPos TokLayoutStart)

  insertSep :: Tuple LayoutStack (Array SourceToken) -> Tuple LayoutStack (Array SourceToken)
  insertSep state@(Tuple stk acc) = case Array.uncons stk of
    Just { head: Tuple lytPos LytTopDecl, tail: rest }
      | sepP lytPos ->
          Tuple rest acc # insertToken sepTok
    Just { head: Tuple lytPos LytTopDeclHead, tail: rest }
      | sepP lytPos ->
          Tuple rest acc # insertToken sepTok
    Just { head: Tuple lytPos lyt }
      | indentSepP lytPos lyt ->
          case lyt of
            LytOf -> state # insertToken sepTok # pushStack tokPos LytCaseBinders
            _     -> state # insertToken sepTok
    _ -> state
    where
    sepTok = lytToken tokPos TokLayoutSep

  insertDefault :: Tuple LayoutStack (Array SourceToken) -> Tuple LayoutStack (Array SourceToken)
  insertDefault state = state # collapse offsideP # insertSep # insertToken src

  insertKwProperty :: (Tuple LayoutStack (Array SourceToken) -> Tuple LayoutStack (Array SourceToken)) -> Tuple LayoutStack (Array SourceToken) -> Tuple LayoutStack (Array SourceToken)
  insertKwProperty k state =
    case state # insertDefault of
      state'@(Tuple stk' _) -> case Array.uncons stk' of
        Just { head: Tuple _ LytProperty, tail: rest' } ->
          Tuple rest' (case state' of Tuple _ acc' -> acc')
        _ -> k state'

  indentedP :: CSTSourcePos -> LayoutDelim -> Boolean
  indentedP _ = isIndented

  offsideP :: CSTSourcePos -> LayoutDelim -> Boolean
  offsideP lytPos lyt = isIndented lyt && getCol tokPos < getCol lytPos

  offsideEndP :: CSTSourcePos -> LayoutDelim -> Boolean
  offsideEndP lytPos lyt = isIndented lyt && getCol tokPos <= getCol lytPos

  indentSepP :: CSTSourcePos -> LayoutDelim -> Boolean
  indentSepP lytPos lyt = isIndented lyt && sepP lytPos

  sepP :: CSTSourcePos -> Boolean
  sepP lytPos = getCol tokPos == getCol lytPos && getLine tokPos /= getLine lytPos

  insert :: Tuple LayoutStack (Array SourceToken) -> Tuple LayoutStack (Array SourceToken)
  insert state@(Tuple stk acc) = case tok of
    TokLowerName [] "data" ->
      case state # insertDefault of
        state'@(Tuple stk' _)
          | isTopDecl tokPos stk' ->
              state' # pushStack tokPos LytTopDecl
        state' ->
          state' # popStack (_ == LytProperty)

    TokLowerName [] "class" ->
      case state # insertDefault of
        state'@(Tuple stk' _)
          | isTopDecl tokPos stk' ->
              state' # pushStack tokPos LytTopDeclHead
        state' ->
          state' # popStack (_ == LytProperty)

    TokLowerName [] "where" ->
      case Array.uncons stk of
        Just { head: Tuple _ LytTopDeclHead, tail: stk' } ->
          Tuple stk' acc # insertToken src # insertStart LytWhere
        Just { head: Tuple _ LytProperty, tail: stk' } ->
          Tuple stk' acc # insertToken src
        _ ->
          state # collapse whereP # insertToken src # insertStart LytWhere
      where
      whereP _ LytDo = true
      whereP lytPos lyt = offsideEndP lytPos lyt

    TokLowerName [] "in" ->
      case collapse inP state of
        state'@(Tuple stk' _) ->
          case Array.uncons stk' of
            Just { head: Tuple _ LytLetStmt, tail: stk'' } ->
              case Array.uncons stk'' of
                Just { head: Tuple _ LytAdo, tail: stk''' } ->
                  Tuple stk''' (case state' of Tuple _ acc' -> acc') # insertEnd # insertEnd # insertToken src
                _ -> insertDefault (Tuple stk' (case state' of Tuple _ a -> a)) # popStack (_ == LytProperty)
            Just { head: Tuple _ lyt, tail: stk'' } | isIndented lyt ->
              Tuple stk'' (case state' of Tuple _ acc' -> acc') # insertEnd # insertToken src
            _ ->
              state # insertDefault # popStack (_ == LytProperty)
      where
      inP _ LytLet = false
      inP _ LytAdo = false
      inP _ lyt    = isIndented lyt

    TokLowerName [] "let" ->
      state # insertKwProperty next
      where
      next state'@(Tuple stk' _) = case Array.uncons stk' of
        Just { head: Tuple p LytDo }
          | getCol p == getCol tokPos ->
              state' # insertStart LytLetStmt
        Just { head: Tuple p LytAdo }
          | getCol p == getCol tokPos ->
              state' # insertStart LytLetStmt
        _ ->
          state' # insertStart LytLet

    TokLowerName _ "do" ->
      state # insertKwProperty (insertStart LytDo)

    TokLowerName _ "ado" ->
      state # insertKwProperty (insertStart LytAdo)

    TokLowerName [] "case" ->
      state # insertKwProperty (pushStack tokPos LytCase)

    TokLowerName [] "of" ->
      case collapse indentedP state of
        state'@(Tuple stk' _) ->
          case Array.uncons stk' of
            Just { head: Tuple _ LytCase, tail: stk'' } ->
              Tuple stk'' (case state' of Tuple _ acc' -> acc') # insertToken src # insertStart LytOf # pushStack nextPos LytCaseBinders
            _ ->
              state' # insertDefault # popStack (_ == LytProperty)

    TokLowerName [] "if" ->
      state # insertKwProperty (pushStack tokPos LytIf)

    TokLowerName [] "then" ->
      case state # collapse indentedP of
        state'@(Tuple stk' _) ->
          case Array.uncons stk' of
            Just { head: Tuple _ LytIf, tail: stk'' } ->
              Tuple stk'' (case state' of Tuple _ acc' -> acc') # insertToken src # pushStack tokPos LytThen
            _ ->
              state # insertDefault # popStack (_ == LytProperty)

    TokLowerName [] "else" ->
      case state # collapse indentedP of
        state'@(Tuple stk' _) ->
          case Array.uncons stk' of
            Just { head: Tuple _ LytThen, tail: stk'' } ->
              Tuple stk'' (case state' of Tuple _ acc' -> acc') # insertToken src
            _ ->
              case state # collapse offsideP of
                state''@(Tuple stk'' _)
                  | isTopDecl tokPos stk'' ->
                      state'' # insertToken src
                state'' ->
                  state'' # insertSep # insertToken src # popStack (_ == LytProperty)

    TokForall _ ->
      state # insertKwProperty (pushStack tokPos LytForall)

    TokBackslash ->
      state # insertDefault # pushStack tokPos LytLambdaBinders

    TokRightArrow _ ->
      state # collapse arrowP # popStack guardP # insertToken src
      where
      arrowP _ LytDo     = true
      arrowP _ LytOf     = false
      arrowP lytPos lyt  = offsideEndP lytPos lyt

      guardP LytCaseBinders   = true
      guardP LytCaseGuard     = true
      guardP LytLambdaBinders = true
      guardP _                = false

    TokEquals ->
      case state # collapse equalsP of
        state'@(Tuple stk' _) ->
          case Array.uncons stk' of
            Just { head: Tuple _ LytDeclGuard, tail: stk'' } ->
              Tuple stk'' (case state' of Tuple _ acc' -> acc') # insertToken src
            _ ->
              state # insertDefault
      where
      equalsP _ LytWhere   = true
      equalsP _ LytLet     = true
      equalsP _ LytLetStmt = true
      equalsP _ _          = false

    TokPipe ->
      case collapse offsideEndP state of
        state'@(Tuple stk' _) ->
          case Array.uncons stk' of
            Just { head: Tuple _ LytOf }    -> state' # pushStack tokPos LytCaseGuard # insertToken src
            Just { head: Tuple _ LytLet }   -> state' # pushStack tokPos LytDeclGuard # insertToken src
            Just { head: Tuple _ LytLetStmt } -> state' # pushStack tokPos LytDeclGuard # insertToken src
            Just { head: Tuple _ LytWhere } -> state' # pushStack tokPos LytDeclGuard # insertToken src
            _                               -> state # insertDefault

    TokTick ->
      case state # collapse indentedP of
        state'@(Tuple stk' _) ->
          case Array.uncons stk' of
            Just { head: Tuple _ LytTick, tail: stk'' } ->
              Tuple stk'' (case state' of Tuple _ acc' -> acc') # insertToken src
            _ ->
              state # collapse offsideEndP # insertSep # insertToken src # pushStack tokPos LytTick

    TokComma ->
      case state # collapse indentedP of
        state'@(Tuple stk' _) ->
          case Array.uncons stk' of
            Just { head: Tuple _ LytBrace } ->
              state' # insertToken src # pushStack tokPos LytProperty
            _ ->
              state' # insertToken src

    TokDot ->
      case state # insertDefault of
        state'@(Tuple stk' _) ->
          case Array.uncons stk' of
            Just { head: Tuple _ LytForall, tail: stk'' } ->
              Tuple stk'' (case state' of Tuple _ acc' -> acc')
            _ ->
              state' # pushStack tokPos LytProperty

    TokLeftParen ->
      state # insertDefault # pushStack tokPos LytParen

    TokLeftBrace ->
      state # insertDefault # pushStack tokPos LytBrace # pushStack tokPos LytProperty

    TokLeftSquare ->
      state # insertDefault # pushStack tokPos LytSquare

    TokRightParen ->
      state # collapse indentedP # popStack (_ == LytParen) # insertToken src

    TokRightBrace ->
      state # collapse indentedP # popStack (_ == LytProperty) # popStack (_ == LytBrace) # insertToken src

    TokRightSquare ->
      state # collapse indentedP # popStack (_ == LytSquare) # insertToken src

    TokString _ _ ->
      state # insertDefault # popStack (_ == LytProperty)

    TokLowerName [] _ ->
      state # insertDefault # popStack (_ == LytProperty)

    TokOperator _ _ ->
      state # collapse offsideEndP # insertSep # insertToken src

    _ ->
      state # insertDefault

unwindLayout :: CSTSourcePos -> Array (Comment LineFeed) -> LayoutStack -> Array SourceToken
unwindLayout pos leading stk = go stk
  where
  go stack = case Array.uncons stack of
    Nothing -> []
    Just { head: Tuple _ LytRoot } ->
      [ SourceToken
        { tokAnn: TokenAnn
          { tokRange: SourceRange { srcStart: pos, srcEnd: pos }
          , tokLeadingComments: leading
          , tokTrailingComments: []
          }
        , tokValue: TokEof
        }
      ]
    Just { head: Tuple _ lyt, tail: rest }
      | isIndented lyt ->
          Array.cons (lytToken pos TokLayoutEnd) (go rest)
    Just { tail: rest } ->
      go rest
