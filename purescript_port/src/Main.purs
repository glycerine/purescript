module Main where

import Prelude

import Data.Argonaut.Core (stringify, toObject) as Json
import Data.Argonaut.Parser (jsonParser) as Json
import Foreign.Object as FO
import Data.Array (filter, foldM, length, null) as Array
import Data.Either (Either(..))
import Data.List.NonEmpty (head) as NEL
import Data.Maybe (Maybe(..))
import Data.String (indexOf, take, drop, length, Pattern(..)) as Data.String
import Data.String.CodeUnits (takeRight) as SCU
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir, writeTextFile)
import Node.Path (concat) as Path
import Control.Monad.Except (runExcept)
import Control.Monad.Writer (runWriterT)

import Language.PureScript.CST.Lexer (lex)
import Language.PureScript.CST.Parser (parseModule, PartialResult(..))
import Language.PureScript.CST.Errors (prettyPrintError)
import Language.PureScript.CST.Convert (convertModule)
import Language.PureScript.CoreFn.ToJSON (moduleToJSON)
import Language.PureScript.Errors (ErrorMessage(..), MultipleErrors(..), SimpleErrorMessage(..), unwrapErrorMessage)
import Language.PureScript.Make (rebuildModule, rebuildModuleWithCoreFn)
import Language.PureScript.Externs (ExternsFile(..))

main :: Effect Unit
main = do
  log "PureScript compiler port (PS to PS)"
  log ""
  log "=== Phase 1: CST Parse Tests ==="
  testParse "module Main where" "minimal module"
  testParse "module Data.Foo where\nimport Prelude" "module with import"
  testParse "module Main where\n\ndata Bool = True | False" "module with data type"
  testParse "module Main where\n\ndata Maybe a = Nothing | Just a" "parametric data type"
  testParse "module Main where\n\ntype Name = String" "type synonym"
  testParse "module Main where\n\nf :: Int -> Int\nf x = x" "function with signature"
  testParse "module Main where\n\nimport Data.Maybe (Maybe(..))\nimport Prelude\n\nfoo :: Int\nfoo = 42" "imports then decls"
  testParse "module Main where\n\nimport Prelude\n\nfoo :: Int\nfoo = 42" "import then decl"
  testParse "module Main where\n\nimport Data.Maybe (Maybe(..))" "import with (..)"
  testParse "module Main where\n\nimport Data.Maybe (Maybe)" "import without (..)"
  testParse "module Main where\n\nclass Eq a where\n  eq :: a -> a -> Boolean" "type class"
  testParse "module Main where\n\ninstance eqInt :: Eq Int where\n  eq x y = eqIntImpl x y" "instance decl"
  testParse "module Main where\n\nf :: Int -> Int\nf x = case x of\n  0 -> 1\n  n -> n + 1" "case expression"
  testParse "module Main where\n\nf :: Int -> Int\nf x = let y = x + 1 in y" "let expression"
  testParse "module Foo.Bar.Baz where" "qualified module name"
  log ""
  log "=== Phase 2: Full Pipeline Compilation Tests ==="
  testCompile "module Test where\n\nid :: forall a. a -> a\nid x = x" "Test" "identity function"
  testCompile "module Test where\n\nconst :: forall a b. a -> b -> a\nconst x _ = x" "Test" "const function"
  testCompile "module Test where\n\nflip :: forall a b c. (a -> b -> c) -> b -> a -> c\nflip f b a = f a b" "Test" "flip function"
  testCompile "module Test where\n\ndata Maybe a = Nothing | Just a" "Test" "Maybe data type"
  testCompile "module Test where\n\ndata List a = Nil | Cons a (List a)" "Test" "List data type"
  testCompile "module Test where\n\ntype Pair a b = { fst :: a, snd :: b }" "Test" "type synonym"
  testCompile "module CaseTest where\n\ndata Maybe a = Nothing | Just a\n\nfromMaybe :: forall a. a -> Maybe a -> a\nfromMaybe def Nothing  = def\nfromMaybe _   (Just x) = x" "CaseTest" "case function"
  testCompile "module Test where\n\nlet_ :: forall a b. a -> b -> a\nlet_ x y = let z = x in z" "Test" "let expression"
  testCompile "module Test where\n\nnewtype Wrapper a = Wrapper a\n\nunwrap :: forall a. Wrapper a -> a\nunwrap (Wrapper x) = x" "Test" "newtype"
  log ""
  log "=== Phase 3: Real Test Files (CST Parse) ==="
  testDir "../tests/purs/passing"
  testDir "../tests/purs/failing"
  testDir "../tests/purs/warning"
  log ""
  log "=== Phase 4: CoreFn JSON Output ==="
  testCoreFnOutput "module Identity where\n\nid :: forall a. a -> a\nid x = x" "Identity" "/tmp/ps_compare/port/Identity"
  testCoreFnOutput "module Const where\n\nconst :: forall a b. a -> b -> a\nconst x _ = x" "Const" "/tmp/ps_compare/port/Const"
  testCoreFnOutput "module DataTypes where\n\ndata Maybe a = Nothing | Just a\n\ndata List a = Nil | Cons a (List a)" "DataTypes" "/tmp/ps_compare/port/DataTypes"
  testCoreFnOutput "module Flip where\n\nflip :: forall a b c. (a -> b -> c) -> b -> a -> c\nflip f b a = f a b" "Flip" "/tmp/ps_compare/port/Flip"
  testCoreFnOutput "module CaseTest where\n\ndata Maybe a = Nothing | Just a\n\nfromMaybe :: forall a. a -> Maybe a -> a\nfromMaybe def Nothing  = def\nfromMaybe _   (Just x) = x" "CaseTest" "/tmp/ps_compare/port/CaseTest"
  testCoreFnOutput "module LetTest where\n\nlet_ :: forall a b. a -> b -> a\nlet_ x y = let z = x in z" "LetTest" "/tmp/ps_compare/port/LetTest"
  testCoreFnOutput "module Newtype where\n\nnewtype Wrapper a = Wrapper a\n\nunwrap :: forall a. Wrapper a -> a\nunwrap (Wrapper x) = x" "Newtype" "/tmp/ps_compare/port/Newtype"
  log ""
  log "=== Phase 5: CoreFn Comparison with Haskell Reference ==="
  compareCoreFn "Identity" "/tmp/ps_compare/reference/Identity/corefn.json" "/tmp/ps_compare/port/Identity/corefn.json"
  compareCoreFn "Const" "/tmp/ps_compare/reference/Const/corefn.json" "/tmp/ps_compare/port/Const/corefn.json"
  compareCoreFn "DataTypes" "/tmp/ps_compare/reference/DataTypes/corefn.json" "/tmp/ps_compare/port/DataTypes/corefn.json"
  compareCoreFn "Flip" "/tmp/ps_compare/reference2/Flip/corefn.json" "/tmp/ps_compare/port/Flip/corefn.json"
  compareCoreFn "CaseTest" "/tmp/ps_compare/reference2/CaseTest/corefn.json" "/tmp/ps_compare/port/CaseTest/corefn.json"
  compareCoreFn "LetTest" "/tmp/ps_compare/reference3/LetTest/corefn.json" "/tmp/ps_compare/port/LetTest/corefn.json"
  compareCoreFn "Newtype" "/tmp/ps_compare/reference3/Newtype/corefn.json" "/tmp/ps_compare/port/Newtype/corefn.json"

testDir :: String -> Effect Unit
testDir dir = do
  allFiles <- readdir dir
  let files = Array.filter (\f -> SCU.takeRight 5 f == ".purs") allFiles
  log ("\nTesting " <> show (Array.length files) <> " files from " <> dir <> ":")
  testFiles dir files

testFiles :: String -> Array String -> Effect Unit
testFiles dir files = do
  Tuple passed failed <- Array.foldM go (Tuple 0 0) files
  log ("  " <> show passed <> " passed, " <> show failed <> " failed")
  where
  go (Tuple passed failed) f = do
    let path = Path.concat [dir, f]
    src <- readTextFile UTF8 path
    let tokens = lex src
    case parseModule tokens of
      Left errs -> do
        log ("  FAIL " <> f <> ": " <> prettyPrintError (NEL.head errs))
        pure (Tuple passed (failed + 1))
      Right _ ->
        pure (Tuple (passed + 1) failed)

testParse :: String -> String -> Effect Unit
testParse src description = do
  let tokens = lex src
  case parseModule tokens of
    Left errs ->
      log ("  FAIL " <> description <> ": " <> prettyPrintError (NEL.head errs))
    Right _cst ->
      log ("  PASS " <> description)

-- | Test full compilation pipeline: parse → desugar → type-check → externs
testCompile :: String -> String -> String -> Effect Unit
testCompile src modName description = do
  let tokens = lex src
  case parseModule tokens of
    Left errs -> do
      log ("  FAIL [parse] " <> description <> ": " <> prettyPrintError (NEL.head errs))
    Right (PartialResult r) ->
      case r.resFull of
        Tuple _ (Left errs) -> do
          log ("  FAIL [parse-full] " <> description <> ": " <> prettyPrintError (NEL.head errs))
        Tuple _ (Right cstMod) -> do
          let astMod = convertModule (modName <> ".purs") cstMod
          case runExcept (runWriterT (rebuildModule [] astMod)) of
            Left errs -> do
              let MultipleErrors msgs = errs
                  showSEM (UnknownName qn) = "UnknownName " <> show qn
                  showSEM (CycleInDeclaration i) = "CycleInDeclaration " <> show i
                  showSEM (TypesDoNotUnify t1 t2) = "TypesDoNotUnify " <> show t1 <> " vs " <> show t2
                  showSEM (KindsDoNotUnify t1 t2) = "KindsDoNotUnify " <> show t1 <> " vs " <> show t2
                  showSEM (InfiniteType _) = "InfiniteType"
                  showSEM (InfiniteKind _) = "InfiniteKind"
                  showSEM (ModuleNotFound m) = "ModuleNotFound " <> show m
                  showSEM (OrphanTypeDeclaration i) = "OrphanTypeDeclaration " <> show i
                  showSEM _ = "OtherError"
                  errStr = show (Array.length msgs) <> " error(s): " <> show (map (showSEM <<< unwrapErrorMessage) msgs)
              log ("  FAIL [compile] " <> description <> ": " <> errStr)
            Right (Tuple (ExternsFile ef) _) ->
              log ("  PASS " <> description <> " [" <> show (Array.length ef.efDeclarations) <> " decls]")

-- | Produce CoreFn JSON output for comparison with Haskell compiler
testCoreFnOutput :: String -> String -> String -> Effect Unit
testCoreFnOutput src modName outputDir = do
  let tokens = lex src
  case parseModule tokens of
    Left errs ->
      log ("  FAIL [parse] " <> modName <> ": " <> prettyPrintError (NEL.head errs))
    Right (PartialResult r) ->
      case r.resFull of
        Tuple _ (Left errs) ->
          log ("  FAIL [parse-full] " <> modName <> ": " <> prettyPrintError (NEL.head errs))
        Tuple _ (Right cstMod) -> do
          let astMod = convertModule (modName <> ".purs") cstMod
          case runExcept (runWriterT (rebuildModuleWithCoreFn [] astMod)) of
            Left errs ->
              log ("  FAIL [compile] " <> modName <> ": " <> show errs)
            Right (Tuple (Tuple _ corefnMod) _) -> do
              let json = moduleToJSON "0.15.16" corefnMod
                  jsonStr = Json.stringify json
              writeTextFile UTF8 (outputDir <> "/corefn.json") jsonStr
              log ("  WROTE " <> outputDir <> "/corefn.json")

-- | Compare port CoreFn JSON against Haskell reference, ignoring modulePath.
compareCoreFn :: String -> String -> String -> Effect Unit
compareCoreFn modName refPath portPath = do
  refRaw  <- readTextFile UTF8 refPath
  portRaw <- readTextFile UTF8 portPath
  case Json.jsonParser refRaw, Json.jsonParser portRaw of
    Left e, _ -> log ("  ERROR parsing ref " <> modName <> ": " <> e)
    _, Left e -> log ("  ERROR parsing port " <> modName <> ": " <> e)
    Right refJson, Right portJson ->
      let refObj  = FO.delete "modulePath" <$> Json.toObject refJson
          portObj = FO.delete "modulePath" <$> Json.toObject portJson
      in if refObj == portObj
         then log ("  MATCH " <> modName)
         else log ("  DIFF  " <> modName)
