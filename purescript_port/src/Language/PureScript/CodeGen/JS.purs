-- | JavaScript code generation from CoreFn.
module Language.PureScript.CodeGen.JS
  ( module Language.PureScript.CoreImp.AST
  , module Language.PureScript.CodeGen.JS.Common
  , moduleToJs
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Supply.Class (class MonadSupply, fresh)
import Control.Monad.Writer.Class (class MonadWriter)
import Data.Array ((:), mapMaybe)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (singleton) as SCU
import Data.Traversable (traverse, for)
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..), fst, snd)

import Language.PureScript.AST.SourcePos (SourceSpan, displayStartEndPos)
import Language.PureScript.CodeGen.JS.Common
import Language.PureScript.Comments (Comment(..))
import Language.PureScript.Constants.Prim as C
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Binders (Binder(..), extractBinderAnn)
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr, Guard, extractAnn, modifyAnn)
import Language.PureScript.CoreFn.Expr as CF
import Language.PureScript.CoreFn.Meta (ConstructorType(..), Meta(..))
import Language.PureScript.CoreFn.Module (Module(..)) as CoreFn
import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.AST as CIAST
import Language.PureScript.CoreImp.Module (Export(..), Import(..), Module(..)) as JS
import Language.PureScript.CoreImp.Optimizer (optimize)
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors (MultipleErrors(..), ErrorMessage(..), SimpleErrorMessage(..), addHint, errorMessage, errorMessage', internalCompilerError, rethrow, rethrowWithPosition)
import Language.PureScript.AST.Declarations (ErrorMessageHint(..))
import Language.PureScript.Names (Ident(..), ModuleName(..), ProperName(..), Qualified(..), QualifiedBy(..), runIdent, runModuleName, showQualified, showIdent)
import Language.PureScript.PSString (PSString, mkString)
import Language.PureScript.AST.Literals (Literal)
import Language.PureScript.AST.Literals as Lit

-- | Generate JavaScript module from CoreFn module.
moduleToJs
  :: forall m
   . MonadError MultipleErrors m
  => MonadSupply m
  => MonadWriter MultipleErrors m
  => CoreFn.Module Ann
  -> Maybe String
  -> m (Tuple (JS.Module) (Array String))
moduleToJs (CoreFn.Module m) foreignInclude =
  rethrow (addHint (ErrorInModule m.moduleName)) $ do
    let mn = m.moduleName
    let coms = m.moduleComments
    let imps = m.moduleImports
    let exps = m.moduleExports
    let reExps = m.moduleReExports
    let foreigns = m.moduleForeign
    let decls = m.moduleDecls
    let usedNames = Array.concatMap getNames decls
    let imps' = Array.nub (map snd imps)
    let mnLookup = renameImports mn usedNames imps'
    -- Generate JS for all declarations; for now laziness is not needed (stub returns false)
    jsDecls <- traverse (moduleBindToJs mn) decls
    optimized <- map (map (map annotatePure)) $ for jsDecls \group ->
      optimize group
    traverse_ (traverse_ (checkIntegers mn)) optimized
    let header = coms
    let foreign' = case if Array.null foreigns then Nothing else foreignInclude of
          Nothing -> []
          Just path -> [ JS.Import ffiNamespace (mkString path) ]
    let moduleBody = Array.concat optimized
    let Tuple usedModuleNames renamedModuleBody = traverseArr (replaceModuleAccessors mn mnLookup) moduleBody
    let reExpKeys = Set.fromFoldable (Map.keys reExps)
    let allUsed = Set.union reExpKeys usedModuleNames
    let primSet = Set.fromFoldable C.primModules
    let jsImports =
          map (importToJs mnLookup)
          $ Array.filter (\mn' -> Set.member mn' allUsed)
          $ Array.filter (\mn' -> not (mn' == mn || Set.member mn' primSet))
          imps'
    let foreignExps = Array.intersect exps foreigns
    let standardExps = Array.filter (\i -> not (Array.elem i foreignExps)) exps
    let reExps' = Map.toUnfoldable (Map.filterKeys (\k -> not (Set.member k primSet)) reExps)
          :: Array (Tuple ModuleName (Array Ident))
    let jsExports
          = (mapMaybe (exportsToJs (map mkString foreignInclude)) [ foreignExps ])
          <> (mapMaybe (exportsToJs Nothing) [ standardExps ])
          <> mapMaybe reExportsToJs reExps'
    let jsModule = JS.Module
          { modHeader: header
          , modImports: foreign' <> jsImports
          , modBody: renamedModuleBody
          , modExports: jsExports
          }
    pure $ Tuple jsModule []
  where
  -- Extract bound names from a binding group.
  getNames :: Bind Ann -> Array Ident
  getNames (NonRec _ ident _) = [ ident ]
  getNames (Rec vals) = map (\(Tuple (Tuple _ ident) _) -> ident) vals

  -- Create alternative names for each module to avoid collisions with declaration names.
  renameImports :: ModuleName -> Array Ident -> Array ModuleName -> Map ModuleName String
  renameImports mn usedNames = go Map.empty usedNames
    where
    go :: Map ModuleName String -> Array Ident -> Array ModuleName -> Map ModuleName String
    go acc used mns = case Array.uncons mns of
      Nothing -> acc
      Just { head: mn', tail: rest } ->
        let mnj = moduleNameToJs mn'
        in if mn' /= mn && Array.elem (Ident mnj) used
           then
             let newName = freshModuleName 1 mnj used
             in go (Map.insert mn' newName acc) (Ident newName : used) rest
           else
             go (Map.insert mn' mnj acc) used rest

    freshModuleName :: Int -> String -> Array Ident -> String
    freshModuleName i base used =
      let newName = base <> "_" <> show i
      in if Array.elem (Ident newName) used
         then freshModuleName (i + 1) base used
         else newName

  -- Generate import statement for a module.
  importToJs :: Map ModuleName String -> ModuleName -> JS.Import
  importToJs mnLookup mn' =
    let mnSafe = case Map.lookup mn' mnLookup of
                   Just s  -> s
                   Nothing -> internalError "Missing value in mnLookup"
    in JS.Import mnSafe (moduleImportPath mn')

  -- Generate export statement.
  exportsToJs :: Maybe PSString -> Array Ident -> Maybe JS.Export
  exportsToJs from idents =
    let names = map (identToJs) idents
    in case NEL.fromFoldable names of
         Nothing -> Nothing
         Just nel -> Just (JS.Export nel from)

  -- Generate re-export statement.
  reExportsToJs :: Tuple ModuleName (Array Ident) -> Maybe JS.Export
  reExportsToJs (Tuple mn' idents) =
    exportsToJs (Just (moduleImportPath mn')) idents

  -- Compute the path to a module's index.js.
  moduleImportPath :: ModuleName -> PSString
  moduleImportPath mn' = mkString (".." <> "/" <> runModuleName mn' <> "/index.js")

  -- Replace ModuleAccessor nodes with Indexer nodes, tracking which modules were used.
  replaceModuleAccessors :: ModuleName -> Map ModuleName String -> AST -> Tuple (Set ModuleName) AST
  replaceModuleAccessors _mn mnLookup = go
    where
    go :: AST -> Tuple (Set ModuleName) AST
    go (ModuleAccessor ss mn' name) =
      let mnSafe = case Map.lookup mn' mnLookup of
                     Just s  -> s
                     Nothing -> internalError "Missing value in mnLookup"
      in Tuple (Set.singleton mn') (Indexer ss (StringLiteral ss name) (Var ss mnSafe))
    go (Unary ss op j) =
      let Tuple s j' = go j
      in Tuple s (Unary ss op j')
    go (Binary ss op j1 j2) =
      let Tuple s1 j1' = go j1
          Tuple s2 j2' = go j2
      in Tuple (Set.union s1 s2) (Binary ss op j1' j2')
    go (ArrayLiteral ss js) =
      let results = map go js
      in Tuple (foldlSet results) (ArrayLiteral ss (map snd results))
    go (Indexer ss j1 j2) =
      let Tuple s1 j1' = go j1
          Tuple s2 j2' = go j2
      in Tuple (Set.union s1 s2) (Indexer ss j1' j2')
    go (ObjectLiteral ss kvs) =
      let results = map (\(Tuple k v) -> let Tuple s v' = go v in Tuple s (Tuple k v')) kvs
      in Tuple (foldlSet results) (ObjectLiteral ss (map snd results))
    go (Function ss name args body) =
      let Tuple s body' = go body
      in Tuple s (Function ss name args body')
    go (App ss f args) =
      let Tuple sf f' = go f
          results = map go args
      in Tuple (Set.union sf (foldlSet results)) (App ss f' (map snd results))
    go (Block ss js) =
      let results = map go js
      in Tuple (foldlSet results) (Block ss (map snd results))
    go (VariableIntroduction ss name (Just (Tuple ie j))) =
      let Tuple s j' = go j
      in Tuple s (VariableIntroduction ss name (Just (Tuple ie j')))
    go (Assignment ss j1 j2) =
      let Tuple s1 j1' = go j1
          Tuple s2 j2' = go j2
      in Tuple (Set.union s1 s2) (Assignment ss j1' j2')
    go (While ss j1 j2) =
      let Tuple s1 j1' = go j1
          Tuple s2 j2' = go j2
      in Tuple (Set.union s1 s2) (While ss j1' j2')
    go (For ss name j1 j2 j3) =
      let Tuple s1 j1' = go j1
          Tuple s2 j2' = go j2
          Tuple s3 j3' = go j3
      in Tuple (Set.union s1 (Set.union s2 s3)) (For ss name j1' j2' j3')
    go (ForIn ss name j1 j2) =
      let Tuple s1 j1' = go j1
          Tuple s2 j2' = go j2
      in Tuple (Set.union s1 s2) (ForIn ss name j1' j2')
    go (IfElse ss cond then_ melse) =
      let Tuple sc cond' = go cond
          Tuple st then_' = go then_
          Tuple se melse' = case melse of
            Nothing -> Tuple Set.empty Nothing
            Just e -> let Tuple s e' = go e in Tuple s (Just e')
      in Tuple (Set.union sc (Set.union st se)) (IfElse ss cond' then_' melse')
    go (Return ss j) =
      let Tuple s j' = go j in Tuple s (Return ss j')
    go (Throw ss j) =
      let Tuple s j' = go j in Tuple s (Throw ss j')
    go (InstanceOf ss j1 j2) =
      let Tuple s1 j1' = go j1
          Tuple s2 j2' = go j2
      in Tuple (Set.union s1 s2) (InstanceOf ss j1' j2')
    go (Comment com j) =
      let Tuple s j' = go j in Tuple s (Comment com j')
    go other = Tuple Set.empty other

  foldlSet :: forall a. Array (Tuple (Set ModuleName) a) -> Set ModuleName
  foldlSet = Array.foldl (\acc (Tuple s _) -> Set.union acc s) Set.empty

  -- Helper: traverse array collecting sets
  traverseArr :: (AST -> Tuple (Set ModuleName) AST) -> Array AST -> Tuple (Set ModuleName) (Array AST)
  traverseArr f xs =
    let results = map f xs
    in Tuple (foldlSet results) (map snd results)

  -- Check that integer literals are within JavaScript's safe range.
  checkIntegers :: ModuleName -> AST -> m Unit
  checkIntegers mn = void <<< everywhereTopDownM go
    where
    go :: AST -> m AST
    go (Unary _ Negate (NumericLiteral ss (Left i))) =
      -- Move negation inside literal to avoid false overflow errors
      pure $ NumericLiteral ss (Left (-i))
    go js@(NumericLiteral ss (Left i)) =
      let minInt = -2147483648
          maxInt = 2147483647
      in if i < minInt || i > maxInt
         then throwError $ case ss of
                Nothing -> errorMessage (IntOutOfRange i "JavaScript" minInt maxInt)
                Just s  -> errorMessage' s (IntOutOfRange i "JavaScript" minInt maxInt)
         else pure js
    go other = pure other

  -- The $runtime_lazy helper variable.
  runtimeLazy :: AST
  runtimeLazy =
    VariableIntroduction Nothing "$runtime_lazy" $ Just $ Tuple UnknownEffects $
      Function Nothing Nothing ["name", "moduleName", "init"] $ Block Nothing
        [ VariableIntroduction Nothing "state" $ Just $ Tuple UnknownEffects $ NumericLiteral Nothing (Left 0)
        , VariableIntroduction Nothing "val" Nothing
        , Return Nothing $ Function Nothing Nothing ["lineNumber"] $ Block Nothing
          [ IfElse Nothing
              (Binary Nothing EqualTo (Var Nothing "state") (NumericLiteral Nothing (Left 2)))
              (Return Nothing (Var Nothing "val"))
              Nothing
          , IfElse Nothing
              (Binary Nothing EqualTo (Var Nothing "state") (NumericLiteral Nothing (Left 1)))
              (Throw Nothing $ Unary Nothing New $ App Nothing (Var Nothing "ReferenceError")
                [ Binary Nothing Add (Var Nothing "name")
                    (Binary Nothing Add
                      (StringLiteral Nothing (mkString " was needed before it finished initializing (module "))
                      (Binary Nothing Add (Var Nothing "moduleName")
                        (Binary Nothing Add
                          (StringLiteral Nothing (mkString ", line "))
                          (Binary Nothing Add (Var Nothing "lineNumber")
                            (StringLiteral Nothing (mkString ")"))))))
                , Var Nothing "moduleName"
                , Var Nothing "lineNumber"
                ])
              Nothing
          , Assignment Nothing (Var Nothing "state") (NumericLiteral Nothing (Left 1))
          , Assignment Nothing (Var Nothing "val") (App Nothing (Var Nothing "init") [])
          , Assignment Nothing (Var Nothing "state") (NumericLiteral Nothing (Left 2))
          , Return Nothing (Var Nothing "val")
          ]
        ]

  -- Adds purity annotations to top-level values for bundlers.
  annotatePure :: AST -> AST
  annotatePure = annotateOrWrap
    where
    annotateOrWrap ast = fromMaybe (pureIife ast) (maybePureGen false ast)

    -- maybePureGen alreadyAnnotated: if true, App nodes are not wrapped with pure annotation
    maybePureGen :: Boolean -> AST -> Maybe AST
    maybePureGen alreadyAnnotated ast = case ast of
      VariableIntroduction ss name j ->
        Just (VariableIntroduction ss name (map (\(Tuple ie a) -> Tuple ie (annotateOrWrap a)) j))
      App ss f args ->
        let appFn = if alreadyAnnotated then App else pureApp
        in appFn ss <$> maybePureGen true f <*> traverse (maybePureGen false) args
      ArrayLiteral ss jss -> ArrayLiteral ss <$> traverse (maybePureGen false) jss
      ObjectLiteral ss props -> ObjectLiteral ss <$> traverse (\(Tuple k v) -> Tuple k <$> maybePureGen false v) props
      Comment c j -> Comment c <$> maybePureGen false j
      Indexer _ _ (Var _ ns) | ns == ffiNamespace -> Just ast
      NumericLiteral _ _ -> Just ast
      StringLiteral _ _ -> Just ast
      BooleanLiteral _ _ -> Just ast
      Function _ _ _ _ -> Just ast
      Var _ _ -> Just ast
      ModuleAccessor _ _ _ -> Just ast
      _ -> Nothing

    pureIife :: AST -> AST
    pureIife val = pureApp Nothing (Function Nothing Nothing [] (Block Nothing [Return Nothing val])) []

    pureApp :: Maybe SourceSpan -> AST -> Array AST -> AST
    pureApp ss f args = Comment PureAnnotation (App ss f args)

-- | Generate code for a binding group.
moduleBindToJs
  :: forall m
   . MonadError MultipleErrors m
  => MonadSupply m
  => MonadWriter MultipleErrors m
  => ModuleName
  -> Bind Ann
  -> m (Array AST)
moduleBindToJs mn = bindToJs
  where
  bindToJs :: Bind Ann -> m (Array AST)
  bindToJs (NonRec (Tuple _ (Tuple _ (Just IsTypeClassConstructor))) _ _) = pure []
  bindToJs (NonRec ann ident val) = map Array.singleton $ nonRecToJS ann ident val
  bindToJs (Rec vals) = do
    -- Apply laziness transform (stub: identity)
    let transformed = vals
    traverse (\(Tuple (Tuple ann ident) val) -> nonRecToJS ann ident val) transformed

  nonRecToJS :: Ann -> Ident -> Expr Ann -> m AST
  nonRecToJS ann ident expr =
    let Tuple ss (Tuple com _) = ann
        hasComs = not (Array.null com)
    in if hasComs
       then do
         withoutComment <- nonRecToJS ann ident (modifyAnn removeComments expr)
         pure (Comment (SourceComments com) withoutComment)
       else do
         js <- valueToJs expr
         pure (withSourceSpan ss (VariableIntroduction Nothing (identToJs ident) (Just (Tuple (guessEffects expr) js))))

  removeComments :: Ann -> Ann
  removeComments (Tuple ss (Tuple _ meta)) = Tuple ss (Tuple [] meta)

  guessEffects :: Expr Ann -> InitializerEffects
  guessEffects = case _ of
    CF.Var _ (Qualified (BySourcePos _) _) -> NoEffects
    CF.App (Tuple _ (Tuple _ (Just IsSynthetic))) _ _ -> NoEffects
    _ -> UnknownEffects

  var :: Ident -> AST
  var = CIAST.Var Nothing <<< identToJs

  valueToJs :: Expr Ann -> m AST
  valueToJs e =
    let Tuple ss _ = extractAnn e
    in rethrowWithPosition ss $ valueToJs' e

  valueToJs' :: Expr Ann -> m AST
  valueToJs' (CF.Literal (Tuple ss _) lit) =
    rethrowWithPosition ss $ literalToValueJS ss lit
  valueToJs' (CF.Var (Tuple _ (Tuple _ (Just (IsConstructor _ [])))) name) =
    pure $ accessorString (mkString "value") (qualifiedToJS identity name)
  valueToJs' (CF.Var (Tuple _ (Tuple _ (Just (IsConstructor _ _)))) name) =
    pure $ accessorString (mkString "create") (qualifiedToJS identity name)
  valueToJs' (CF.Accessor _ prop val) =
    accessorString prop <$> valueToJs val
  valueToJs' (CF.ObjectUpdate (Tuple ss _) o copy ps) = do
    obj <- valueToJs o
    sts <- traverse (\(Tuple k v) -> map (Tuple k) (valueToJs v)) ps
    case copy of
      Nothing -> extendObj obj sts
      Just names ->
        let f name = Tuple name (accessorString name obj)
        in pure $ ObjectLiteral (Just ss) (map f names <> sts)
  valueToJs' (CF.Abs _ arg val) = do
    ret <- valueToJs val
    let jsArg = case arg of
          UnusedIdent -> []
          _ -> [ identToJs arg ]
    pure $ Function Nothing Nothing jsArg (Block Nothing [Return Nothing ret])
  valueToJs' e@(CF.App _ _ _) = do
    let Tuple f args = unApp e []
    args' <- traverse valueToJs args
    case f of
      CF.Var (Tuple _ (Tuple _ (Just IsNewtype))) _ ->
        case Array.head args' of
          Nothing -> internalCompilerError "Newtype constructor without arguments"
          Just h -> pure h
      CF.Var (Tuple _ (Tuple _ (Just (IsConstructor _ fields)))) name
        | Array.length args == Array.length fields ->
          pure $ Unary Nothing New (App Nothing (qualifiedToJS identity name) args')
      _ -> do
        fJs <- valueToJs f
        pure $ Array.foldl (\fn a -> App Nothing fn [a]) fJs args'
  valueToJs' (CF.Var (Tuple _ (Tuple _ (Just IsForeign))) qi@(Qualified (ByModuleName mn') ident)) =
    pure $ if mn' == mn
           then foreignIdent ident
           else varToJs qi
  valueToJs' (CF.Var (Tuple _ (Tuple _ (Just IsForeign))) ident) =
    internalCompilerError $ "Encountered an unqualified reference to a foreign ident " <> showQualified showIdent ident
  valueToJs' (CF.Var _ ident) = pure $ varToJs ident
  valueToJs' (CF.Case (Tuple ss _) values binders) = do
    vals <- traverse valueToJs values
    bindersToJs ss binders vals
  valueToJs' (CF.Let _ ds val) = do
    ds' <- Array.concat <$> traverse bindToJs ds
    ret <- valueToJs val
    pure $ App Nothing (Function Nothing Nothing [] (Block Nothing (ds' <> [Return Nothing ret]))) []
  valueToJs' (CF.Constructor (Tuple _ (Tuple _ (Just IsNewtype))) _ ctor _) =
    pure $ VariableIntroduction Nothing (properToJs ctor) $ Just $ Tuple UnknownEffects $
      ObjectLiteral Nothing
        [ Tuple (mkString "create")
            (Function Nothing Nothing ["value"] (Block Nothing [Return Nothing (Var Nothing "value")]))
        ]
  valueToJs' (CF.Constructor _ _ ctor []) =
    pure $ iife (properToJs ctor)
      [ Function Nothing (Just (properToJs ctor)) [] (Block Nothing [])
      , Assignment Nothing (accessorString (mkString "value") (Var Nothing (properToJs ctor)))
          (Unary Nothing New (App Nothing (Var Nothing (properToJs ctor)) []))
      ]
  valueToJs' (CF.Constructor _ _ ctor fields) =
    let constructor =
          let body = map (\f -> Assignment Nothing (accessorString (mkString (identToJs f)) (Var Nothing "this")) (var f)) fields
          in Function Nothing (Just (properToJs ctor)) (map identToJs fields) (Block Nothing body)
        createFn =
          let body = Unary Nothing New (App Nothing (Var Nothing (properToJs ctor)) (map var fields))
          in Array.foldr (\f inner -> Function Nothing Nothing [identToJs f] (Block Nothing [Return Nothing inner])) body fields
    in pure $ iife (properToJs ctor) [ constructor, Assignment Nothing (accessorString (mkString "create") (Var Nothing (properToJs ctor))) createFn ]

  iife :: String -> Array AST -> AST
  iife v exprs = App Nothing (Function Nothing Nothing [] (Block Nothing (exprs <> [Return Nothing (Var Nothing v)]))) []

  unApp :: Expr Ann -> Array (Expr Ann) -> Tuple (Expr Ann) (Array (Expr Ann))
  unApp (CF.App _ f arg) args = unApp f (Array.cons arg args)
  unApp other args = Tuple other args

  literalToValueJS :: SourceSpan -> Literal (Expr Ann) -> m AST
  literalToValueJS ss (Lit.NumericLiteral (Left i)) = pure $ NumericLiteral (Just ss) (Left i)
  literalToValueJS ss (Lit.NumericLiteral (Right n)) = pure $ NumericLiteral (Just ss) (Right n)
  literalToValueJS ss (Lit.StringLiteral s) = pure $ StringLiteral (Just ss) s
  literalToValueJS ss (Lit.CharLiteral c) = pure $ StringLiteral (Just ss) (mkString (SCU.singleton c))
  literalToValueJS ss (Lit.BooleanLiteral b) = pure $ BooleanLiteral (Just ss) b
  literalToValueJS ss (Lit.ArrayLiteral xs) = ArrayLiteral (Just ss) <$> traverse valueToJs xs
  literalToValueJS ss (Lit.ObjectLiteral ps) = ObjectLiteral (Just ss) <$> traverse (\(Tuple k v) -> map (Tuple k) (valueToJs v)) ps

  extendObj :: AST -> Array (Tuple PSString AST) -> m AST
  extendObj obj sts = do
    newObj <- freshName
    key <- freshName
    evaluatedObj <- freshName
    let jsKey = Var Nothing key
        jsNewObj = Var Nothing newObj
        jsEvaluatedObj = Var Nothing evaluatedObj
        evaluate = VariableIntroduction Nothing evaluatedObj (Just (Tuple UnknownEffects obj))
        objAssign = VariableIntroduction Nothing newObj (Just (Tuple NoEffects (ObjectLiteral Nothing [])))
        cond = App Nothing
                 (accessorString (mkString "call")
                   (accessorString (mkString "hasOwnProperty") (ObjectLiteral Nothing [])))
                 [jsEvaluatedObj, jsKey]
        assign = Block Nothing [Assignment Nothing (Indexer Nothing jsKey jsNewObj) (Indexer Nothing jsKey jsEvaluatedObj)]
        copy = ForIn Nothing key jsEvaluatedObj (Block Nothing [IfElse Nothing cond assign Nothing])
        stToAssign (Tuple s js) = Assignment Nothing (accessorString s jsNewObj) js
        extend = map stToAssign sts
        block = Block Nothing ([ evaluate, objAssign, copy ] <> extend <> [Return Nothing jsNewObj])
    pure $ App Nothing (Function Nothing Nothing [] block) []

  varToJs :: Qualified Ident -> AST
  varToJs (Qualified (BySourcePos _) ident) = var ident
  varToJs qual = qualifiedToJS identity qual

  qualifiedToJS :: forall a. (a -> Ident) -> Qualified a -> AST
  qualifiedToJS f (Qualified (ByModuleName mn') a)
    | mn' == ModuleName "Prim" = CIAST.Var Nothing (runIdent (f a))
  qualifiedToJS f (Qualified (ByModuleName mn') a)
    | mn /= mn' = ModuleAccessor Nothing mn' (mkString (anyNameToJs (runIdent (f a))))
  qualifiedToJS f (Qualified _ a) = CIAST.Var Nothing (identToJs (f a))

  foreignIdent :: Ident -> AST
  foreignIdent ident = accessorString (mkString (runIdent ident)) (CIAST.Var Nothing ffiNamespace)

  bindersToJs :: SourceSpan -> Array (CaseAlternative Ann) -> Array AST -> m AST
  bindersToJs ss binders vals = do
    valNames <- replicateM (Array.length vals) freshName
    let assignments = Array.zipWith (\name v -> VariableIntroduction Nothing name (Just (Tuple UnknownEffects v))) valNames vals
    jss <- for binders \(CaseAlternative ca) -> do
      ret <- guardsToJs ca.caseAlternativeResult
      go valNames ret ca.caseAlternativeBinders
    pure $ App Nothing (Function Nothing Nothing [] (Block Nothing (assignments <> Array.concat jss <> [Throw Nothing (failedPatternError valNames)]))) []
    where
    go :: Array String -> Array AST -> Array (Binder Ann) -> m (Array AST)
    go _ done [] = pure done
    go valNames done' binders' =
      case Array.uncons valNames, Array.uncons binders' of
        Just { head: v, tail: vs }, Just { head: b, tail: bs } -> do
          done'' <- go vs done' bs
          binderToJs v done'' b
        _, _ -> internalCompilerError "Invalid arguments to bindersToJs"

    failedPatternError :: Array String -> AST
    failedPatternError names =
      Unary Nothing New $ App Nothing (Var Nothing "Error")
        [ Binary Nothing Add
            (StringLiteral Nothing (mkString (failedPatternMessage)))
            (ArrayLiteral Nothing (Array.zipWith valueError names vals))
        ]

    failedPatternMessage :: String
    failedPatternMessage = "Failed pattern match at " <> runModuleName mn <> " " <> displayStartEndPos ss <> ": "

    valueError :: String -> AST -> AST
    valueError _ l@(NumericLiteral _ _) = l
    valueError _ l@(StringLiteral _ _) = l
    valueError _ l@(BooleanLiteral _ _) = l
    valueError s _ = accessorString (mkString "name") (accessorString (mkString "constructor") (Var Nothing s))

    guardsToJs :: Either (Array (Tuple (Guard Ann) (Expr Ann))) (Expr Ann) -> m (Array AST)
    guardsToJs (Left gs) = traverse genGuard gs
      where
      genGuard (Tuple cond val) = do
        cond' <- valueToJs cond
        val' <- valueToJs val
        pure $ IfElse Nothing cond' (Block Nothing [Return Nothing val']) Nothing
    guardsToJs (Right v) = map (\x -> [Return Nothing x]) (valueToJs v)

  binderToJs :: String -> Array AST -> Binder Ann -> m (Array AST)
  binderToJs s done binder =
    let ann = extractBinderAnn binder
        Tuple ss _ = ann
    in rethrowWithPosition ss $ binderToJs' s done binder

  binderToJs' :: String -> Array AST -> Binder Ann -> m (Array AST)
  binderToJs' _ done (NullBinder _) = pure done
  binderToJs' varName done (LiteralBinder _ lit) = literalToBinderJS varName done lit
  binderToJs' varName done (VarBinder _ ident) =
    pure (VariableIntroduction Nothing (identToJs ident) (Just (Tuple NoEffects (Var Nothing varName))) : done)
  binderToJs' varName done (ConstructorBinder (Tuple _ (Tuple _ (Just IsNewtype))) _ _ [b]) =
    binderToJs varName done b
  binderToJs' varName done (ConstructorBinder (Tuple _ (Tuple _ (Just (IsConstructor ctorType fields)))) _ ctor bs) = do
    js <- go (Array.zip fields bs) done
    pure $ case ctorType of
      ProductType -> js
      SumType ->
        [ IfElse Nothing
            (InstanceOf Nothing (Var Nothing varName) (qualifiedToJS (\(ProperName n) -> Ident n) ctor))
            (Block Nothing js)
            Nothing
        ]
    where
    go :: Array (Tuple Ident (Binder Ann)) -> Array AST -> m (Array AST)
    go pairs done' = case Array.uncons pairs of
      Nothing -> pure done'
      Just { head: Tuple field binder, tail: rest } -> do
        argVar <- freshName
        done'' <- go rest done'
        js <- binderToJs argVar done'' binder
        pure (VariableIntroduction Nothing argVar (Just (Tuple UnknownEffects (accessorString (mkString (identToJs field)) (Var Nothing varName)))) : js)
  binderToJs' _ _ (ConstructorBinder _ _ _ _) =
    internalCompilerError "binderToJs: Invalid ConstructorBinder"
  binderToJs' varName done (NamedBinder _ ident binder) = do
    js <- binderToJs varName done binder
    pure (VariableIntroduction Nothing (identToJs ident) (Just (Tuple NoEffects (Var Nothing varName))) : js)

  literalToBinderJS :: String -> Array AST -> Literal (Binder Ann) -> m (Array AST)
  literalToBinderJS varName done (Lit.NumericLiteral num) =
    pure [IfElse Nothing (Binary Nothing EqualTo (Var Nothing varName) (NumericLiteral Nothing num)) (Block Nothing done) Nothing]
  literalToBinderJS varName done (Lit.CharLiteral c) =
    pure [IfElse Nothing (Binary Nothing EqualTo (Var Nothing varName) (StringLiteral Nothing (mkString (SCU.singleton c)))) (Block Nothing done) Nothing]
  literalToBinderJS varName done (Lit.StringLiteral str) =
    pure [IfElse Nothing (Binary Nothing EqualTo (Var Nothing varName) (StringLiteral Nothing str)) (Block Nothing done) Nothing]
  literalToBinderJS varName done (Lit.BooleanLiteral true) =
    pure [IfElse Nothing (Var Nothing varName) (Block Nothing done) Nothing]
  literalToBinderJS varName done (Lit.BooleanLiteral false) =
    pure [IfElse Nothing (Unary Nothing Not (Var Nothing varName)) (Block Nothing done) Nothing]
  literalToBinderJS varName done (Lit.ObjectLiteral bs) = go done bs
    where
    go :: Array AST -> Array (Tuple PSString (Binder Ann)) -> m (Array AST)
    go done' [] = pure done'
    go done' binders = case Array.uncons binders of
      Nothing -> pure done'
      Just { head: Tuple prop binder, tail: rest } -> do
        propVar <- freshName
        done'' <- go done' rest
        js <- binderToJs propVar done'' binder
        pure (VariableIntroduction Nothing propVar (Just (Tuple UnknownEffects (accessorString prop (Var Nothing varName)))) : js)
  literalToBinderJS varName done (Lit.ArrayLiteral bs) = do
    js <- goArr done 0 bs
    pure [IfElse Nothing (Binary Nothing EqualTo (accessorString (mkString "length") (Var Nothing varName)) (NumericLiteral Nothing (Left (Array.length bs)))) (Block Nothing js) Nothing]
    where
    goArr :: Array AST -> Int -> Array (Binder Ann) -> m (Array AST)
    goArr done' _ [] = pure done'
    goArr done' index binders = case Array.uncons binders of
      Nothing -> pure done'
      Just { head: binder, tail: rest } -> do
        elVar <- freshName
        done'' <- goArr done' (index + 1) rest
        js <- binderToJs elVar done'' binder
        pure (VariableIntroduction Nothing elVar (Just (Tuple UnknownEffects (Indexer Nothing (NumericLiteral Nothing (Left index)) (Var Nothing varName)))) : js)

-- | Create an Indexer to access a property by string key.
accessorString :: PSString -> AST -> AST
accessorString prop = Indexer Nothing (StringLiteral Nothing prop)

-- | The FFI namespace variable name.
ffiNamespace :: String
ffiNamespace = "$foreign"

-- | Generate a fresh variable name.
freshName :: forall m. MonadSupply m => m String
freshName = map (\n -> "$" <> show n) fresh

-- | Replicate a monadic action n times.
replicateM :: forall m a. Monad m => Int -> m a -> m (Array a)
replicateM n m
  | n <= 0 = pure []
  | otherwise = do
      x <- m
      xs <- replicateM (n - 1) m
      pure (x : xs)

