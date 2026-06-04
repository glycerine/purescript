module Language.PureScript.Constants.Prim
  ( mPrim
  , mPrimBoolean
  , mPrimCoerce
  , mPrimOrdering
  , mPrimRow
  , mPrimRowList
  , mPrimSymbol
  , mPrimInt
  , mPrimTypeError
  , primModules
  , tyPartial
  , tyArray
  , tyBoolean
  , tyChar
  , tyConstraint
  , tyFunction
  , tyInt
  , tyNumber
  , tyRecord
  , tyRow
  , tyString
  , tySymbol
  , tyType
  , tyCoercible
  -- Prim.Boolean
  , tyTrue
  , tyFalse
  -- Prim.Ordering
  , tyTypeOrdering
  , tyLT
  , tyEQ
  , tyGT
  -- Prim.Row
  , clsRowUnion
  , clsRowNub
  , clsRowLacks
  , clsRowCons
  -- Prim.RowList
  , tyRowList
  , clsRowToList
  , tyRowListCons
  , tyRowListNil
  -- Prim.Symbol
  , clsSymbolAppend
  , clsSymbolCompare
  , clsSymbolCons
  -- Prim.Int
  , clsIntAdd
  , clsIntCompare
  , clsIntMul
  , clsIntToString
  -- Prim.TypeError
  , clsFail
  , clsWarn
  , tyDoc
  , tyAbove
  , tyBeside
  , tyQuote
  , tyQuoteLabel
  , tyText
  ) where

import Language.PureScript.Names
  ( ModuleName(..)
  , ProperName(..)
  , Qualified(..)
  , QualifiedBy(..)
  , ClassName
  , TypeName
  )

mPrim :: ModuleName
mPrim = ModuleName "Prim"

mPrimBoolean :: ModuleName
mPrimBoolean = ModuleName "Prim.Boolean"

mPrimCoerce :: ModuleName
mPrimCoerce = ModuleName "Prim.Coerce"

mPrimOrdering :: ModuleName
mPrimOrdering = ModuleName "Prim.Ordering"

mPrimRow :: ModuleName
mPrimRow = ModuleName "Prim.Row"

mPrimRowList :: ModuleName
mPrimRowList = ModuleName "Prim.RowList"

mPrimSymbol :: ModuleName
mPrimSymbol = ModuleName "Prim.Symbol"

mPrimInt :: ModuleName
mPrimInt = ModuleName "Prim.Int"

mPrimTypeError :: ModuleName
mPrimTypeError = ModuleName "Prim.TypeError"

primModules :: Array ModuleName
primModules =
  [ mPrim
  , mPrimBoolean
  , mPrimCoerce
  , mPrimOrdering
  , mPrimRow
  , mPrimRowList
  , mPrimSymbol
  , mPrimInt
  , mPrimTypeError
  ]

primQual :: forall a. ModuleName -> String -> Qualified (ProperName a)
primQual mn n = Qualified (ByModuleName mn) (ProperName n)

tyPartial :: Qualified (ProperName ClassName)
tyPartial = primQual mPrim "Partial"

tyArray :: Qualified (ProperName TypeName)
tyArray = primQual mPrim "Array"

tyBoolean :: Qualified (ProperName TypeName)
tyBoolean = primQual mPrim "Boolean"

tyChar :: Qualified (ProperName TypeName)
tyChar = primQual mPrim "Char"

tyConstraint :: Qualified (ProperName TypeName)
tyConstraint = primQual mPrim "Constraint"

tyFunction :: Qualified (ProperName TypeName)
tyFunction = primQual mPrim "Function"

tyInt :: Qualified (ProperName TypeName)
tyInt = primQual mPrim "Int"

tyNumber :: Qualified (ProperName TypeName)
tyNumber = primQual mPrim "Number"

tyRecord :: Qualified (ProperName TypeName)
tyRecord = primQual mPrim "Record"

tyRow :: Qualified (ProperName TypeName)
tyRow = primQual mPrim "Row"

tyString :: Qualified (ProperName TypeName)
tyString = primQual mPrim "String"

tySymbol :: Qualified (ProperName TypeName)
tySymbol = primQual mPrim "Symbol"

tyType :: Qualified (ProperName TypeName)
tyType = primQual mPrim "Type"

tyCoercible :: Qualified (ProperName ClassName)
tyCoercible = primQual mPrimCoerce "Coercible"

tyTrue :: Qualified (ProperName TypeName)
tyTrue = primQual mPrimBoolean "True"

tyFalse :: Qualified (ProperName TypeName)
tyFalse = primQual mPrimBoolean "False"

tyTypeOrdering :: Qualified (ProperName TypeName)
tyTypeOrdering = primQual mPrimOrdering "Ordering"

tyLT :: Qualified (ProperName TypeName)
tyLT = primQual mPrimOrdering "LT"

tyEQ :: Qualified (ProperName TypeName)
tyEQ = primQual mPrimOrdering "EQ"

tyGT :: Qualified (ProperName TypeName)
tyGT = primQual mPrimOrdering "GT"

clsRowUnion :: Qualified (ProperName ClassName)
clsRowUnion = primQual mPrimRow "Union"

clsRowNub :: Qualified (ProperName ClassName)
clsRowNub = primQual mPrimRow "Nub"

clsRowLacks :: Qualified (ProperName ClassName)
clsRowLacks = primQual mPrimRow "Lacks"

clsRowCons :: Qualified (ProperName ClassName)
clsRowCons = primQual mPrimRow "Cons"

tyRowList :: Qualified (ProperName TypeName)
tyRowList = primQual mPrimRowList "RowList"

clsRowToList :: Qualified (ProperName ClassName)
clsRowToList = primQual mPrimRowList "RowToList"

tyRowListCons :: Qualified (ProperName TypeName)
tyRowListCons = primQual mPrimRowList "Cons"

tyRowListNil :: Qualified (ProperName TypeName)
tyRowListNil = primQual mPrimRowList "Nil"

clsSymbolAppend :: Qualified (ProperName ClassName)
clsSymbolAppend = primQual mPrimSymbol "Append"

clsSymbolCompare :: Qualified (ProperName ClassName)
clsSymbolCompare = primQual mPrimSymbol "Compare"

clsSymbolCons :: Qualified (ProperName ClassName)
clsSymbolCons = primQual mPrimSymbol "Cons"

clsIntAdd :: Qualified (ProperName ClassName)
clsIntAdd = primQual mPrimInt "Add"

clsIntCompare :: Qualified (ProperName ClassName)
clsIntCompare = primQual mPrimInt "Compare"

clsIntMul :: Qualified (ProperName ClassName)
clsIntMul = primQual mPrimInt "Mul"

clsIntToString :: Qualified (ProperName ClassName)
clsIntToString = primQual mPrimInt "ToString"

clsFail :: Qualified (ProperName ClassName)
clsFail = primQual mPrimTypeError "Fail"

clsWarn :: Qualified (ProperName ClassName)
clsWarn = primQual mPrimTypeError "Warn"

tyDoc :: Qualified (ProperName TypeName)
tyDoc = primQual mPrimTypeError "Doc"

tyAbove :: Qualified (ProperName TypeName)
tyAbove = primQual mPrimTypeError "Above"

tyBeside :: Qualified (ProperName TypeName)
tyBeside = primQual mPrimTypeError "Beside"

tyQuote :: Qualified (ProperName TypeName)
tyQuote = primQual mPrimTypeError "Quote"

tyQuoteLabel :: Qualified (ProperName TypeName)
tyQuoteLabel = primQual mPrimTypeError "QuoteLabel"

tyText :: Qualified (ProperName TypeName)
tyText = primQual mPrimTypeError "Text"
