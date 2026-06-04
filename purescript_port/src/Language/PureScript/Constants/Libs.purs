module Language.PureScript.Constants.Libs
  ( sApply
  , sBind
  , sDiscard
  , sDiscardUnit
  , sMap
  , sPure
  , sAppend
  , sMempty
  , sNegate
  , sSub
  , sAdd
  , sMul
  , sDiv
  , sZero
  , sOne
  , sConj
  , sDisj
  , sNot
  , sEq
  , sCompare
  , sUndefined
  , sMkFn
  , sRunFn
  , sMkEffectFn
  , sRunEffectFn
  , sMkSTFn
  , sRunSTFn
  , mControlApply
  , mControlApplicative
  , mControlBind
  , mControlCategory
  , mControlSemigroupoid
  , mDataBounded
  , mDataEq
  , mDataEuclideanRing
  , mDataFunction
  , mDataFunctor
  , mDataMonoid
  , mDataOrd
  , mDataOrdering
  , mDataRing
  , mDataSemigroup
  , mDataSemiring
  , mDataSymbol
  , mDataArray
  , mDataBifunctor
  , mDataFunctorContravariant
  , mEffectUncurried
  , mDataFunctionUncurried
  , mPartialUnsafe
  , mDataProfunctor
  , mControlMonadSTInternal
  , mControlMonadSTUncurried
  , mEffect
  , mControlMonadEff
  , EffectDictionaries(..)
  , effDictionaries
  , effectDictionaries
  , stDictionaries
  , stRefValue
  ) where

import Prelude

import Language.PureScript.Names (ModuleName(..))
import Language.PureScript.PSString (PSString, mkString)

-- String constants (S_ prefix in Haskell)
sApply :: String
sApply = "apply"

sBind :: String
sBind = "bind"

sDiscard :: String
sDiscard = "discard"

sDiscardUnit :: String
sDiscardUnit = "discardUnit"

sMap :: String
sMap = "map"

sPure :: String
sPure = "pure"

sAppend :: String
sAppend = "append"

sMempty :: String
sMempty = "mempty"

sNegate :: String
sNegate = "negate"

sSub :: String
sSub = "sub"

sAdd :: String
sAdd = "add"

sMul :: String
sMul = "mul"

sDiv :: String
sDiv = "div"

sZero :: String
sZero = "zero"

sOne :: String
sOne = "one"

sConj :: String
sConj = "conj"

sDisj :: String
sDisj = "disj"

sNot :: String
sNot = "not"

sEq :: String
sEq = "eq"

sCompare :: String
sCompare = "compare"

sUndefined :: String
sUndefined = "undefined"

sMkFn :: String
sMkFn = "mkFn"

sRunFn :: String
sRunFn = "runFn"

sMkEffectFn :: String
sMkEffectFn = "mkEffectFn"

sRunEffectFn :: String
sRunEffectFn = "runEffectFn"

sMkSTFn :: String
sMkSTFn = "mkSTFn"

sRunSTFn :: String
sRunSTFn = "runSTFn"

-- Module names (M_ prefix in Haskell)
mControlApply :: ModuleName
mControlApply = ModuleName "Control.Apply"

mControlApplicative :: ModuleName
mControlApplicative = ModuleName "Control.Applicative"

mControlBind :: ModuleName
mControlBind = ModuleName "Control.Bind"

mControlCategory :: ModuleName
mControlCategory = ModuleName "Control.Category"

mControlSemigroupoid :: ModuleName
mControlSemigroupoid = ModuleName "Control.Semigroupoid"

mDataBounded :: ModuleName
mDataBounded = ModuleName "Data.Bounded"

mDataEq :: ModuleName
mDataEq = ModuleName "Data.Eq"

mDataEuclideanRing :: ModuleName
mDataEuclideanRing = ModuleName "Data.EuclideanRing"

mDataFunction :: ModuleName
mDataFunction = ModuleName "Data.Function"

mDataFunctor :: ModuleName
mDataFunctor = ModuleName "Data.Functor"

mDataMonoid :: ModuleName
mDataMonoid = ModuleName "Data.Monoid"

mDataOrd :: ModuleName
mDataOrd = ModuleName "Data.Ord"

mDataOrdering :: ModuleName
mDataOrdering = ModuleName "Data.Ordering"

mDataRing :: ModuleName
mDataRing = ModuleName "Data.Ring"

mDataSemigroup :: ModuleName
mDataSemigroup = ModuleName "Data.Semigroup"

mDataSemiring :: ModuleName
mDataSemiring = ModuleName "Data.Semiring"

mDataSymbol :: ModuleName
mDataSymbol = ModuleName "Data.Symbol"

mDataArray :: ModuleName
mDataArray = ModuleName "Data.Array"

mDataBifunctor :: ModuleName
mDataBifunctor = ModuleName "Data.Bifunctor"

mDataFunctorContravariant :: ModuleName
mDataFunctorContravariant = ModuleName "Data.Functor.Contravariant"

mEffectUncurried :: ModuleName
mEffectUncurried = ModuleName "Effect.Uncurried"

mDataFunctionUncurried :: ModuleName
mDataFunctionUncurried = ModuleName "Data.Function.Uncurried"

mPartialUnsafe :: ModuleName
mPartialUnsafe = ModuleName "Partial.Unsafe"

mDataProfunctor :: ModuleName
mDataProfunctor = ModuleName "Data.Profunctor"

mControlMonadSTInternal :: ModuleName
mControlMonadSTInternal = ModuleName "Control.Monad.ST.Internal"

mControlMonadSTUncurried :: ModuleName
mControlMonadSTUncurried = ModuleName "Control.Monad.ST.Uncurried"

mEffect :: ModuleName
mEffect = ModuleName "Effect"

mControlMonadEff :: ModuleName
mControlMonadEff = ModuleName "Control.Monad.Eff"

stRefValue :: String
stRefValue = "value"

type EffectDictionaries =
  { edApplicativeDict :: PSString
  , edBindDict        :: PSString
  , edMonadDict       :: PSString
  , edWhile           :: PSString
  , edUntil           :: PSString
  }

effDictionaries :: EffectDictionaries
effDictionaries =
  { edApplicativeDict: mkString "applicativeEff"
  , edBindDict:        mkString "bindEff"
  , edMonadDict:       mkString "monadEff"
  , edWhile:           mkString "whileE"
  , edUntil:           mkString "untilE"
  }

effectDictionaries :: EffectDictionaries
effectDictionaries =
  { edApplicativeDict: mkString "applicativeEffect"
  , edBindDict:        mkString "bindEffect"
  , edMonadDict:       mkString "monadEffect"
  , edWhile:           mkString "whileE"
  , edUntil:           mkString "untilE"
  }

stDictionaries :: EffectDictionaries
stDictionaries =
  { edApplicativeDict: mkString "applicativeST"
  , edBindDict:        mkString "bindST"
  , edMonadDict:       mkString "monadST"
  , edWhile:           mkString "while"
  , edUntil:           mkString "until"
  }
