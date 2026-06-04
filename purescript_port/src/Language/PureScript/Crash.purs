module Language.PureScript.Crash (internalError) where

foreign import internalError :: forall a. String -> a
