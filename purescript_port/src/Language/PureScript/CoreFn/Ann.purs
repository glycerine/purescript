-- | Annotation type for core functional representation.
module Language.PureScript.CoreFn.Ann
  ( Ann
  , ssAnn
  , extractAnn
  ) where

import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Meta (Meta)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | Type alias for basic annotations: (SourceSpan, comments, optional Meta)
type Ann = Tuple SourceSpan (Tuple (Array Comment) (Maybe Meta))

-- | An annotation empty of metadata aside from a source span.
ssAnn :: SourceSpan -> Ann
ssAnn ss = Tuple ss (Tuple [] Nothing)

-- | Extract the SourceSpan from an annotation.
extractAnn :: Ann -> SourceSpan
extractAnn (Tuple ss _) = ss
