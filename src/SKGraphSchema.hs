{-# LANGUAGE OverloadedStrings #-}

module SKGraphSchema where

import Autodocodec
import Data.Text

data SModelElement = SModelElement
  { sType :: !Text,
    sId :: !Text,
    sChildren :: [SModelElement]
  }

instance HasCodec SModelElement where
  codec =
    object "SModelElement" $
      SModelElement
        <$> requiredField "type" "element type" .= sType
        <*> requiredField "id" "element id" .= sId
        <*> requiredField "children" "element children" .= sChildren
