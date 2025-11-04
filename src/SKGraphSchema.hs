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
    named "SModelElementCodec" $
      object "SModelElement" $
        SModelElement
          <$> requiredField "type" "element type" .= sType
          <*> requiredField "id" "element id" .= sId
          <*> optionalFieldWithDefault "children" [] "children of the element" .= sChildren
