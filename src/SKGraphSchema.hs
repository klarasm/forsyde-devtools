{-# LANGUAGE OverloadedStrings #-}

module SKGraphSchema where

-- import Autodocodec

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Sequence as Seq
import qualified Data.Text as T

data GraphElement
  = KLabel
      { label :: !T.Text
      }
  | KNode
      { children :: [GraphElement],
        renderings :: [KRenderings]
      }

data KProperties
  = NodeLabelsPlacement [Int]
  | NodeSizeConstraints [Int]
  | NodeSizeMinimum [Int]

data KRenderings
  = KEllipse

instance A.ToJSON KRenderings where
  toJSON KEllipse =
    A.object
      [ "type" .= T.pack "KEllipseImpl",
        "children" .= (Seq.empty :: Seq.Seq A.Object),
        "actions" .= (Seq.empty :: Seq.Seq A.Object),
        "styles" .= (Seq.empty :: Seq.Seq A.Object),
        "properties"
          .= A.object
            [ "klighd.lsp.rendering.id" .= T.pack "$root$Na$$R0"
            ]
      ]

instance A.ToJSON GraphElement where
  toJSON
    KNode
      { children = c,
        renderings = r
      } =
      A.object
        [ "data" .= Seq.fromList r,
          "type" .= T.pack "node",
          "id" .= T.pack "$root$Na",
          "properties"
            .= A.object
              [ "org.eclipse.elk.nodeLabels.placement"
                  .= Seq.fromList
                    [ (1 :: Int),
                      (4 :: Int),
                      (6 :: Int)
                    ],
                "org.eclipse.elk.nodeSize.constraints"
                  .= Seq.fromList
                    [ (3 :: Int)
                    ],
                "org.eclipse.elk.nodeSize.minimum"
                  .= Seq.fromList
                    [ (64 :: Int),
                      (64 :: Int)
                    ]
              ],
          "children"
            .= Seq.fromList c
        ]
  toJSON KLabel {label = l} =
    A.object
      [ "type" .= T.pack "label",
        "text" .= l,
        "id" .= T.pack "$R0",
        "properties"
          .= A.object
            [],
        "data"
          .= Seq.fromList
            [ A.object
                [ "actions" .= (Seq.empty :: Seq.Seq A.Object),
                  "children" .= (Seq.empty :: Seq.Seq A.Object),
                  "properties"
                    .= A.object
                      [],
                  "styles" .= (Seq.empty :: Seq.Seq A.Object),
                  "text" .= T.pack "A",
                  "type" .= T.pack "KTextImpl"
                ]
            ],
        "children" .= (Seq.empty :: Seq.Seq A.Object)
      ]
