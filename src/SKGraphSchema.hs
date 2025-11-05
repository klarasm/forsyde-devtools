{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SKGraphSchema where

-- import Autodocodec

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T

data GraphElement
  = KLabel
      { label :: !T.Text,
        gid :: !T.Text
      }
  | KNode
      { children :: ![GraphElement],
        renderings :: ![KRenderings],
        properties :: !KProperties,
        gid :: !T.Text
      }
  | KPort
      { children :: ![GraphElement],
        renderings :: ![KRenderings],
        properties :: !KProperties,
        gid :: !T.Text
      }
  | KEdge
      { children :: ![GraphElement],
        renderings :: ![KRenderings],
        properties :: !KProperties,
        gid :: !T.Text,
        source :: !T.Text,
        target :: !T.Text
      }

data KProperty
  = NodeLabelsPlacement
  | NodeSizeConstraints
  | NodeSizeMinimum
  | EdgeType
  | JunctionPoints

instance Show KProperty where
  show NodeLabelsPlacement = "org.eclipse.elk.nodeLabels.placement"
  show NodeSizeConstraints = "org.eclipse.elk.nodeSize.constraints"
  show NodeSizeMinimum = "org.eclipse.elk.nodeSize.minimum"
  show EdgeType = "org.eclipse.elk.edge.type"
  show JunctionPoints = "org.eclipse.elk.junctionPoints"

type KProperties = [(KProperty, [Int])]

data KRenderings
  = KEllipse
  | KPolyline
  | KRectangle

instance A.ToJSON KRenderings where
  toJSON KEllipse =
    A.object
      [ "type" .= T.pack "KEllipseImpl",
        "children" .= (Seq.empty :: Seq.Seq A.Object),
        "actions" .= (Seq.empty :: Seq.Seq A.Object),
        "styles" .= (Seq.empty :: Seq.Seq A.Object),
        "properties"
          .= A.object
            [ "klighd.lsp.rendering.id" .= T.pack "$R0"
            ]
      ]
  toJSON KPolyline =
    A.object
      [ "type" .= T.pack "KPolylineImpl",
        "children" .= (Seq.empty :: Seq.Seq A.Object),
        "actions" .= (Seq.empty :: Seq.Seq A.Object),
        "styles" .= (Seq.empty :: Seq.Seq A.Object),
        "properties"
          .= A.object
            [ "klighd.lsp.rendering.id" .= T.pack "$R0"
            ]
      ]
  toJSON KRectangle =
    A.object
      [ "type" .= T.pack "KRectangleImpl",
        "children" .= (Seq.empty :: Seq.Seq A.Object),
        "actions" .= (Seq.empty :: Seq.Seq A.Object),
        "styles" .= (Seq.empty :: Seq.Seq A.Object),
        "properties"
          .= A.object
            [ "klighd.lsp.rendering.id" .= T.pack "$R0"
            ]
      ]

instance A.ToJSON GraphElement where
  toJSON
    KNode
      { children = c,
        renderings = r,
        properties = p,
        gid = i
      } =
      A.object
        [ "data" .= Seq.fromList r,
          "type" .= T.pack "node",
          "id" .= i,
          "properties" .= (M.fromList $ map (\(a, b) -> (T.pack $ show a, Seq.fromList b)) p),
          "children"
            .= Seq.fromList c
        ]
  toJSON KLabel {label = l, gid = i} =
    A.object
      [ "type" .= T.pack "label",
        "text" .= l,
        "id" .= i,
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
                      [ "klighd.lsp.rendering.id" .= T.pack "$R0"
                      ],
                  "styles" .= (Seq.empty :: Seq.Seq A.Object),
                  "text" .= l,
                  "type" .= T.pack "KTextImpl"
                ]
            ],
        "children" .= (Seq.empty :: Seq.Seq A.Object)
      ]
  toJSON
    KPort
      { children = c,
        renderings = r,
        properties = p,
        gid = i
      } =
      A.object
        [ "data" .= Seq.fromList r,
          "type" .= T.pack "port",
          "id" .= i,
          "properties" .= (M.fromList $ map (\(a, b) -> (T.pack $ show a, Seq.fromList b)) p),
          "children"
            .= Seq.fromList c
        ]
  toJSON
    KEdge
      { children = c,
        renderings = r,
        properties = p,
        gid = i,
        source = s,
        target = t
      } =
      A.object
        [ "data" .= Seq.fromList r,
          "type" .= T.pack "edge",
          "id" .= i,
          "properties" .= (M.fromList $ map (\(a, b) -> (T.pack $ show a, Seq.fromList b)) p),
          "children"
            .= Seq.fromList c,
          "sourceId" .= s,
          "targetId" .= t,
          "junctionPoints" .= A.object []
        ]
