module HelloWorld where

import Prelude
import React.DOM as D
import React.DOM.Props as P
import Control.Monad.Eff (Eff)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import React (createClass, spec, createFactory, ReactClass, ReactElement, createClassStateless)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust, Maybe(..))
import Data.Nullable (toMaybe)
import ReactDOM (render)
import React
import Data.Unit (unit)

helloWorld :: ReactClass Unit
helloWorld = createClassStateless helloText
  where
    helloText :: Unit -> ReactElement
    helloText _ = D.h1 [] [D.text "hello dfkajk world"]

helloWorld2 :: ReactElement
helloWorld2 = createFactory helloWorld unit

helloWorld3 :: forall props. ReactClass { name :: String | props }
helloWorld3 = createClass $ spec unit \ctx -> do
  props <- getProps ctx
  pure $ D.h1 [ P.className "Hello"
              , P.style { background: "lightgray" }
              ]
              [ D.text "Hello, "
              , D.text props.name
              , createElement (createClassStateless \props' -> D.div' [ D.text $ "Stateless" <> props'.test ])
                                { test: " test" } []
              ]

helloWorld4 :: ReactElement
helloWorld4 = createFactory helloWorld3 { name: "world "}

