module HelloWorld where

import Prelude
import React.DOM as D
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

helloWorld :: ReactClass Unit
helloWorld = createClassStateless helloText
  where
    helloText :: Unit -> ReactElement
    helloText _ = D.h1 [] [D.text "hello dfkajk world"]

helloWorld2 :: ReactElement
helloWorld2 = createFactory helloWorld unit

