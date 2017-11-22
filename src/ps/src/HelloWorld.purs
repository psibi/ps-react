module HelloWorld where

import Prelude
import React.DOM as D
import React.DOM.Props as P
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import Data.Int (decimal, toStringAs)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import React (createClass, spec, createFactory, ReactClass, ReactElement, createClassStateless)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust, Maybe(..))
import Data.Nullable (toMaybe)
import ReactDOM (render)
import React
import Data.Unit (unit)

foreign import interval :: forall eff a.
                             Int ->
                             Eff eff a ->
                             Eff eff Unit

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

newtype AppState = AppState
  { count :: Int }


helloWorld5 :: forall props. ReactClass props
helloWorld5 = createClass counterSpec
  where
  
  initialState :: AppState
  initialState = AppState { count: 0  }

  counterSpec = (spec initialState render1)
      { componentDidMount = \ctx ->
          interval 1000 $ do
            readState ctx >>=
              toString >>> log
      }
 
  toString :: AppState -> String
  toString ( AppState { count } )  =
        toStringAs decimal count

  addOne :: AppState -> AppState
  addOne ( AppState { count } ) = do
      AppState { count: count + 1 }

  render1 ctx = do
    count <- readState ctx
    pure $ D.button [ P.className "Counter"
                    , P.onClick \_ -> do
                        readState ctx >>=
                          addOne >>> writeState ctx
                    ]
                    [ D.text (toString count)
                    , D.text " Click me to increment!"
                    ]


helloWorld6 :: ReactElement
helloWorld6 = createFactory helloWorld5 unit

type ReactEff e = ( props :: ReactProps, refs :: ReactRefs (read :: Read), state :: ReactState ReadWrite | e)

type ReactCEff eff = ( props :: ReactProps, refs :: ReactRefs (read :: Read), state :: ReactState ReadWrite, console :: CONSOLE | eff)

helloWorld7 :: forall props eff.
  ReactClass
    { routeToUrl :: String
                    -> Unit
                    -> Eff (ReactCEff eff) Unit
    , name :: String
    | props
    }
helloWorld7 = createClass $ spec unit \ctx -> do
  props <- getProps ctx
  pure $ D.h1 [ P.className "Hello"
              , P.style { background: "lightgray" }
              , P.onClick \_ -> do
                  props.routeToUrl "Hello" unit
                  log "hell"
                  pure unit
              ]
              [ D.text "Click handler "
              , D.text props.name
              ]

someEff :: forall eff.  String -> Unit -> Eff (console :: CONSOLE | eff) Unit
someEff x _ = log x

helloWorld8 :: ReactElement
helloWorld8 = createFactory helloWorld7 { name: "world ", routeToUrl: someEff}
