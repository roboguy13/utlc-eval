module Main where

import Prelude

import Effect.Aff.Class
import Effect
import Effect.Console

import Data.Foldable

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.HTML.HTMLElement as HTMLElement
import Web.DOM.Node as Node
import Web.DOM.Element as Element
import Web.DOM.Node (Node)
import Web.DOM.NodeList as NodeList

import Data.Maybe
import Data.List
import Data.Either
import Data.String as String
import Data.Array as Array

import UTLC.Eval.NbE

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI replComponent unit body

type ReplState = { input :: String, history :: Array String, message :: String }

data Action = Nop | Focus | UpdateInput String | ExecuteCommand

initialState :: ReplState
initialState = { input: "", history: [], message: "" }

replComponent :: forall query input output m. MonadAff m => H.Component query input output m
replComponent = H.mkComponent
  { initialState: \_ -> initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Focus }
  }

replInputRef :: H.RefLabel
replInputRef = H.RefLabel "replInput"

terminalRef :: H.RefLabel
terminalRef = H.RefLabel "terminalRef"

prompt :: String
prompt = ">> "

render :: forall m. MonadAff m => ReplState -> H.ComponentHTML Action () m
render st = 
  HH.div_
    [ HH.h1 [ HP.class_ (HH.ClassName "toolHeader") ]
            [ HH.text "UTLC Evaluator" ]
    , HH.div [ HP.id "panels" ]
        [HH.div [ HP.id "definitionsPanel" ]
          [ HH.h2 [ HP.class_ (HH.ClassName "panelHeader") ]
                  [ HH.text "Definitions" ]
          , HH.textarea
              [ HP.id "definitionsArea"
              , HP.rows 10
              , HP.placeholder "Type your definitions here"
              ]
          ]

        , HH.div [ HP.id "replPanel" ]
            [ HH.h2 [ HP.class_ (HH.ClassName "panelHeader") ]
                    [ HH.text "REPL" ]
            , HH.div [ HP.id "terminal", HP.ref terminalRef
                     , HE.onClick (\_ -> Focus)
                     ]
                (historyToHtml st
                <>
                [ HH.div [ HP.class_ (HH.ClassName "line")
                         , HP.id "line1" ]
                    
                    [ HH.span [ HP.id "promptSpan"
                              , HP.class_ (HH.ClassName "prompt") ]
                        [ HH.text prompt ]

                    , HH.input [ HP.id "replInput"
                               , HP.type_ HP.InputText
                               , HP.class_ (HH.ClassName "input")
                               , HE.onKeyDown mkAction
                               , HP.ref replInputRef
                               , HE.onValueInput updateInput
                               ]
                    ]
                ])
            ]
        , HH.div [ HP.id "instructionsPanel" ]
                [ HH.h2 [ HP.class_ (HH.ClassName "panelHeader") ]
                        [ HH.text "Syntax" ]
                , HH.div [ HP.class_ (HH.ClassName "instructionsContent") ]
                    instructions
                ]
        ]
    ]

historyToHtml :: forall w i. ReplState -> Array (HTML w i)
historyToHtml st =
  let texts = st.history -- <> [st.message]
  in
  Array.intersperse HH.br_ $ map HH.text texts

updateInput :: String -> Action
updateInput x = UpdateInput x

unlines :: Array String -> String
unlines = foldr go ""
  where
    go :: String -> String -> String
    go here rest =
      if String.null rest
      then here
      else here <> "\n" <> rest

mkAction :: KeyboardEvent -> Action
mkAction ev =
  if KE.key ev == "Enter"
  then ExecuteCommand
  else Nop

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM ReplState Action () o m Unit
handleAction = case _ of
  Nop -> pure unit
  Focus -> refocus
  UpdateInput str -> do
    H.modify_ \st -> st { input = str }
  ExecuteCommand  -> do
    st <- H.get
    H.liftEffect $ log $ "history = " <> show st.history
    H.modify_ \st -> st { history = st.history <> [prompt <> st.input] }
    updateTerminal $ "input = " <> st.input
    refocus
    -- -- Evaluate st.input and get the output, here assumed to be st.input for simplicity.
    -- let parserResult = runParser st.input (parseTerm <* eof)
    -- traceM $ "Input is " <> show st.input
    -- H.modify_ \st -> st { history = st.history <> [prompt <> st.input] }
    -- case parserResult of
    --   Left e -> updateTerminal ("Parse error: " <> show e)
    --   Right parsed -> do
    --     let nameless = fromNamed parsed
    --     let parsedNF = nf nameless
    --     case inferType emptyNameEnv nameless of
    --       Left e -> updateTerminal ("Type error: " <> e)
    --       Right ty -> do
    --         updateTerminal (ppr parsedNF)
    --         updateTerminal ("  : " <> ppr ty)
    -- refocus

refocus :: forall o m. MonadAff m => H.HalogenM ReplState Action () o m Unit
refocus = do
  H.getHTMLElementRef replInputRef >>= traverse_ \el ->
    H.liftEffect $ HTMLElement.focus el
  H.getHTMLElementRef terminalRef >>= traverse_ \el ->
    H.liftEffect $ Element.setScrollLeft 0.0 (HTMLElement.toElement el)

updateTerminal :: forall o m. MonadAff m => String -> H.HalogenM ReplState Action () o m Unit
updateTerminal resultMsg = do
  H.modify_ $ \st -> st { history = st.history <> [resultMsg], message = resultMsg }

instructions :: forall w i. Array (HTML w i)
instructions =
  [HH.table_
    [ HH.tr_
        [ HH.th_ [ HH.text "Description" ]
        , HH.th_ [ HH.text "Syntax" ]
        ]
    , HH.tr_
        [ HH.td_ [ HH.text "Anonymous function" ]
        , HH.td_ [ HH.pre_ [ HH.text "\\<var>. <term>" ] ]
        ]
    , HH.tr_
        [ HH.td_ [ HH.text "Function application" ]
        , HH.td_ [ HH.pre_ [ HH.text "<term> <term>" ] ]
        ]
    , HH.tr_
        [ HH.td_ [ HH.text "Printing to standard output" ]
        , HH.td_ [ HH.pre_ [ HH.text "print" ] ]
        ]
    , HH.tr_
        [ HH.td_ [ HH.text "Definition" ]
        , HH.td_ [ HH.pre_ [ HH.text "<defName> := <term>" ] ]
        ]
    ]
  , HH.br_
  , HH.ul_
      [ HH.li_ [ HH.text "Press the 'Reload' button to reload the definitions for use in the REPL." ]
      , HH.li_ [ HH.text $ "Each expression entered into the REPL is evaluated into a normal form and then this is printed. If it is not in a normal form after " <> show maxSteps <> " evaluation steps, evaluation is terminated with an error" ]
      , HH.li_ [ HH.text "Note that the ", printWord, HH.text " function prints its argument to standard output (after normalizing it) whenever an application of ", printWord, HH.text " to an argument is evaluated." ]
      ]
  ]

printWord :: forall w i. HTML w i
printWord = HH.span [ HP.class_ (HH.ClassName "monospace") ] [ HH.text "print" ]

