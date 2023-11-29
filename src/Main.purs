module Main where

import Prelude

import Effect.Aff.Class
import Effect
import Effect.Console

import Data.Foldable

import Parsing
import Parsing.String

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

import Data.Tuple

import UTLC.Eval.NbE
import UTLC.Syntax.Term

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI replComponent unit body

type ReplState = { input :: String, defsString :: String, defs :: List NamedDef, history :: Array String, message :: String }

data Action = Nop | Focus | UpdateInput String | UpdateDefString String | ReloadDefs | ExecuteCommand

initialState :: ReplState
initialState = { input: "", history: [], message: "", defsString: defaultDefString, defs: Nil }

replComponent :: forall query input output m. MonadAff m => H.Component query input output m
replComponent = H.mkComponent
  { initialState: \_ -> initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just ReloadDefs }
  }

replInputRef :: H.RefLabel
replInputRef = H.RefLabel "replInput"

terminalRef :: H.RefLabel
terminalRef = H.RefLabel "terminalRef"

defsRef :: H.RefLabel
defsRef = H.RefLabel "defsRef"

prompt :: String
prompt = ">> "

render :: forall m. MonadAff m => ReplState -> H.ComponentHTML Action () m
render st = 
  HH.div_
    [ HH.h1 [ HP.class_ (HH.ClassName "toolHeader") ]
            [ HH.text "UTLC Evaluator" ]
    , HH.div [ HP.id "panels" ]
        -- , HH.br_
        [ HH.div [ HP.id "definitionsPanel" ]
          [ HH.h2 [ HP.class_ (HH.ClassName "panelHeader") ]
                  [ HH.text "Definitions" ]
          , HH.textarea
              [ HP.id "definitionsArea"
              , HP.ref defsRef
              , HP.rows 10
              , HP.value defaultDefString
              , HE.onValueInput UpdateDefString
              ]
          , HH.button
            [ HE.onClick \_ -> ReloadDefs
            , HP.id "reloadButton"
            ]
            [ HH.text "Reload" ]
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
                        [ HH.text "Instructions" ]
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
  UpdateDefString str ->
    H.modify_ \st -> st { defsString = str }
  ReloadDefs -> do
    st <- H.get
    case runParser st.defsString (many parseDef <* eof) of
      Left err -> do
        updateTerminal $ "Parse error: " <> show err
        updateTerminal $ "In: " <> show st.defsString
      Right defs ->
        H.modify_ \st -> st { defs = defs }
  ExecuteCommand  -> do
    st <- H.get
    H.liftEffect $ log $ "history = " <> show st.history
    H.modify_ \st -> st { history = st.history <> [prompt <> st.input] }

    runInput st.input

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
        [ HH.td_ [ HH.text "Print the argument" ]
        , HH.td_ [ HH.pre_ [ HH.text "print" ] ]
        ]
    , HH.tr_
        [ HH.td_ [ HH.text "Definition" ]
        , HH.td_ [ HH.pre_ [ HH.text "<var> := <term>;" ] ]
        ]
    , HH.tr_
        [ HH.td_ [ HH.text "Comment" ]
        , HH.td_ [ HH.pre_ [ HH.text "-- This is a comment" ] ]
        ]
    ]
  , HH.br_
  , HH.h2_ [ HH.text "Examples" ]
  , HH.ul_
      [ HH.li_ [ HH.pre_ [ HH.text "(\\x. x) a" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "(\\x. x x) a" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "print b" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "(print c) (print d)" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "two (print c)" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "add two four (print c)" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "mult two three (print c)" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "exp two three (print c)" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "minus six two (print a)" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "isZero zero (print a) (print b)" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "isZero one (print a) (print b)" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "factorial three (print a)" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "fibonacci five (print a)" ] ]
      ]
  , HH.br_
  , HH.h2_ [ HH.text "Notes" ]
  , HH.ul_
      -- [ HH.li_ [ HH.text "Press the 'Reload' button to reload the definitions for use in the REPL." ]
      [ HH.li_ [ HH.text $ "Each expression entered into the REPL is evaluated into a normal form and then this is printed." ] -- If it is not in a normal form after " <> show maxSteps <> " evaluation steps, evaluation is terminated with an error" ]
      , HH.li_ [ HH.text "Note that the ", printWord, HH.text " function prints its argument (after normalizing it) whenever an application of ", printWord, HH.text " to an argument is evaluated." ]
      , HH.li_ [ HH.text "There is *nothing* else in the language that is not described above. Everything is either: an anonymous function, a function application or a variable. Definitions simply provide a way to name expressions. This could be eliminated from the language without changing any properties of the language, though it would be much harder to work with." ]
      ]
  ]

runInput :: forall o m. MonadAff m => String -> H.HalogenM ReplState Action () o m Unit
runInput input =
  case runParser input (parseTerm <* eof) of
    Left err -> updateTerminal $ "Parse error: " <> show err
    Right term -> do
      st <- H.get
      -- case runParser 
      case runEval (normalize mempty (fromNamed st.defs term)) of
          Left err -> updateTerminal err
          Right (Tuple stdout r) -> do
            traverse_ updateTerminal stdout
            updateTerminal $ "Normal form: " <> showTerm (toNamed r)


printWord :: forall w i. HTML w i
printWord = HH.span [ HP.class_ (HH.ClassName "monospace") ] [ HH.text "print" ]

defaultDefString :: String
defaultDefString =
  unlines
  [ "zero := \\f. \\x. x;"
  , "succ := \\n. (\\f. (\\x. (f (n f x))));"
  , "add := \\m. \\n. \\f. \\x. m f (n f x);"
  , "mult := \\m. \\n. \\f. \\x. m (n f) x;"
  , "exp := \\m. \\n. n m;"
  , ""
  , "one := succ zero;"
  , "two := succ one;"
  , "three := succ two;"
  , "four := succ three;"
  , "five := succ four;"
  , "six := succ five;"
  , ""
  , "pred :="
  , "  \\n. \\f. \\x."
  , "  n (\\g. \\h. h (g f)) (\\u. x) (\\u. u);"
  , ""
  , "minus :="
  , "  \\m. \\n. n pred m;"
  , ""
  , ""
  , "const := \\x. \\y. x;"
  , ""
  , "Y := \\f. (\\x. f (\\v. x x v)) (\\x. f (\\v. x x v));"
  , ""
  , "true := \\t. \\f. t;"
  , "false := \\t. \\f. f;"
  , "and := \\p. \\q. p q p;"
  , "or := \\p. \\q. p p q;"
  , "if := \\p. \\t. \\f. p t f;"
  , ""
  , "isZero := \\n. n (\\x. false) true;"
  , ""
  , "equal :="
  , "  Y (\\f."
  , "    \\m. \\n."
  , "    if (and (isZero m) (isZero n))"
  , "      true"
  , "      (if (or (isZero m) (isZero n))"
  , "         false"
  , "         (f (pred m) (pred n))));"
  , ""
  , "factorial :="
  , "  \\arg."
  , "  Y (\\f."
  , "      \\n."
  , "      if (isZero n)"
  , "        (\\v. one)"
  , "        (\\v. mult n (f (pred n) v)))"
  , "   arg dummy; -- The dummy argument is just because we need a thunk, since we are call-by-value"
  , ""
  , "fibonacci :="
  , "  \\arg."
  , "  Y (\\f."
  , "    \\n."
  , "    if (isZero n)"
  , "      (\\v. one)"
  , "      (if (isZero (pred n))  -- n = 1?"
  , "        (\\v. one)"
  , "        (\\v. add (f (pred n) v) (f (pred (pred n)) v))))"
  , "   arg dummy;"
  ]

