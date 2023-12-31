module Main where

import Prelude

import Effect.Aff.Class
import Effect
import Effect.Console

import Data.Foldable

import Parsing hiding (Parser)
import Parsing.Combinators (try, (<|>))
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
import UTLC.Utils (unlines)
import UTLC.Backend.Python

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI replComponent unit body

type ReplState = { input :: String, defsString :: String, defs :: List NamedDef, history :: Array String, message :: String }

data Action = Nop | Focus | UpdateInput String | UpdateDefString String | ReloadDefs | ExecuteCommand

data ReplCmd
  = ReplTerm NamedTerm
  | Compile NamedTerm

parseReplCmd :: Parser ReplCmd
parseReplCmd =
  try (symbol ":" *> keyword "compile" *> map Compile parseTerm)
  <|>
  try (map ReplTerm parseTerm)

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
                               , HE.onClick (\_ -> Focus)
                               , HE.onLoad (\_ -> Focus)
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
    , HH.tr_
        [ HH.td_ [ HH.text "Compile to Python (this is a REPL command)" ]
        , HH.td_ [ HH.pre_ [ HH.text ":compile <term>" ] ]
        ]
    ]
  , HH.br_
  , HH.h2_ [ HH.text "Examples" ]
  , HH.ul_
      [ HH.li_ [ HH.pre_ [ HH.text "(\\x. x) a" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "(\\x. x x) a" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "print b" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "print b (\\x. x)" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "print b id" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "print c (print d id)" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "two" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "add two one" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "minus six two" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "two (print c) id" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "add two four (print c) id" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "mult two three (print c) id" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "exp two three (print c) id" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "minus six two (print a) id" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "isZero zero (print a) (print b) id" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "isZero one (print a) (print b) id" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "factorial three (print a) id" ] ]
      , HH.li_ [ HH.pre_ [ HH.text "fibonacci five (print a) id" ] ]
      ]
  , HH.br_
  , HH.h2_ [ HH.text "Notes" ]
  , HH.ul_
      -- [ HH.li_ [ HH.text "Press the 'Reload' button to reload the definitions for use in the REPL." ]
      [ HH.li_ [ HH.text "Each expression entered into the REPL is evaluated into a normal form and then this is printed." ] -- If it is not in a normal form after " <> show maxSteps <> " evaluation steps, evaluation is terminated with an error" ]
      , HH.li_ [ monospace "id", HH.text " is just a name for the identity function. See the definitions panel. The identity function is ", monospace "\\x. x" ]
      , HH.li_ [ HH.text "Note that the ", printWord, HH.text " function prints its argument (after normalizing it) whenever an application of ", printWord, HH.text " to an argument is evaluated. More specifically, when applied to an argument it gives back a function that will print when that function is applied to a function. This whole thing normalizes to the function its applied to. See the examples." ]
      , HH.li_ [ HH.text "There is *nothing* else in the language that is not described above. Everything is either: an anonymous function, a function application, ", monospace "print", HH.text " or a variable. Definitions simply provide a way to name expressions as a shorthand. This could be eliminated from the language without changing any properties of the language, though it would be much harder to work with." ]
      ]
  ]

runInput :: forall o m. MonadAff m => String -> H.HalogenM ReplState Action () o m Unit
runInput input =
  case runParser input (parseReplCmd <* eof) of
    Left err -> updateTerminal $ "Parse error: " <> show err
    Right (Compile term) -> do
       st <- H.get

       let pyCode = pythonPrelude <> "\n" <> toPython st.defs term

       updateTerminal $ "# Python code:\n" <> pyCode
    Right (ReplTerm term) -> do
      st <- H.get
      -- case runParser 
      case runEval (normalize mempty (fromNamed st.defs term)) of
          Left err -> updateTerminal err
          Right (Tuple stdout r) -> do
            traverse_ updateTerminal stdout
            updateTerminal $ "Normal form: " <> showTerm (toNamed r)


printWord :: forall w i. HTML w i
printWord = monospace "print"

monospace :: forall w i. String -> HTML w i
monospace str = HH.span [ HP.class_ (HH.ClassName "monospace") ] [ HH.text str ]

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
  , "id := \\x. x;"
  , "const := \\x. \\y. x;"
  , ""
  , "Y := \\f. (\\x. f (\\v. x x v)) (\\x. f (\\v. x x v));"
  , ""
  , "true := \\t. \\f. t;"
  , "false := \\t. \\f. f;"
  , "and := \\p. \\q. p q p;"
  , "or := \\p. \\q. p p q;"
  , "if := \\p. \\t. \\f. p t f;"
  , "not := \\x. if x false true;"
  , ""
  , "isZero := \\n. n (\\x. false) true;"
  , ""
  , "equal :="
  , "  \\x. \\y."
  , "  Y (\\f."
  , "    \\m. \\n."
  , "    if (and (isZero m) (isZero n))"
  , "      (\\v. true)"
  , "      (if (or (isZero m) (isZero n))"
  , "         (\\v. false)"
  , "         (\\v. f (pred m) (pred n) v)))"
  , "  x y dummy;"
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
  , ""
  , "lessEqual :="
  , "  \\n."
  , "  \\m."
  , "  Y (\\f."
  , "    \\x."
  , "    \\y."
  , "    if (isZero x)"
  , "       (\\v. true)"
  , "       (if (isZero y)"
  , "           (\\v. false)"
  , "           (\\v. f (pred x) (pred y) v)))"
  , "  n"
  , "  m"
  , "  dummy;"
  , ""
  , "lessThan :="
  , "  \\x. \\y. and (lessEqual x y) (not (equal x y));"
  , ""
  , "mod :="
  , "  \\n."
  , "  \\m."
  , "  Y (\\f."
  , "    \\x."
  , "    \\y."
  , "    if (lessThan x y)"
  , "       (\\v. x)"
  , "       (\\v. f (minus x y) y v))"
  , "  n"
  , "  m"
  , "  dummy;"
  -- , ""
  -- , "sumThreeFiveMultiples :="
  -- , "  \\n."
  -- , "  Y (\\f."
  -- , "    \\x."
  -- , "    if (isZero x)"
  -- , "      (\\v. zero)"
  -- , "      if (or (equal (mod x three) zero)"
  -- , "             (equal (mod x five) zero))"
  -- , "         (\\v. add x (f (pred x) v))"
  -- , "         (\\v. f (pred x) v))"
  -- , "  n dummy;"
  -- , ""
  -- , "ten := mult two five;"
  -- , "oneThousand := mult ten (mult ten ten);"
  -- , ""
  -- , "projectEuler1 :="
  -- , "  sumThreeFiveMultiples oneThousand;"
  ]

