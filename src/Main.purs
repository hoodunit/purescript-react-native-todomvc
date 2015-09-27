module Main where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (log)
import React (ReactElement(), Render(), createClass, spec)
import ReactNative (StyleId(), StyleSheet(), registerComponent, createStyleSheet, getStyleId)
import ReactNative.Components (ListViewDataSource(), listView, listViewDataSource, text, view)
import ReactNative.Props (renderRow, dataSource)

import qualified React.DOM as D
import qualified React.DOM.Props as P

data Todo = Todo String Boolean

initialTodos = [
  Todo "Get milk" false,
  Todo "Return library books" false,
  Todo "Drink water" true,
  Todo "Return papers" false,
  Todo "Call your mother" false
]

appName = "PureScriptSampleApp"

appStyleSheet :: StyleSheet
appStyleSheet = createStyleSheet {
  "container": {
     flex: 1,
     flexDirection: "column",
     justifyContent: "center"
     },
  "title": {
     fontSize: 40,
     textAlign: "center"
     },
  "todoList": {
    flex: 1,
    flexDirection: "column"
    },
  "todoRow": {
    flexDirection: "column",
    height: 40,
    padding: 10,
    marginBottom: 10
    },
  "todoText": {
    fontSize: 24
    },
  "separator": {
    backgroundColor: "rgba(0, 0, 0, 0.1)",
    height: 1,
    marginVertical: 10
    }
  }
  
appStyle :: String -> P.Props
appStyle key = P.unsafeMkProps "style" $ getStyleId appStyleSheet key

todoRow :: String -> Int -> Int -> Int -> ReactElement
todoRow rowData sectionId rowId highlightRow = 
  view [appStyle "todoRow"] [
    text [appStyle "todoText"] [D.text rowData],
    view [appStyle "separator"] []]

todoDataSource :: Array Todo -> ListViewDataSource
todoDataSource todos = listViewDataSource $ map todoText todos
  where todoText (Todo text _) = text

todoList :: Array Todo -> ReactElement
todoList todos = listView 
  [appStyle "todoList",
   renderRow todoRow,
   dataSource $ todoDataSource todos]

render :: forall props state eff. Render props state eff
render ctx = pure $ view [(appStyle "container")] [
  text [(appStyle "title")] [D.text "todos"],
  todoList initialTodos
  ]
        
foreign import unsafeLog :: forall p e. p -> Eff e Unit
  
main = do
  log "Running app"
  registerComponent appName component
  unsafeLog $ todoList initialTodos
  unsafeLog $ todoDataSource initialTodos
  where
    component = createClass viewSpec
    viewSpec = (spec unit render)
