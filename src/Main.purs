module Main where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (log)
import Data.Array (concat)
import React (ReactElement(), Render(), createClass, spec)
import ReactNative (StyleId(), StyleSheet(), registerComponent, createStyleSheet, getStyleId)
import ReactNative.Components (ListViewDataSource(), listView, listViewDataSource, text, view)
import ReactNative.Props (RenderRowFn(), RenderSeparatorFn(), RenderHeaderFn(), dataSource, renderRow, renderSeparator, renderHeader)

import qualified React.DOM as D
import qualified React.DOM.Props as P

data Todo = Todo String Boolean

initialTodos = [
  Todo "Hack PureScript into Android (using JS mostly)" true,
  Todo "Display text field using purescript-react" true,
  Todo "Port SampleApp to PureScript" true,
  Todo "Pull out bindings as separate library" true,
  Todo "Add support for ListViews" true,
  Todo "Display list of todo items" true,
  Todo "Fix wrapping of long todos" false,
  Todo "Display completed items as completed" false,
  Todo "Make items highlightable" false,
  Todo "Make items completable" false,
  Todo "Add new todos" false,
  Todo "Clear completed todos" false,
  Todo "Filter All/Active/Completed todos" false,
  Todo "Clean up styling" false
]

appName = "PureScriptSampleApp"

appStyleSheet :: StyleSheet
appStyleSheet = createStyleSheet {
  "container": {
     flex: 1,
     flexDirection: "column"
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
    justifyContent: "flex-start",
    flex: 1,
    paddingHorizontal: 10
    },
  "todoText": {
    fontSize: 20
    },
  "separator": {
    backgroundColor: "rgba(0, 0, 0, 0.1)",
    height: 1,
    marginVertical: 10
    }
  }
  
appStyle :: String -> P.Props
appStyle key = P.unsafeMkProps "style" $ getStyleId appStyleSheet key

todoRow :: RenderRowFn
todoRow rowData sectionId rowId highlightRow = 
  view [appStyle "todoRow"] [text [appStyle "todoText"] [D.text rowData]]

todoSeparator :: RenderSeparatorFn
todoSeparator sectionId rowId adjacentHighlighted = view [appStyle "separator"] []

todoDataSource :: Array Todo -> ListViewDataSource
todoDataSource todos = listViewDataSource $ map todoText todos
  where todoText (Todo text _) = text

todoList :: Array Todo -> ReactElement
todoList todos = listView 
  [appStyle "todoList",
   renderRow todoRow,
   renderSeparator todoSeparator,
   renderHeader $ view [appStyle "separator"] [],
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
