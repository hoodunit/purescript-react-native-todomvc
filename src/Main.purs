module Main where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (log)
import Data.Array (concat, modifyAt)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import React (ReactElement(), Render(), createClass, readState, spec, writeState)
import ReactNative (StyleId(), StyleSheet(), registerComponent, createStyleSheet, getStyleId)
import ReactNative.Components (ListViewDataSource(), listView, listViewDataSource, text, touchableNativeFeedback, view)
import ReactNative.Props (RenderSeparatorFn(), RenderHeaderFn(), dataSource, onPress, renderRow, renderSeparator, renderHeader)

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
  "todo": {
    paddingHorizontal: 10,
    paddingVertical: 15
    },
  "todoCompleted": {
    paddingHorizontal: 10,
    paddingVertical: 15,
    backgroundColor: "#DDDDDD"
    },
  "todoText": {
    fontSize: 18
    },
  "separator": {
    backgroundColor: "rgba(0, 0, 0, 0.15)",
    height: 1
    }
  }
  
appStyle :: String -> P.Props
appStyle key = P.unsafeMkProps "style" $ getStyleId appStyleSheet key

todoSeparator :: RenderSeparatorFn
todoSeparator sectionId rowId adjacentHighlighted = view [appStyle "separator"] []

toggleTodoAtIndex :: String -> Array Todo -> Array Todo
toggleTodoAtIndex rowId todos = fromMaybe todos $ do
  index <- fromString rowId
  newTodos <- modifyAt index toggleTodo todos
  return newTodos
  
toggleTodo :: Todo -> Todo
toggleTodo (Todo s complete) = Todo s (not complete)

render :: forall props eff. Render props (Array Todo) eff
render ctx = do
  todos <- readState ctx
  return $ 
    view [(appStyle "container")] [
      text [(appStyle "title")] [D.text "todos"],
      listView [appStyle "todoList",
                renderRow todoRow,
                renderSeparator todoSeparator,
                renderHeader $ view [appStyle "separator"] [],
                dataSource $ listViewDataSource todos]]
    where todoRow (Todo item completed) _ rowId _ = 
            touchableNativeFeedback [onPress onPressFn] $ rowView
            where onPressFn _ = do
                    todos <- readState ctx
                    writeState ctx $ toggleTodoAtIndex rowId todos
                  rowView = view [appStyle todoStyle] [todoText]
                  todoStyle = (if completed then "todoCompleted" else "todo")
                  todoText = text [appStyle "todoText"] [D.text item]
        
foreign import unsafeLog :: forall p e. p -> Eff e Unit
  
main = do
  log "Running app"
  registerComponent appName component
  where
    component = createClass viewSpec
    viewSpec = (spec initialTodos render)
