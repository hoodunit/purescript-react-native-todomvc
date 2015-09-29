module Main where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (log)
import Data.Array (concat, modifyAt)
import Data.Int (fromString)
import Data.Maybe (Maybe(), fromMaybe)
import React (ReactElement(), Render(), createClass, readState, spec, writeState)
import ReactNative (StyleId(), StyleSheet(), registerComponent, createStyleSheet, getStyleId)
import ReactNative.Components (ListViewDataSource(), cloneWithRows, listView, listViewDataSource, text, textInput, touchableNativeFeedback, view)
import ReactNative.Props (RenderSeparatorFn(), RenderHeaderFn(), dataSource, onPress, renderRow, renderSeparator, renderHeader)

import qualified React.DOM as D
import qualified React.DOM.Props as P

data AppState = AppState { todos :: Array Todo, dataSource :: ListViewDataSource }
data Todo = Todo String Boolean

instance todoEq :: Eq Todo where
  eq (Todo t1 c1) (Todo t2 c2) = (t1 == t2) && (c1 == c2)

initialTodos = [
  Todo "Hack PureScript into Android (using JS mostly)" true,
  Todo "Display text field using purescript-react" true,
  Todo "Port SampleApp to PureScript" true,
  Todo "Pull out bindings as separate library" true,
  Todo "Add support for ListViews" true,
  Todo "Display list of todo items" true,
  Todo "Fix wrapping of long todos" true,
  Todo "Display completed items as completed" true,
  Todo "Make items highlightable" true,
  Todo "Make items completable" true,
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
  "todoInput": {
    fontSize: 18,
    paddingHorizontal: 10
    },
  "todoList": {
    flex: 1,
    flexDirection: "column"
    },
  "todo": {
    paddingHorizontal: 10,
    paddingVertical: 15,
    backgroundColor: "#FFFFFF"
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
    backgroundColor: "#CCCCCC",
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
  return (unsafeLog2 newTodos)
  
toggleTodo :: Todo -> Todo
toggleTodo (Todo s complete) = Todo s (not complete)

render :: forall props eff. Render props AppState eff
render ctx = do
  (AppState state) <- readState ctx
  return $ 
    view [(appStyle "container")] [
      text [appStyle "title"] [D.text "todos"],
      textInput [appStyle "todoInput", P.placeholder "What needs to be done?"],
      listView [appStyle "todoList",
                renderRow todoRow,
                renderSeparator todoSeparator,
                renderHeader $ view [appStyle "separator"] [],
                dataSource state.dataSource]]
    where todoRow (Todo item completed) _ rowId _ = 
            touchableNativeFeedback [onPress onPressFn] $ rowView
            where onPressFn _ = do
                    (AppState state) <- readState ctx
                    let newTodos = toggleTodoAtIndex rowId state.todos
                    writeState ctx $ AppState { todos: newTodos, dataSource: cloneWithRows state.dataSource newTodos }
                  rowView = view [appStyle todoStyle] [todoText]
                  todoStyle = (if completed then "todoCompleted" else "todo")
                  todoText = text [appStyle "todoText"] [D.text item]
        
foreign import unsafeLog :: forall p e. p -> Eff e Unit
foreign import unsafeLog2 :: forall p. p -> p
  
main = do
  log "Running app"
  registerComponent appName component
  where
    component = createClass viewSpec
    viewSpec = (spec initialState render)
    initialState = AppState { todos: initialTodos, dataSource: listViewDataSource initialTodos }
