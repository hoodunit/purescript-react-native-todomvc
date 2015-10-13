module Main where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (log)
import Data.Array ((:), concat, filter, findIndex, length, modifyAt, range, sortBy, zip)
import Data.Int (fromString)
import Data.Maybe (Maybe(), fromMaybe)
import Data.Tuple (fst, snd)
import React (ReactElement(), ReactThis(), Render(), createClass, readState, spec, transformState, writeState)
import ReactNative (registerComponent)
import ReactNative.Components (ListViewDataSource(), cloneWithRows, listView, listViewDataSource, text, textInput, touchableHighlight, view)

import qualified ReactNative.Props as N
import qualified React.DOM.Props as P
import qualified ReactNative.Styles as S

data AppState = AppState {
  nextId :: Int, 
  newTodo :: String, 
  todos :: Array Todo, 
  dataSource :: ListViewDataSource, 
  filter :: Filter
  }
data Todo = Todo Int String Boolean
instance todoEq :: Eq Todo where
  eq (Todo id1 item1 c1) (Todo id2 item2 c2) = (id1 == id2) && (item1 == item2) && (c1 == c2)

getTodoId :: Todo -> Int
getTodoId (Todo id _ _) = id
  
data Filter = All | Active | Completed
instance eqFilter :: Eq Filter where
  eq All       All       = true
  eq Active    Active    = true
  eq Completed Completed = true
  eq _         _         = false

initialTodos = [
  Todo 1 "Hack PureScript into Android (using JS mostly)" true,
  Todo 2 "Display text field using purescript-react" true,
  Todo 3 "Port SampleApp to PureScript" true,
  Todo 4 "Pull out bindings as separate library" true,
  Todo 5 "Add support for ListViews" true,
  Todo 6 "Display list of todo items" true,
  Todo 7 "Fix wrapping of long todos" true,
  Todo 8 "Display completed items as completed" true,
  Todo 9 "Make items highlightable" true,
  Todo 10 "Make items completable" true,
  Todo 12 "Add new todos" true,
  Todo 13 "Clear completed todos" true,
  Todo 14 "Filter All/Active/Completed todos" true,
  Todo 15 "Delete todos" false,
  Todo 16 "Clean up styling" false,
  Todo 17 "Re-focus input field when adding todo" false
]

appStyleSheet :: S.StyleSheet
appStyleSheet = S.createStyleSheet {
  container: {
     flex: 1,
     flexDirection: "column",
     backgroundColor: backgroundColor
     },
  title: {
     fontSize: 50,
     color: "rgba(175, 47, 47, 0.15)",
     textAlign: "center"
     },
  todoList: {
    flex: 1,
    flexDirection: "column"
    },
  newTodoContainer: {
    fontSize: 18,
    paddingHorizontal: 10,
    height: 56,
    backgroundColor: todoBackgroundColor,
    textDecorationColor: fontColorFaded,
    borderTopColor: borderColor,
    borderTopWidth: 1,
    borderBottomColor: borderColor,
    borderBottomWidth: 1
    },
  newTodo: {
    fontSize: 18,
    paddingHorizontal: 10,
    flex: 1,
    backgroundColor: todoBackgroundColor,
    textDecorationColor: fontColorFaded
    },
  todo: {
    paddingHorizontal: 10,
    paddingVertical: 15,
    backgroundColor: todoBackgroundColor
    },
  todoText: {
    fontSize: 18,
    color: fontColorDefault
    },
  todoTextCompleted: {
    color: fontColorFaded
    },
  separator: {
    backgroundColor: borderColor,
    height: 1
    },
  bottomBar: {
    paddingVertical: 10,
    paddingHorizontal: 15,
    borderTopColor: borderColor,
    borderTopWidth: 1,
    flexDirection: "row",
    alignItems: "stretch",
    backgroundColor: todoBackgroundColor
    },
  filters: {
    flexDirection: "row",
    alignItems: "stretch",
    flex: 1
    },
  filter: {
    marginHorizontal: 5,
    padding: 5
    },
  activeFilter: {
    borderWidth: 1,
    borderColor: "rgba(175, 47, 47, 0.2)",
    borderStyle: "solid",
    borderRadius: 3
    },
  clearCompleted: {
    margin: 5
    }
  }

fontColorDefault = "#000000" 
fontColorFaded = "#D9D9D9" 
backgroundColor = "#F5F5F5"
todoBackgroundColor = "#FFFFFF"
borderColor = "#EDEDED"
  
style :: String -> P.Props
style key = S.style $ S.getStyleId appStyleSheet key
  
styles :: Array String -> P.Props
styles keys = S.styles $ map (S.getStyleId appStyleSheet) keys


toggleTodoWithId :: Int -> AppState -> AppState
toggleTodoWithId id (AppState state) = fromMaybe (AppState state) $ do
  index <- findIndex (((==) id) <<< getTodoId) state.todos
  newTodos <- modifyAt (unsafeLog2 index) toggleTodo state.todos
  return $ updateDataSource $ AppState $ state { todos = newTodos }
  
toggleTodo :: Todo -> Todo
toggleTodo (Todo id s complete) = Todo id s (not complete)

addTodo :: AppState -> AppState
addTodo (AppState state) = updateDataSource $ AppState $ state { nextId = state.nextId + 1, newTodo = "", todos = newTodos }
  where newTodos = (Todo state.nextId state.newTodo false) : state.todos
        
updateNewTodo :: String -> AppState -> AppState
updateNewTodo newTodo (AppState state) = AppState state { newTodo = newTodo }

clearCompleted :: AppState -> AppState
clearCompleted (AppState state) = updateDataSource $ AppState $ state { todos = newTodos }
  where newTodos = filter notCompleted state.todos
        notCompleted (Todo _ _ completed) = not completed
        
filterTodos :: Filter -> AppState -> AppState
filterTodos filter (AppState state) = updateDataSource $ AppState $ state { filter = filter }


todoOrdering :: Todo -> Todo -> Ordering
todoOrdering (Todo _ _ true) (Todo _ _ false) = GT
todoOrdering (Todo _ _ false) (Todo _ _ true) = LT
todoOrdering (Todo id1 _ _) (Todo id2 _ _) = if id1 < id2 then LT else GT

updateDataSource :: AppState -> AppState
updateDataSource (AppState state) = AppState $ state { dataSource = cloneWithRows state.dataSource filteredTodos }
  where filteredTodos = sortBy todoOrdering $ filter (applyFilter state.filter) state.todos

applyFilter :: Filter -> Todo -> Boolean
applyFilter All _ = true
applyFilter Active (Todo _ _ c) = not c
applyFilter Completed (Todo _ _ c) = c

todoSeparator :: N.RenderSeparatorFn
todoSeparator sectionId rowId adjacentHighlighted = view [style "separator"] []
          
filterButton :: forall props. ReactThis props AppState -> Filter -> Filter -> ReactElement
filterButton ctx activeFilter filter = 
  view [styles (if activeFilter == filter then ["filter", "activeFilter"] else ["filter"])] [
    text [N.onPress \_ -> transformState ctx (filterTodos filter)] filterText]
  where filterText = case filter of 
          All -> "All"
          Active -> "Active"
          Completed -> "Completed"
          
todoRow ctx (Todo id item completed) _ _ _ = touchableHighlight [N.onPress onPressFn] $ rowView
  where
    rowView = view [style "todo"] [todoText]
    todoText = text [styles (if completed then ["todoText", "todoTextCompleted"] else ["todoText"])] item
    onPressFn _ = transformState ctx (toggleTodoWithId (unsafeLog2 id))

render :: forall props eff. Render props AppState eff
render ctx = do
  (AppState state) <- readState ctx
  return $ 
    view [(style "container")] [
      text [style "title"] "todos",
      view [style "newTodoContainer"] [
        textInput [style "newTodo", 
                   P.value state.newTodo,
                   P.placeholder "What needs to be done?",
                   N.onChangeText \newTodo -> transformState ctx (updateNewTodo newTodo),
                   N.onSubmitEditing \_ -> transformState ctx addTodo]],
      listView [style "todoList",
                N.renderRow $ todoRow ctx,
                N.renderSeparator todoSeparator,
                N.renderHeader $ view [style "separator"] [],
                N.dataSource state.dataSource],
      view [style "bottomBar"] [
        view [style "filters"] [
           filterButton ctx state.filter All, 
           filterButton ctx state.filter Active,
           filterButton ctx state.filter Completed],
        text [style "clearCompleted", 
              N.onPress \_ -> transformState ctx clearCompleted] 
             "Clear completed"]]
        
foreign import unsafeLog :: forall p e. p -> Eff e Unit
foreign import unsafeLog2 :: forall p. p -> p
  
main = do
  log "Running app"
  registerComponent "PureScriptSampleApp" component
  where
    component = createClass $ spec initialState render
    dataSource = listViewDataSource initialTodos
    initialState = updateDataSource $ AppState { nextId: 18, 
                                                 newTodo: "", 
                                                 todos: initialTodos, 
                                                 dataSource: dataSource, 
                                                 filter: All }
