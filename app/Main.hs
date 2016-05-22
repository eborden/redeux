{-# LANGUAGE DeriveFunctor, LambdaCase, OverloadedStrings, RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Monad (when, void)
import Control.Monad.Trans (liftIO)
import Control.Monad.Free (liftF, iterM)
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.String (fromString)

import qualified Redeux as Redeux
import qualified Redeux.DOM.Core as DOM
import Redeux.DOM

main :: IO ()
main = do
  inject <- DOM.createInjector
  void . Redeux.redeux initialTodo interpreter $ inject todoMain

data Todo
  = Todo
  { currentFilter :: Filter
  , todos :: [(Text, Bool)]
  , editing :: Maybe Int
  } deriving (Show)

initialTodo :: Todo
initialTodo = Todo { currentFilter = All
                   , todos = [ ("Done task", True)
                             , ("Active task", False)
                             ]
                   , editing = Nothing
                   }

data Filter = All | Active | Complete
  deriving (Show)

data Action next
  = Add Text next
  | Edit (Maybe Int) next
  | Update Int Text next
  | Delete Int next
  | SetFilter Filter next
  | ToggleComplete Int next
  | Async (Redeux.Command Action ()) next
  deriving (Functor)

interpreter :: Redeux.Reducer Action Todo a
interpreter = iterM $ \case
  Add str next -> do
    Redeux.modifyState_ $ \s -> s {todos = todos s ++ [(str, False)]}
    next
  Edit target next -> do
    Redeux.modifyState_ $ \s -> s {editing = target}
    next
  Update target str next -> do
    Redeux.modifyState_ $ \s ->
      let (heads, (_, bool):tails) = splitAt target $ todos s
      in s {todos = heads ++ (str, bool):tails}
    next
  Delete target next -> do
    Redeux.modifyState_ $ \s ->
      case splitAt target $ todos s of
        (heads, _:tails) -> s {todos = heads ++ tails}
        (heads, tails)   -> s {todos = heads ++ tails}
    next
  SetFilter newFilter next -> do
    Redeux.modifyState_ $ \s -> s {currentFilter = newFilter}
    next
  ToggleComplete target next -> do
    Redeux.modifyState_ $ \s ->
      let (heads, (str, bool):tails) = splitAt target $ todos s
      in s {todos = heads ++ (str, not bool):tails}
    next
  Async command next -> do
    env <- Redeux.dupEnv
    void . liftIO . forkIO . env $ do
      interpreter command
      Redeux.revalidateUI
    next

add :: Text -> Redeux.Command Action ()
add = liftF . flip Add ()

edit :: Maybe Int -> Redeux.Command Action ()
edit = liftF . flip Edit ()

delete :: Int -> Redeux.Command Action ()
delete = liftF . flip Delete ()

update :: Int -> Text -> Redeux.Command Action ()
update target todo = liftF $ Update target todo ()

toggleComplete :: Int -> Redeux.Command Action ()
toggleComplete = liftF . flip ToggleComplete ()

setFilter :: Filter -> Redeux.Command Action ()
setFilter = liftF . flip SetFilter ()

async :: Redeux.Command Action () -> Redeux.Command Action ()
async = liftF . flip Async ()

    
todoMain :: Todo -> DOM Action ()
todoMain state@Todo{..} = do
  section_ [class_ "todoapp"] $ do
    header_ [class_ "header"] $ do
      h1_ [] "todos"
      input_ [ class_ "new-todo", placeholder_ "What needs to be done?"
             , onKeyUp_ $ {-do
                traceM $ DOM.key event
                when (DOM.key event == "13") $-} add "_placeholder_"
             ]
    when hasTodos $ do
      section_ [class_ "main"] $ do
        input_ [ class_ "toggle-all", type_ "checkbox"
               , onClick_ $ mapM_ toggleComplete [0..length todos - 1]
               ]
        label_ [attr_ "for" "toggle-all"] "Mark all as complete"
        ul_ [class_ "todo-list"] $ todoList state
    when hasTodos $ todoFooter state
  footer_ [class_ "info"] $ do
      p_ [] "Double-click to edit a todo"
  where
  hasTodos = not $ null todos

todoList :: Todo -> DOM Action ()
todoList Todo{..} =
  foldMap (mkTodo editing) . zip [0..] $ filter (filters currentFilter) todos

filters :: Filter -> (Text, Bool) -> Bool
filters f (_, complete) = case f of
  All -> True
  Complete -> complete
  Active -> not complete

mkTodo :: Maybe Int -> (Int, (Text, Bool)) -> DOM Action ()
mkTodo editIndex (index, (name, completed)) =
  li_ [classes_ $ todoClasses (Just index == editIndex) completed] $ do
    div_ [class_ "view"] $ do
      input_ [ class_ "toggle", type_ "checkbox", checked_ completed
             , onClick_ $ toggleComplete index
             ]
      label_ [onDoubleClick_ . edit $ Just index] $ text_ name
      button_ [class_ "destroy", onClick_ $ delete index] ""
    input_ [ class_ "edit", value_ name
           , onBlur . const $ do
              update index "_placeholder_"
              edit Nothing
           ]

todoClasses :: Bool -> Bool -> [Text]
todoClasses editing' completed =
  when' completed ["completed"] <> when' editing' ["editing"]

      
todoFooter :: Todo -> DOM Action ()
todoFooter Todo{..} =
  footer_ [class_ "footer"] $ do
    todoCount (length todos)
    -- Remove this if you don't implement routing
    ul_ [class_ "filters"] $
      mapM_ (li_ [])
        [ a_ [class_ "selected", href_ "#/", onClick_ $ setFilter All] "All"
        , a_ [class_ "selected", href_ "#/active", onClick_ $ setFilter Active] "Active"
        , a_ [class_ "selected", href_ "#/completed", onClick_ $ setFilter Complete] "Completed"
        ]
    when (any snd todos) $ do
      button_ [ class_ "clear-completed"
              , onClick_ . mapM_ (\x -> when (snd (snd x)) $ delete (fst x))
                         . reverse $ zip [0..] todos
              ] "Clear completed"
    
todoCount :: Int -> DOM Action ()
todoCount total =
  span_ [class_ "todo-count"] $ do
    strong_ [] . fromString $ show total
    " item" <> pluralize <> " left"
  where
  pluralize = case total of
                1 -> ""
                _ -> "s"

when' :: Monoid a => Bool -> a -> a
when' b a = if b then a else mempty
