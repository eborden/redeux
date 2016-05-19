{-# LANGUAGE LambdaCase, OverloadedStrings, GeneralizedNewtypeDeriving, TypeFamilies, RecordWildCards, QuasiQuotes #-}
module Redeux.DOM.Core where

import Data.IORef
import Data.String
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Writer
import Data.Functor.Identity

import qualified Data.JSString as JS
import           GHCJS.Marshal.Pure
import           GHCJS.Foreign.QQ

import qualified GHCJS.VDOM.Attribute      as A
import qualified GHCJS.VDOM.Element        as E
import qualified GHCJS.VDOM.Event          as Ev
import           GHCJS.VDOM

import Redeux.Core (Command, Sink)

data Chunk grammer
  = Empty
  | Singleton (Elem grammer)
  | Siblings [Chunk grammer]
  | TextNode Text
  deriving (Show)

instance Monoid (Chunk g) where
  mempty = Empty
  mappend Empty         x             = x
  mappend x             Empty         = x
  mappend (Singleton x) (Singleton y) = Siblings [Singleton x, Singleton y]
  mappend (TextNode x)  (Singleton y) = Siblings [TextNode x,  Singleton y]
  mappend (Singleton x) (TextNode y)  = Siblings [Singleton x, TextNode y]
  mappend (TextNode x)  (TextNode y)  = Siblings [TextNode x,  TextNode y]
  mappend (Siblings xs) (Siblings ys) = Siblings (xs <> ys)
  mappend (Siblings xs) x             = Siblings (xs <> [x])
  mappend x             (Siblings xs) = Siblings (x:xs)

instance IsString (Chunk g) where
  fromString = TextNode . Text.pack

newtype DOM g a = DOM {unDOM :: Writer (Chunk g) a}
  deriving (Functor, Applicative, Monad)

instance (a ~ ()) => IsString (DOM g a) where
  fromString s = dom () . TextNode $ Text.pack s

instance (a ~ ()) => Monoid (DOM g a) where
  mempty = dom () mempty
  mappend a b = let (r, c) = run b
                in dom r $ exec a <> c

dom :: a -> Chunk g -> DOM g a
dom a c = DOM . WriterT $ Identity (a, c)

exec :: DOM g () -> Chunk g
exec c = execWriter $ unDOM c

run :: DOM g a -> (a, Chunk g)
run c = runWriter $ unDOM c

data Elem grammer
  = Elem
  { tag        :: Text
  , attributes :: [(Text, Text)]
  , handlers   :: [Handler grammer]
  , children   :: [Chunk grammer]
  } deriving (Show)

data AttrOrHandler grammer
  = OrHandler (Handler grammer)
  | OrAttr (Text, Text)
  | OrEmpty

reactAttr :: [AttrOrHandler grammer] -> [(Text, Text)]
reactAttr = concatMap (\case
  OrHandler _ -> []
  OrEmpty -> []
  OrAttr x -> [x])

reactHandler :: [AttrOrHandler grammer] -> [Handler grammer]
reactHandler = concatMap (\case
  OrAttr _ -> []
  OrEmpty -> []
  OrHandler x -> [x])

data Handler grammer
  = Handler
  { eventType :: EventType
  , callback  :: EventData -> Command grammer ()
  }

instance Show (Handler g) where
  show x = show $ eventType x

data EventData
  = KeyboardEvent Ev.KeyboardEvent
  | FocusEvent Ev.FocusEvent
  | MouseEvent Ev.MouseEvent
                
data EventType
  = Click | DoubleClick | MouseOver | MouseOut
  | KeyUp | KeyDown
  | Focus | Blur
  deriving (Show)

key :: EventData -> String
key (KeyboardEvent ev) = show $ Ev.key ev
key _ = "Dead"

el_ :: Text -> [AttrOrHandler grammer] -> DOM grammer a -> DOM grammer a
el_ tagStr attrs childr = do
  let (x, childr') = run childr
  dom x . Singleton $ Elem { tag = tagStr
                              , attributes = reactAttr attrs
                              , handlers = reactHandler attrs
                              , children = [childr']
                              } 

text :: Text -> DOM g ()
text str = dom () $ TextNode str


chunkToVdom :: Sink grammer -> Chunk grammer -> [VNode]
chunkToVdom sink = \case
  Empty -> []
  Singleton el -> [elToVdom sink el]
  Siblings chunks -> concatMap (chunkToVdom sink) chunks
  TextNode t -> [fromString $ Text.unpack t]

elToVdom :: Sink grammer -> Elem grammer -> VNode
elToVdom sink Elem{..} = E.custom (JS.pack $ Text.unpack tag)
                                  (fmap (eventToVdom sink) handlers ++ fmap attrToVdom attributes)
                                  (concatMap (chunkToVdom sink) children)

attrToVdom :: (Text, Text) -> A.Attribute
attrToVdom ("class", val) = A.class_ . JS.pack $ Text.unpack val
attrToVdom (name, val) = A.Attribute (JS.pack $ Text.unpack name) (pToJSVal . JS.pack $ Text.unpack val)

eventToVdom :: Sink grammer -> Handler grammer -> A.Attribute
eventToVdom sink Handler{..} = case eventType of
  Click -> Ev.click (\e -> sink $ callback (MouseEvent e))
  DoubleClick -> Ev.dblclick (\e -> sink $ callback (MouseEvent e))
  MouseOut -> Ev.mouseout (\e -> sink $ callback (MouseEvent e))
  MouseOver -> Ev.mouseover (\e -> sink $ callback (MouseEvent e))
  KeyUp -> Ev.keyup (\e -> sink $ callback (KeyboardEvent e))
  KeyDown -> Ev.keydown (\e -> sink $ callback (KeyboardEvent e))
  Focus -> Ev.focus (\e -> sink $ callback (FocusEvent e))
  Blur -> Ev.blur (\e -> sink $ callback (FocusEvent e))

inject :: Show state
       => IORef (Maybe VMount)
       -> (state -> DOM grammer ())
       -> Sink grammer -> state -> IO ()
inject ref interface sink state = do
  let vdom = E.custom "div" () . chunkToVdom sink
           . exec $ interface state
  readIORef ref >>= \case
    Just m -> void $ diff m vdom >>= patch m 
    Nothing -> do
      m <- initVdom vdom
      writeIORef ref $ Just m
  

initVdom :: VNode -> IO VMount
initVdom vdom = do
  Ev.initEventDelegation Ev.defaultEvents
  body <- [js| document.body |]
  mount body vdom
