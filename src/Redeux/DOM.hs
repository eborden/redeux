{-# LANGUAGE OverloadedStrings #-}
module Redeux.DOM (module Redeux.DOM, DOM) where

import Redeux.Core
import Redeux.DOM.Core
import Data.Text (Text)
import qualified Data.Text as Text

div_, span_, p_, a_, h1_, h2_, h3_
  , section_, footer_, header_, strong_
  , label_, ul_, li_
  , button_  :: [AttrOrHandler g] -> DOM g a -> DOM g a
div_      = el_ "div"
section_  = el_ "section"
footer_   = el_ "footer"
header_   = el_ "header"
label_    = el_ "label"
span_     = el_ "span"
strong_   = el_ "strong"
button_   = el_ "button"
ul_       = el_ "ul"
li_       = el_ "li"
p_        = el_ "p"
a_        = el_ "a"
h1_       = el_ "h1"
h2_       = el_ "h2"
h3_       = el_ "h3"

input_  :: [AttrOrHandler g] -> DOM g ()
input_ a = el_ "input" a (pure ())

attr_ :: Text -> Text -> AttrOrHandler g
attr_ name value = OrAttr (name, value)

text_ :: Text -> DOM g ()
text_ = text

class_, placeholder_, type_, value_
  , href_ :: Text -> AttrOrHandler g
class_ = attr_ "class"
placeholder_ = attr_ "placeholder"
type_ = attr_ "type"
value_ = attr_ "value"
href_ = attr_ "href"

classes_ :: [Text] -> AttrOrHandler g
classes_ xs =
  let classes = Text.intercalate " " $ xs
  in if Text.null classes then OrEmpty
                          else class_ classes

checked_ :: Bool -> AttrOrHandler g
checked_ b =
  if b then attr_ "checked" "checked"
       else OrEmpty

on :: EventType -> (EventData -> Command grammer ()) -> AttrOrHandler grammer
on t f = OrHandler $ Handler { eventType = t
                             , callback  = f
                             }

onClick, onDoubleClick, onBlur, onKeyUp :: (EventData -> Command grammer ()) -> AttrOrHandler grammer
onClick f = on Click f
onDoubleClick f = on DoubleClick f
onBlur f = on Blur f
onKeyUp f = on KeyUp f

onClick_, onDoubleClick_, onBlur_, onKeyUp_ :: Command grammer () -> AttrOrHandler grammer
onClick_ = onClick . const
onDoubleClick_ = onDoubleClick . const
onBlur_ = onBlur . const
onKeyUp_ = onKeyUp . const
