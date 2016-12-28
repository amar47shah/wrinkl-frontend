{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import Data.Monoid ((<>))
import Data.Text (Text)

main = mainWidget $ el "div" $ do
  rec text <- textInput $
        def { _textInputConfig_setValue = const "" <$> submit }
      submit <- button "submit"
  clear <- button "clear"
  let submitEvent = tagPromptlyDyn (Just <$> value text) submit
  let clearEvent = const Nothing <$> clear
  sentence <- foldDyn toSentence "" $ leftmost [submitEvent, clearEvent]
  dynText sentence

toSentence :: Maybe Text -> Text -> Text
toSentence mw s =
  maybe "" (\w -> s <> " " <> w) mw
