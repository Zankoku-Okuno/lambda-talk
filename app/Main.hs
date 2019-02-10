module Main where

import Lambda

import Text.Read
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom


main :: IO ()
main = mainWidgetWithHead htmlhead $ do
    sourceEv <- source
    el "div" $
        dynText =<< holdDyn "" (T.pack . show <$> sourceEv)
    el "hr" $ pure ()
    el "div" $ do
        let redexesEv = redexes <$> sourceEv
        dynText =<< holdDyn "" (T.pack . show <$> redexesEv)


htmlhead :: forall t m. (MonadWidget t m) => m ()
htmlhead = do
    el "title" $ text "λ Debugger"

source :: MonadWidget t m => m (Event t Expr)
source = el "div" $ do
    ti <- el "div" $ textArea $ def
        { _textAreaConfig_attributes = constDyn $ mconcat
            [ "wrap" =: "off"
            , "rows" =: "10"
            , "cols" =: "80"
            ]
        , _textAreaConfig_initialValue = ""
        }
    go <- button "Go"
    let trySend = tagPromptlyDyn (_textArea_value ti) go
        exprEv = fmapMaybe (readMaybe . T.unpack) trySend
    pure exprEv