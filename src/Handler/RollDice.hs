{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.RollDice where

import Import
import System.Random
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

-- Define our data that will be used for creating the form.
data RollForm = RollForm
    { sides :: Int
    , gen :: Int
    }

roll :: RollForm -> RollForm
roll (RollForm n g) = RollForm (fst (rand n g)) g

rand :: Int -> Int -> (Int, StdGen)
rand n g = randomR (1, n) (mkStdGen g)

-- The GET handler displays the form
getRollDiceR :: Handler Html
getRollDiceR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe RollForm
        handlerName = "getRollDiceR" :: Text
    defaultLayout $ do
        setTitle "Roll Dice"
        $(widgetFile "rollDice")     

postRollDiceR :: Handler Html
postRollDiceR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postRollDiceR" :: Text
        submission = case result of
            FormSuccess res -> Just (roll res)
            _ -> Nothing

    defaultLayout $ do
        setTitle "Roll Dice"
        $(widgetFile "rollDice")        

sampleForm :: Form RollForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ RollForm
    <$> areq intField (textSettings "Enter a number of sides") Nothing
    <*> lift (liftIO randomIO)
    -- Add attributes like the placeholder and CSS classes.
    where textSettings label = FieldSettings
            { fsLabel = label
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "Number of sides")
                ]
            }        