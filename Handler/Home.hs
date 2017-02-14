{-# LANGUAGE ExistentialQuantification #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))


-- Data type for formA. 
data FormDataA = FormDataA Text

-- Data type for formB. 
data FormDataB = FormDataB Int

-- Define formA.
formA :: Form FormDataA
formA = identifyForm "form-a" $ renderBootstrap3 BootstrapBasicForm $ FormDataA
    <$> areq textField "Some text" Nothing

-- Define formB.
formB :: Form FormDataB
formB = identifyForm "form-b" $ renderBootstrap3 BootstrapBasicForm $ FormDataB
    <$> areq intField "A number" Nothing

getHomeR :: Handler Html
getHomeR = do
    (wA, eA) <- generateFormPost formA
    (wB, eB) <- generateFormPost formB
    defaultLayout $ do
        [whamlet|
            <form .form-basic role=form method=post action="@{HomeR}" enctype=#{eA}>
                ^{wA}
                <div .form-group>
                    <button .btn .btn-primary .btn-lg .btn-block type="submit">
                        Submit

            <form .form-basic role=form method=post action="@{HomeR}" enctype=#{eB}>
                ^{wB}
                <div .form-group>
                    <button .btn .btn-primary .btn-lg .btn-block type="submit">
                        Submit
        |]

postHomeR :: Handler Html
postHomeR = defaultLayout $
    runMultipleFormsPost [
        FormAndHandler formA formHandlerA
      , FormAndHandler formB formHandlerB
      ]

    where
        formHandlerA FormMissing = error "unreachable"
        formHandlerA (FormFailure _) = error "failureA"
        formHandlerA (FormSuccess _) = error "successA"

        formHandlerB FormMissing = error "unreachable"
        formHandlerB (FormFailure _) = error "failureB"
        formHandlerB (FormSuccess _) = error "successB"

data FormAndHandler = forall a . FormAndHandler (Form a) (FormResult a -> Widget -> Enctype -> Widget)

runMultipleFormsPost :: [FormAndHandler] -> Widget
runMultipleFormsPost [] = return ()
runMultipleFormsPost ((FormAndHandler form handler):t) = do
    ((res, widget), enctype) <- handlerToWidget $ runFormPost form
    case res of
        FormMissing ->
            runMultipleFormsPost t
        _ ->
            handler res widget enctype
