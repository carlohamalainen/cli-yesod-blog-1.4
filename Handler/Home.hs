module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import qualified Data.Text as DT
import qualified System.FilePath.Posix as FP

import Data.Maybe (fromJust)
import Text.Printf
import Yesod.ReCAPTCHA

cu :: FP.FilePath -> FP.FilePath -> FP.FilePath
cu x y = FP.dropTrailingPathSeparator $ (FP.dropTrailingPathSeparator x) FP.</> y

baseUrl = (appBaseUrl . appSettings) <$> getYesod

commentForm :: EntryId -> Form Comment
commentForm entryId = renderDivs $ Comment
    <$> pure entryId
    <*> lift (liftIO getCurrentTime)
    <*> areq textField (fieldSettingsLabel MsgCommentName) Nothing
    <*> aopt emailField (fieldSettingsLabel MsgCommentEmail) Nothing
    <*> aopt urlField (fieldSettingsLabel MsgCommentUrl) Nothing
    <*> areq htmlField (fieldSettingsLabel MsgCommentText) Nothing
    <*> pure False <* recaptchaAForm

getHomeR :: Handler RepHtml
getHomeR = do
    master <- getYesod

    entries <- runDB $ selectList [EntryVisible ==. True] [ Desc EntryPostedYear
                                                          , Desc EntryPostedMonth
                                                          , Desc EntryPostedDay
                                                          ]

    let entriesAsTuples = map deconstructEntryEntity entries

    let url = DT.unpack $ appRoot $ appSettings master

    base <- DT.unpack <$> baseUrl

    let feedUrl = url `cu` base `cu` "feed"

    defaultLayout $ do
        setTitleI MsgWelcomeHomepage
        [whamlet|

<h1><a href=#{url}>_{MsgBlogTitle}</a>
<hr>
$if null entries
    <p>_{MsgNoEntries}
$else
    <ul>
        $forall (title, mashedTitle, year, month, mm, day, dd, content, visible) <- entriesAsTuples
            <li> <a href=@{EntryLongR year month day mashedTitle}>#{year}-#{mm}-#{dd} #{title}</a>

<p> <a href="#{feedUrl}">Posts: RSS</a>

|]

    -- This deconstruction to a tuple is a bit clunky, but I can't work out how to put
    -- the printf into the #{mm} in the hamlet.
    where deconstructEntryEntity (Entity _ (Entry title mashedTitle year month day content visible)) = (title, mashedTitle, year, month, printf "%02d" month :: String, day, printf "%02d" day :: String, content, visible)

getEntryLongR :: Int -> Int -> Int -> Text -> Handler RepHtml
getEntryLongR year month day mashedTitle = do
    e <- runDB $ getBy $ EntryYMDMashed year month day mashedTitle

    url <- (DT.unpack . appRoot . appSettings) <$> getYesod

    case e of (Just (Entity eid (Entry title' mashedTitle' year' month' day' content' True)))     -> do comments <- runDB $ selectList [CommentEntry ==. eid, CommentVisible ==. True] [Asc CommentPosted]

                                                                                                        maxNrComments <- (appMaxNrComments . appSettings) <$> getYesod
                                                                                                        let commentsOpen = length comments < maxNrComments
                                                                                                        base <- DT.unpack <$> baseUrl

                                                                                                        (commentWidget, enctype) <- generateFormPost (commentForm eid)

                                                                                                        let dateString = printf "%04d-%02d-%02d" year' month' day' :: String

                                                                                                        defaultLayout $ do
                                                                                                            setTitleI title'
                                                                                                            [whamlet|
<p align="right"><h1><a href=#{cu url base}>_{MsgBlogTitle}</a>
<hr>
<h1>#{title'}
<h3>#{dateString}
<article>#{content'}
    <section .comments>
        <div id="comments"></div>
        <h2>_{MsgCommentsHeading}
        <br>
        $if null comments
            <p>_{MsgNoComments}
        $else
            $forall Comment _entry posted name email url text visible <- map entityVal comments
                <hr>
                $if isNothing url
                    <h3>#{name}
                $else
                    <h3><a href=#{fromJust url}>#{name}</a>
                <h4>#{show posted}
                <p>#{toHtml text}

        $if commentsOpen
            <section>
                <h1>_{MsgAddCommentHeading}

                <form method=post enctype=#{enctype}>
                    ^{commentWidget}
                    <div>
                        <input type=submit value=_{MsgAddCommentButton}>
        $else
            <p> Comments are closed.
|]
              _                                                                            -> notFound

{-
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")
-}

getFeedR :: Handler RepXml
getFeedR = undefined

{-
postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing
-}

postEntryLongR :: Int -> Int -> Int -> Text -> Handler RepHtml
postEntryLongR = undefined

