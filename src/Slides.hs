{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
module Slides (withSlides) where

import Yesod
import qualified Text.Blaze.Html5 as H (h1, ul, li)
--import Language.Haskell.TH.Quote (QuasiQuoter(..))
--import Language.Haskell.TH (pprint, runQ)

data Slides = Slides

mkYesod "Slides" [parseRoutes|
/              RootR          GET
/main          Main           GET
/how           How            GET
/hamlet        Hamlet         GET
/hamletvsblaze HamletVsBlaze  GET
/cassius       Cassius        GET
/julius        Julius         GET
|]

instance Yesod Slides where
    approot _ = ""

    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            widget
            addCassius [cassius|
                body
                    font-family: sans-serif
                    font-size: 30px
                    height: 100%
                h1
                    font-size: 40px
                h2
                    font-size: 30px
                ul
                    margin-bottom: 5px
                li li
                    font-size: 25px
                |]
        hamletToRepHtml [hamlet|
            <html
                <head
                    <title>#{pageTitle pc}
                    ^{pageHead pc}
                <body
                    $maybe msg <- mmsg
                        <div #message>#{msg}
                    <div style="margin:0 auto; width:600px;"
                        ^{pageBody pc}
            |]

type Handler = GHandler Slides Slides

getRootR :: Handler RepHtml
getRootR = do
    defaultLayout $ do
        setTitle "Yesod Web Framework"
        nextPage Main
        [hamlet|
            <h1>Quick Tour to Yesod
            <h2>by Hamish Mackenzie
            <p>
                These are the slides I used in my presentation #
                to the Wellington FP Users group.  After each slide #
                I showed the code for the slide and discussed how it #
                works.
            <p>
                The talk covered the parts of Yesod that were of particular #
                interest to me.
            |]

getMain :: Handler RepHtml
getMain = defaultLayout $ do
    setTitle "Yesod Web Framework"
    nextPage How
    [hamlet|
        <h1>Yesod Web Framwork
        <ul
            <li>Developers
                <ul
                    <li>Michael Snoyman
                    <li>Matt Brown
                    <li>Greg Weber
            <li>Goals
                <ul
                    <li>type-safe
                    <li>secure
                    <li>RESTful
                    <li>fast
        |]

getHow :: Handler RepHtml
getHow = defaultLayout $ do
    setTitle "Yesod Web Framework - How"
    nextPage Hamlet
    [hamlet|
        <h1>How
        <ul
            <li>Domain Specific Languages
            <li>Quasy Quoting
            <li>Enumerators
            <li>And much more
    |]

getHamlet :: Handler RepHtml
getHamlet = defaultLayout $ do
    setTitle "Yesod Web Framework - Hamlet"
    nextPage HamletVsBlaze
    [hamlet|
        <h1>Hamlet DSL for HTML
        <ul
            <li>Layout sensitive <b>user</b> friendly syntax
            <li>Compiled or Interpreted
            <li>Uses Blaze Builder
            #{hamletFunction}
            #{blazeFunction}

            $forall p1 <- listOfPoints
                #{H.li p1}

            $if True
                <li>If then else
            $else
                <li>No if then else

            $maybe p2 <- maybePoint
                #{H.li p2}
            $nothing
                <li>No point
        |]
  where
    hamletFunction, blazeFunction :: Html
    hamletFunction = [hamlet|<li>Call Hamlet functions|]
    blazeFunction = H.li "Call Blaze functions"
    listOfPoints = ["Map lists"]
    maybePoint = Just "Evaluate Maybe types"

-- This is how the quasy quoting works
{-
demoQuasyQuoting :: IO ()
demoQuasyQuoting = do
    test <- runQ (quoteExp hamlet "<h1>#{x}")
    putStrLn (pprint test)
-}

-- Output is verbose because of namespaces but it is basically
-- toHamletValue $ do
--    (htmlToHamletMonad . preEscapedString) "<h1>"
--    (htmlToHamletMonad . toHtml) x
--    (htmlToHamletMonad . preEscapedString) "</h1>"

getHamletVsBlaze :: Handler RepHtml
getHamletVsBlaze = defaultLayout $ do
    setTitle "Yesod Web Framework - Hamlet vs Blaze"
    nextPage Cassius
    [hamlet|
        #{hamletPros}
        #{blazePros}|]

hamletPros :: Html
hamletPros = [hamlet|
    <h1>Hamlet (DSL)
    <ul
        <li>Optimized syntax
        <li>No namespace issues
        <li>Looks like HTML
    |]

blazePros :: Html
blazePros = do
    H.h1 "Blaze (Embedded DSL)"
    H.ul $ do
        H.li "Better GHC error messages"
        H.li "Haskell syntax highlighting"
        H.li "Haskell text completion & metadata"
        H.li "Easier to mix with Haskell (it is Haskell)"

getCassius :: Handler RepHtml
getCassius = defaultLayout $ do
    setTitle "Yesod Web Framework - Cassius"
    nextPage Julius
    myId <- lift newIdent
    [hamlet|
        <h1>Cassius DSL for CSS
        <ul
            <li>Mainly CSS with Embedded Haskell
            <li>Use #
                <span .someClass>Class
                \ or #
                <span #someId>ID
            <li>Use an ID provided by Yesod #
                <span ##{myId}>(like say "#{myId}")
        |]
    addCassius [cassius|
        .someClass, #someId
            color: #090
        ##{myId}
            font-size: 20px
            color: #009
        |]

getJulius :: Handler RepHtml
getJulius = defaultLayout $ do
    setTitle "Yesod Web Framework - Julius"
    nextPage RootR
    [hamlet|
        <h1>Julius DSL for JavaScript
        <ul
            <li>Mainly JavaScript with Embedded Haskell
            <li>This is how nextPage works
        |]

nextPage :: Monad m => Route master -> GGWidget sub master m ()
nextPage next = do
    addJulius [julius|
            document.onclick = function(){
                 window.location.href="@{next}";
            }
        |]

withSlides :: (Application -> IO a) -> IO a
withSlides f = toWaiApp Slides >>= f
