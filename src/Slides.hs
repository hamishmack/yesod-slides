{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Slides (withSlides, helloString, validatePrime) where

import Yesod.Core
import Yesod.Helpers.Static
import Network.Wai (Application)
import Text.Hamlet (hamlet, Html)
import Text.Cassius (cassius)
import Text.Julius (julius)
import qualified Text.Blaze.Html5 as H (h1, ul, li)
import Text.Blaze.Renderer.String (renderHtml)
--import Language.Haskell.TH.Quote (QuasiQuoter(..))
--import Language.Haskell.TH (pprint, runQ)
import System.FilePath ((</>))
import Control.Monad.Trans.Class (lift)

data Slides = Slides {
    jsexeStatic :: Static
    }

-- This example uses GHC 7 syntax.  GHC 6 users should replace
-- [parseRoutes| with [$parseRoutes|
-- [hamlet|      with [$hamlet|
-- [cassius|     with [$cassius|
-- [julius|      with [$julius|
mkYesod "Slides" [parseRoutes|
    /              RootR          GET
    /main          Main           GET
    /how           How            GET
    /hamlet        Hamlet         GET
    /hamletvsblaze HamletVsBlaze  GET
    /cassius       Cassius        GET
    /julius        Julius         GET
    /ghcjs         GhcJs          GET
    /jsexe         Jsexe          Static    jsexeStatic    
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
                input
                    font-size: 30px
                |]
        hamletToRepHtml [hamlet|
            <html>
                <head>
                    <script type="text/javascript" src="jsexe/rts-common.js">
                    <script type="text/javascript" src="jsexe/rts-trampoline.js">
                    <title>#{pageTitle pc}
                    ^{pageHead pc}
                <body #slideBody>
                    $maybe msg <- mmsg
                        <div #message>#{msg}
                    <div style="margin:0 auto; width:600px;">
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
        <ul>
            <li>Developers
                <ul>
                    <li>Michael Snoyman
                    <li>Matt Brown
                    <li>Greg Weber
            <li>Goals
                <ul>
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
        <ul>
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
        <ul>
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
    <ul>
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
        <ul>
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
    nextPage GhcJs
    [hamlet|
        <h1>Julius DSL for JavaScript
        <ul>
            <li>Mainly JavaScript with Embedded Haskell
            <li>This is how nextPage works
        |]

nextPage :: Monad m => Route master -> GGWidget master m ()
nextPage next = do
    addJulius [julius|
            document.onclick = function(){
                 window.location.href="@{next}";
            }
        |]

hello :: String -> Html
hello who = [hamlet|Hello from <b>#{who}</b>|]

helloString :: String -> String
helloString = renderHtml . hello

isPrime :: Integral a => a -> Bool
isPrime p = p > 1 && (all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n*n <= p) [2..])

validatePrime :: Int -> String
validatePrime p | isPrime p = renderHtml [hamlet|<b>Yes</b>, #{p} is a prime|]
                | otherwise = renderHtml [hamlet|<b>No</b>, #{p} is not a prime|]

getGhcJs :: Handler RepHtml
getGhcJs = defaultLayout $ do
    setTitle "Yesod Web Framework - GHC JavaScript"
    -- Not sure how to disable onclick for all but the input box
    -- nextPage RootR
    helloId <- lift newIdent
    textIn <- lift newIdent
    message <- lift newIdent
    [hamlet|
        <h1>Haskell on client using GHCJS
        Thanks to Victor Nazarov we can...
        <ul>
            <li>Run function on both server and client
                <ul>
                    <li>#{hello "Server"}
                    <li ##{helloId}>
            <li>
                Use Haskell for client side validation
                <input ##{textIn} size="8" oninput="textChanged()" onchange="textChanged()">
                <span ##{message}>Enter a prime number
        <div #log>
        |]
    addJulius [julius|
            window.onload = function(){
                $hs.loadPaths = ["jsexe"];

                // Used packages
                $hs.packages = [""];

                // Must be called first
                $hs.init();
                
                $hs.loadModule("GHC.Base");
                $hs.loadModule("Slides");
                
                #{helloId}.innerHTML = $hs.fromHaskellString(
                    $hs.force( $hs.modules.Slides.hs_helloString,
                        $hs.force( $hs.modules.GHCziBase.hs_unpackCStringzh, "Client\x00")));
            }
            function textChanged() {
                #{message}.innerHTML = $hs.fromHaskellString(
                    $hs.force( $hs.modules.Slides.hs_validatePrime,
                      $hs.toHaskellInt(#{textIn}.value)));
            }
        |]

withSlides :: FilePath -> (Application -> IO a) -> IO a
withSlides binDir f =
    toWaiApp Slides {
          jsexeStatic = static $ binDir </> "yesod-slides.jsexe"
        } >>= f
