--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)
import           Data.Monoid         (mappend, mempty)
import           Data.Maybe          (catMaybes, fromMaybe, listToMaybe, maybeToList)
import qualified Data.Map            as M

import           Utils
import           RouteFactories
import           LinkedCompilers

import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

--------------------------------------------------------------------------------
-- Assets
    match "assets/images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/font/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*.css" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*.less" $ do
       route   $ setExtension "css"
       compile $ getResourceString >>=
           withItemBody (unixFilter "lessc" ["-","--yui-compress","-O2"])

--------------------------------------------------------------------------------
-- JS
--
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    makeImagesdata
--------------------------------------------------------------------------------
-- Reusable blocks
--

    forM_ langs $ \lang ->
        match (fromGlob $ lang ++ "/blocks/*.md") $ do
        compile $ pandocCompiler

    forM_ langs $ \lang ->
        match (fromGlob $ lang ++ "/blocks/*.html") $ do
        compile $ getResourceBody

--------------------------------------------------------------------------------
-- Events
--

    forM_ langs $ \lang -> (makeIndexPage lang "events" "event")

    forM_ langs $ \lang -> (makeElementsWithContext (completeEventContext lang) lang "events" "event")

--------------------------------------------------------------------------------
-- Topics
--

    forM_ langs $ \lang -> (makeIndexPage lang "topics" "topic")

    forM_ langs $ \lang -> (makeElementsWithContext (completeTopicContext lang) lang "topics" "topic")

--------------------------------------------------------------------------------
-- Calendar
--
    forM_ langs makeCalendar


--------------------------------------------------------------------------------
-- Speakers
--

    forM_ langs $ \lang -> (makeIndexPage lang "speakers" "speaker")

    forM_ langs $ \lang -> (makeElementsWithContext (completeSpeakerContext lang) lang "speakers" "speaker")


--------------------------------------------------------------------------------
-- Posts
--

    forM_ langs $ \lang -> (makeIndexPage lang "posts" "post")

    forM_ langs $ \lang -> (makeElements lang "posts" "post")

--------------------------------------------------------------------------------
-- Contributors
--

    forM_ langs $ \lang -> (makeIndexPage lang "contributors" "contributor")

    forM_ langs $ \lang -> (makeElements lang "contributors" "contributor")


--------------------------------------------------------------------------------
-- Partners
--

    forM_ langs $ \lang -> (makeIndexPage lang "partners" "partner")

    forM_ langs $ \lang -> (makeElements lang "partners" "partner")


--------------------------------------------------------------------------------
-- Simple pages
--
    forM_ langs makeSinglePages


--------------------------------------------------------------------------------
-- Index
--
    forM_ langs $ \lang ->
        match (fromGlob $ lang ++ "/index.html") $ do
            route langRoute
            compile $ getResourceBody
                >>= loadAndApplyTemplate "templates/index.html" (globalContext lang)
                >>= loadAndApplyTemplate "templates/default.html" (globalContext lang)
                >>= relativizeUrls

--------------------------------------------------------------------------------
-- Compile all templates
--
    match "templates/**" $ compile templateCompiler

--------------------------------------------------------------------------------
