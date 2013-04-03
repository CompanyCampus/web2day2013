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

    forM_ langs $ makeIndexPage "events" "event"

    forM_ langs $ \lang -> (makeElementsWithContext (completeEventContext lang) "events" "event" lang)

--------------------------------------------------------------------------------
-- Topics
--

    forM_ langs $ makeIndexPage "topics" "topic"

    forM_ langs $ \lang -> (makeElementsWithContext (completeTopicContext lang) "topics" "topic" lang)

--------------------------------------------------------------------------------
-- Calendar
--
    forM_ langs makeCalendar


--------------------------------------------------------------------------------
-- Speakers
--

    forM_ langs $ makeIndexPage "speakers" "speaker"

    forM_ langs $ \lang -> (makeElementsWithContext (completeSpeakerContext lang) "speakers" "speaker" lang)


--------------------------------------------------------------------------------
-- Posts
--

    forM_ langs $ makeIndexPage "posts" "post"

    forM_ langs $ makeElements  "posts" "post"

--------------------------------------------------------------------------------
-- Contributors
--

    forM_ langs $ makeIndexPage "contributors" "contributor"

    forM_ langs $ makeElements  "contributors" "contributor"


--------------------------------------------------------------------------------
-- Partners
--

    forM_ langs $ makeIndexPage "partners" "partner"

    forM_ langs $ makeElements  "partners" "partner"


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
