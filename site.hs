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
           withItemBody (unixFilter "lessc" ["-"])
           >>= return . fmap id

--------------------------------------------------------------------------------
-- JS
--
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler
        
--------------------------------------------------------------------------------
-- Reusable blocks
--

    forM_ ["en", "fr"] $ \lang ->
        match (fromGlob $ lang ++ "/blocks/*.md") $ do
        compile $ pandocCompiler

    forM_ ["en", "fr"] $ \lang ->
        match (fromGlob $ lang ++ "/blocks/*.html") $ do
        compile $ getResourceBody

--------------------------------------------------------------------------------
-- Events
--

    forM_ ["en", "fr"] $ \lang -> (makeIndexPage lang "events" "event")

    forM_ ["en", "fr"] $ \lang -> (makeElementsWithContext (confSpeakersCtx lang) lang "events" "event")

--------------------------------------------------------------------------------
-- Calendar
--
    forM_ ["en", "fr"] makeCalendar


--------------------------------------------------------------------------------
-- Speakers
--

    forM_ ["en", "fr"] $ \lang -> (makeIndexPage lang "speakers" "speaker")

    forM_ ["en", "fr"] $ \lang -> (makeElementsWithContext (hisEventsCtx lang) lang "speakers" "speaker")


--------------------------------------------------------------------------------
-- Posts
--

    forM_ ["en", "fr"] $ \lang -> (makeIndexPage lang "posts" "post")

    forM_ ["en", "fr"] $ \lang -> (makeElements lang "posts" "post")

--------------------------------------------------------------------------------
-- Index
--
    forM_ ["en", "fr"] $ \lang ->
        match (fromGlob $ lang ++ "/index.html") $ do
            route idRoute
            compile $ getResourceBody
                >>= loadAndApplyTemplate "templates/index.html" (globalContext lang)
                >>= loadAndApplyTemplate "templates/default.html" (globalContext lang)
                >>= relativizeUrls

--------------------------------------------------------------------------------
-- Compile all templates
--
    match "templates/**" $ compile templateCompiler

--------------------------------------------------------------------------------

