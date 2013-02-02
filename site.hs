--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend, mempty)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

--------------------------------------------------------------------------------
-- Assets
    match "assets/images/*" $ do
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
           -- >>=
--           return . fmap copyFileCompiler

--------------------------------------------------------------------------------
-- JS
--
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

--------------------------------------------------------------------------------
-- Reusable blocks
--
    match "fr/blocks/*" $ do
        compile $ getResourceBody

    match "en/blocks/*" $ do
        compile $ getResourceBody

--------------------------------------------------------------------------------
-- Events
--

    match "fr/events/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/event.html"   (globalContext "fr")
            >>= loadAndApplyTemplate "templates/default.html" (globalContext "fr")
            >>= relativizeUrls

    match "fr/speakers/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/speaker.html"   (globalContext "fr")
            >>= loadAndApplyTemplate "templates/default.html" (globalContext "fr")
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" (globalContext "fr")
            >>= relativizeUrls

    match "templates/**" $ compile templateCompiler

getBlock :: String -> String -> (Context String) -> Compiler String
getBlock lang name ctx = let
        blockContext = ctx `mappend` defaultContext
    in do
    tpl <- loadBody $ fromFilePath ("templates/blocks/"++ name ++".html")
    content <- load $ fromFilePath (lang ++ "/blocks/"++ name ++".html")
    compiledBlock <- applyTemplate tpl blockContext content
    return $ itemBody compiledBlock

-- getBlockInContext :: String -> Context String -> Context String
-- getBlockInContext lang name ctx =
--     field name (\_ -> getBlock lang name ctx)

blockLoader :: String -> Context String
blockLoader lang =
    functionField "block" (\args item -> getBlock lang (head args) mempty)

globalContext lang = blockLoader lang `mappend` defaultContext
