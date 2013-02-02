--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)
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
        compile compressCssCompiler

    match "assets/css/*.less" $ do
       route   $ setExtension "css"
       compile $ getResourceString >>=
           withItemBody (unixFilter "lessc" ["-"]) >>=
           return . fmap compressCss

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
        match (fromGlob (lang ++ "/blocks/*.md")) $ do
            compile $ pandocCompiler

    forM_ ["en", "fr"] $ \lang ->
        match (fromGlob (lang ++ "/blocks/*.html")) $ do
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

    match "index.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" (globalContext "fr")
            >>= relativizeUrls

    match "templates/**" $ compile templateCompiler

--------------------------------------------------------------------------------
-- Compilator
--
    match "blocs/*.md" $ do
        compile pandocCompiler

--------------------------------------------------------------------------------

getBlock :: String -> [String] -> (Context String) -> Compiler String
getBlock lang args ctx = let
        blockContext = ctx `mappend` defaultContext
        [name, fmt] = args
    in do
    tpl <- loadBody $ fromFilePath ("templates/blocks/"++ name ++".html")
    content <- load $ fromFilePath (lang ++ "/blocks/"++ name ++ "." ++ fmt)
    debugCompiler (lang ++ "/blocks/" ++ name)
    compiledBlock <- applyTemplate tpl blockContext content
    return $ itemBody compiledBlock

-- getBlockInContext :: String -> Context String -> Context String
-- getBlockInContext lang name ctx =
--     field name (\_ -> getBlock lang name ctx)

blockLoader :: String -> Context String
blockLoader lang =
    functionField "block" (\args item -> getBlock lang args mempty)

globalContext lang = blockLoader lang `mappend` defaultContext
