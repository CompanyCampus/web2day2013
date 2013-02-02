--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend)
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
-- Homepage blocks
--
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
           sharebloc <- loadAndApplyTemplate "templates/bloc.html" defaultContext "blocs/share.md"
           indexcontent <- load "index.html"
           defaultTemplate <- loadBody "templates/default.html"
           content <- applyTemplate defaultTemplate (constField "share" sharebloc `mappend` defaultContext) indexcontent
           return content

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
-- Compilator
--
   match "blocs/*.md" $ do
      compile pandocCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


--------------------------------------------------------------------------------
postList :: ([Item String] -> [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter <$> loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list
