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
        compile copyFileCompiler

    match "assets/css/*.less" $ do
        route   idRoute
        compile copyFileCompiler
--       route   $ setExtension "css"
--       compile $ getResourceString >>=
--           withItemBody (unixFilter "lessc" ["-"])
--           >>= return . fmap copyFileCompiler

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

    forM_ ["en", "fr"] $ \lang -> (makeElements lang "events" "event")

    forM_ ["en", "fr"] $ \lang -> (makeIndexPage lang "events" "event")

--------------------------------------------------------------------------------
-- Speakers
--

    forM_ ["en", "fr"] $ \lang ->
        match (fromGlob $ lang ++ "/speakers/*.md") $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/speaker.html" (globalContext lang)
                >>= loadAndApplyTemplate "templates/default.html" (globalContext lang)
                >>= relativizeUrls

    forM_ ["en", "fr"] $ \lang -> (makeIndexPage lang "speakers" "speaker")

--------------------------------------------------------------------------------
-- Posts
--

    forM_ ["en", "fr"] $ \lang ->
        match (fromGlob $ lang ++ "/posts/*.md") $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"   (globalContext lang)
                >>= loadAndApplyTemplate "templates/default.html" (globalContext lang)
                >>= relativizeUrls

    forM_ ["en", "fr"] $ \lang -> (makeIndexPage lang "posts" "post")

--------------------------------------------------------------------------------
-- Index
--
    forM_ ["en", "fr"] $ \lang ->
        match (fromGlob $ lang ++ "/index.html") $ do
            route idRoute
            compile $ getResourceBody
                >>= loadAndApplyTemplate "templates/default.html" (globalContext lang)
                >>= relativizeUrls

--------------------------------------------------------------------------------
-- Compile all templates
--
    match "templates/**" $ compile templateCompiler

--------------------------------------------------------------------------------

getBlock :: String -> [String] -> (Context String) -> Compiler String
getBlock lang args ctx = let
        blockContext = ctx `mappend` defaultContext
        [name, fmt] = args
    in do
    tpl <- loadBody $ fromFilePath ("templates/blocks/"++ name ++".html")
    content <- load $ fromFilePath (lang ++ "/blocks/"++ name ++ "." ++ fmt)
    compiledBlock <- applyTemplate tpl blockContext content
    return $ itemBody compiledBlock

-- getBlockInContext :: String -> Context String -> Context String
-- getBlockInContext lang name ctx =
--     field name (\_ -> getBlock lang name ctx)

blockLoader :: String -> Context String
blockLoader lang =
    functionField "block" (\args item -> getBlock lang args mempty)

globalContext lang = blockLoader lang `mappend` defaultContext

elementList :: String -> String -> String -> Compiler String
elementList lang plural singular = do
    elts <- loadAll $ fromGlob (lang ++ "/" ++ plural ++ "/*.md")
    tpl  <- loadBody $ fromFilePath ("templates/" ++ singular ++ "-item.html")
    list <- applyTemplateList tpl defaultContext elts
    return list

makeIndexPage lang plural singular =
    create [fromFilePath (lang ++ "/"++ plural ++".html")] $ do
        route $ setExtension "html"
        compile $ do
            let elts =
                    field plural (\_ -> elementList lang plural singular) `mappend`
                    globalContext lang
                tplName = fromFilePath ("templates/" ++ plural ++ "-page.html")

            makeItem ""
                >>= loadAndApplyTemplate tplName elts
                >>= loadAndApplyTemplate "templates/default.html" (globalContext lang)
                >>= relativizeUrls

makeElements lang plural singular =
    match (fromGlob $ lang ++ "/"++ plural ++"/*.md") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate (
                fromFilePath $ "templates/"++ singular ++".html")  (globalContext lang)
            >>= loadAndApplyTemplate "templates/default.html" (globalContext lang)
            >>= relativizeUrls
