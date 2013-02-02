--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Control.Monad       (forM_)
import           Data.Monoid         (mappend, mempty)
import           Data.Maybe          (catMaybes, fromMaybe, listToMaybe, maybeToList)
import qualified Data.Map            as M
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

    --forM_ ["en", "fr"] $ \lang -> (makeElementsWithContext (confSpeakersCtx lang) lang "events" "event")
    forM_ ["en", "fr"] $ \lang -> (makeElementsWithContext defaultContext lang "events" "event")


--------------------------------------------------------------------------------
-- Speakers
--

    forM_ ["en", "fr"] $ \lang -> (makeIndexPage lang "speakers" "speaker")

    forM_ ["en", "fr"] $ \lang -> (makeElementsWithContext (hisEventsCtx lang) lang "speakers" "speaker")


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
    functionField "block" (\args item -> getBlock lang args (constField "lang" lang))

globalContext lang =
    blockLoader lang `mappend`
    constField "lang" lang `mappend`
    defaultContext

elementList :: String -> String -> String -> Compiler String
elementList lang plural singular = do
    elts <- loadAllSnapshots (fromGlob (lang ++ "/" ++ plural ++ "/*.md")) "content"
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

makeElementsWithContext ctx lang plural singular = let
        bigCtx = ctx `mappend` (globalContext lang)
    in
        match (fromGlob $ lang ++ "/"++ plural ++"/*.md") $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate (
                    fromFilePath $ "templates/"++ singular ++".html") bigCtx
                >>= loadAndApplyTemplate "templates/default.html" bigCtx
                >>= relativizeUrls

makeElements = makeElementsWithContext mempty

confSpeakersCtx :: String -> Context String
confSpeakersCtx lang =
    field "speakers" (\conf -> getSpeakerCompiler lang conf)

getSpeakerCompiler lang conf = do
    speakerList <- getSpeakerList conf
    speakers <- getSpeakers lang speakerList
    tpl <- loadBody "templates/speaker-item.html"
    content <- applyTemplateList tpl defaultContext speakers
    return content

getSpeakerList :: Item String -> Compiler [String]
getSpeakerList conf = do
    metadata <- getMetadata $ itemIdentifier conf
    return $ splitSpeakers metadata
    where
        getSpeakers = (fromMaybe "") . (M.lookup "speaker")
        splitSpeakers = splitOn (==',') . getSpeakers

getSpeakers :: String -> [String] -> Compiler [Item String]
getSpeakers lang names = do
    allSpeakers <- loadAllSnapshots  (fromGlob (lang ++ "/speakers/*.md")) "content"
    filterItems (isWithinSpeakers names) allSpeakers

isWithinSpeakers :: [String] -> (Item String) -> Compiler Bool
isWithinSpeakers speakers speaker = do
    return $ itemIdFromItem speaker `elem` speakers


filterItems :: (Item String -> Compiler Bool) -> [Item String] -> Compiler ([Item String])
filterItems p l = do
    ct <- sequence $ map (keepItem p) l
    return $ catMaybes ct

keepItem :: (Item String -> Compiler Bool) -> Item String -> Compiler (Maybe (Item String))
keepItem p i = do
    ok <- p i
    return $ if ok then Just i else Nothing

hisEventsCtx :: String -> Context String
hisEventsCtx lang =
    field "confs" (\speaker -> taSoeur lang speaker)

taSoeur :: String -> Item String -> Compiler String
taSoeur lang speaker = getEventsCompiler lang $ itemIdFromItem speaker

itemIdFromItem :: Item String -> String
itemIdFromItem i = itemId
    where
        itemId = head $ splitOn (=='.') fileName
        stringId = show $ itemIdentifier i
        fileName = last $ splitOn (=='/') stringId

getEventsCompiler :: String -> String -> Compiler String
getEventsCompiler lang speaker = do
    hisEvents <- getEvents lang speaker
    tpl <- loadBody $ "templates/event-item.html"
    content <- applyTemplateList tpl defaultContext hisEvents
    return content

getEvents :: String -> String -> Compiler [Item String]
getEvents lang speaker = do
    allEvents <- loadAllSnapshots (fromGlob $ lang ++ "/events/*.md") "content"
    filterItems (hasSpeaker speaker) allEvents

hasSpeaker :: String -> Item String -> Compiler Bool
hasSpeaker name conf =
    pipe <$> (getMetadata $ itemIdentifier conf)
    where
        getSpeakers = M.lookup "speaker"
        splitSpeakers = fmap $ splitOn (==',')
        matchSpeakers = fmap $ any (== name)
        end = fromMaybe False
        pipe = end . matchSpeakers . splitSpeakers . getSpeakers

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)
