{-# LANGUAGE OverloadedStrings #-}
module RouteFactories where

import           Data.List               (intercalate)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             (mappend, mconcat, mempty)
import           Data.Time.Clock         (UTCTime (..))
import           Data.Time.LocalTime     (LocalTime, hoursToTimeZone, localTimeToUTC)
import           Data.Time.Format        (formatTime, parseTime)
import           System.FilePath         (joinPath, replaceExtension, splitDirectories)
import           System.Locale           (TimeLocale, defaultTimeLocale)

import Hakyll

import LinkedCompilers

langs = ["fr", "en"]
defaultLang = "fr"

langRoute = gsubRoute (defaultLang ++ "/") (const "")

globalContext lang =
    blockLoader lang `mappend`
    constField "urllang" (if (lang == defaultLang) then "" else lang ++ "/") `mappend`
    constField "lang" lang `mappend`
    field "eventcolor" (getRoomClass . itemIdentifier) `mappend`
    partnersCtx `mappend`
    reducedEventContext lang `mappend`
    defaultContext

reducedEventContext :: String -> Context String
reducedEventContext lang =
    field "speakers-names" (\conf -> getSpeakerNameCompiler lang conf) `mappend`
    roomClassCtx `mappend`
    short_date lang `mappend`
    field "topicBlock" (\conf -> getTopicCompiler lang conf)

completeEventContext :: String -> Context String
completeEventContext lang =
    reducedEventContext lang `mappend`
    field "speakers" (\conf -> getSpeakerCompiler lang conf)

completeSpeakerContext :: String -> Context String
completeSpeakerContext lang =
    let e = reducedEventContext lang
    in field "confs" (\speaker -> speakerEventsCompiler e lang speaker)

completeTopicContext :: String -> Context String
completeTopicContext lang =
    field "events" (\topic ->
        getTopicEventsCompiler (reducedEventContext lang) lang (
                        itemIdFromIdentifier $ itemIdentifier topic))



getBlock :: String -> [String] -> (Context String) -> Compiler String
getBlock lang args ctx = let
        invertLangCtx = constField "ilang" . invertLang
        blockContext identifier = (invertLangCtx identifier) `mappend` ctx `mappend` defaultContext
        [name, fmt] = args
    in do
    identifier <- getUnderlying
    tpl <- loadBody $ fromFilePath ("templates/blocks/"++ name ++".html")
    content <- load $ fromFilePath (lang ++ "/blocks/"++ name ++ "." ++ fmt)
    compiledBlock <- applyTemplate tpl (blockContext $ show identifier) content
    return $ itemBody compiledBlock

blockLoader :: String -> Context String
blockLoader lang =
    functionField "block" (\args item -> getBlock lang args (globalContext lang))

invertLang :: FilePath -> FilePath
invertLang = joinPath . il . removePage . splitDirectories . getPath
    where
        getPath = (flip replaceExtension "html")
        removePage = filter (/= "pages")
        il ("fr":xs) = if("en" /= defaultLang) then "en":xs else xs
        il ("en":xs) = if("fr" /= defaultLang) then "fr":xs else xs
        il xs         = xs

elementList :: String -> String -> String -> Compiler String
elementList lang plural singular = do
    elts <- loadAll (fromGlob (lang ++ "/" ++ plural ++ "/*.md"))
    tpl  <- loadBody $ fromFilePath ("templates/" ++ singular ++ "-item.html")
    list <- applyTemplateList tpl (globalContext lang) elts
    return list

makeIndexPage lang plural singular =
    create [fromFilePath (lang ++ "/"++ plural ++".html")] $ do
        route $ (setExtension "html") `composeRoutes` langRoute
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
            route $ (setExtension "html") `composeRoutes` langRoute
            compile $ pandocCompiler
                >>= loadAndApplyTemplate (
                    fromFilePath $ "templates/"++ singular ++".html") bigCtx
                >>= loadAndApplyTemplate "templates/default.html" bigCtx
                >>= relativizeUrls

makeElements = makeElementsWithContext mempty

makeCalendar :: String -> Rules ()
makeCalendar lang =
    create [fromFilePath $ lang ++ "/calendar.ics"] $ do
    route $ (setExtension "html") `composeRoutes` langRoute
    compile $ do
        makeItem ""
            >>= calendarCompiler lang
            >>= loadAndApplyTemplate "templates/calendar.ics" (globalContext lang)

calendarCompiler :: String -> Item a ->  Compiler (Item String)
calendarCompiler lang item = do
    events <- loadAll $ fromGlob (lang ++ "/events/*.md")
    tpl <- loadBody "templates/ics-event"
    contents <- applyTemplateList tpl (iso8601Ctx `mappend` globalContext lang) events
    makeItem contents

iso8601Ctx = (iso8601_date "start") `mappend` (iso8601_date "end")

makeIso8601 :: String -> Maybe String
makeIso8601 =
    let parser = parseTime defaultTimeLocale "%Y-%m-%d %H:%M" :: String -> Maybe LocalTime
        addTimeZone = localTimeToUTC (hoursToTimeZone 2) -- /!\ Hard coded for May 2013
        formatter = formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ%z"
    in fmap (formatter . addTimeZone) . parser

iso8601_date :: String -> Context String
iso8601_date date =
    field ("iso8601_"++ date) (\item -> do
        metadata <- getMetadata $ itemIdentifier item
        return $ fromMaybe "" $ M.lookup date metadata >>= makeIso8601
    )

makeShortStart :: String -> String -> Maybe String
makeShortStart lang =
    let parser = parseTime defaultTimeLocale "%Y-%m-%d %H:%M" :: String -> Maybe LocalTime
        formatter = formatTime defaultTimeLocale "%A %H:%M"
    in fmap formatter . parser

makeShortEnd :: String -> String -> Maybe String
makeShortEnd lang =
    let parser = parseTime defaultTimeLocale "%Y-%m-%d %H:%M" :: String -> Maybe LocalTime
        formatter = formatTime defaultTimeLocale "%H:%M"
    in fmap formatter . parser

short_date :: String -> Context String
short_date lang =
    let s_start m = fromMaybe "" $ M.lookup "start" m >>= makeShortStart lang
        s_end m = fromMaybe "" $ M.lookup "end" m >>= makeShortEnd lang
    in field "short_start" (\item -> do
        md <- getMetadata $ itemIdentifier item
        return $ s_start md ++ " - " ++ s_end md
    )

makeSinglePages :: String -> Rules ()
makeSinglePages lang =
    let r = gsubRoute ("pages/") (const "")
    in
        match (fromGlob $ lang ++ "/pages/*.md") $ do
        route $ r `composeRoutes` (setExtension "html") `composeRoutes` langRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (globalContext lang)
            >>= relativizeUrls

imagesList :: String -> Compiler String
imagesList dir = let
      addBrackets = ("["++) . (++"]")
      addQuotes = ("\""++) . (++"\"")
      imgs = loadAll (fromGlob $ "assets/images/photos/" ++ dir ++ "/*") :: Compiler [Item CopyFile]
    in do
      list <- imgs
      return $ addBrackets $ intercalate (",") $ map (addQuotes . show .  itemIdentifier) list


imagesDataCtx = let
   dirs = ["200x200", "408x408", "200x408", "408x200"]
   lists = map imagesList dirs
   in
      mconcat $ zipWith field dirs (map const lists)

makeImagesdata :: Rules ()
makeImagesdata =
   create ["js/imagesdata.js"] $ do
      route idRoute
      compile $ do
         makeItem ""
             >>= loadAndApplyTemplate "templates/imagesdata.js" imagesDataCtx

