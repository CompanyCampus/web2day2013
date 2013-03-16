{-# LANGUAGE OverloadedStrings #-}
module RouteFactories where

import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             (mappend, mempty)
import           Data.Time.Clock         (UTCTime (..))
import           Data.Time.LocalTime     (LocalTime, hoursToTimeZone, localTimeToUTC)
import           Data.Time.Format        (formatTime, parseTime)
import           System.FilePath         (takeBaseName, takeFileName)
import           System.Locale           (TimeLocale, defaultTimeLocale)

import Hakyll

globalContext lang =
    blockLoader lang `mappend`
    constField "lang" lang `mappend`
    defaultContext

getBlock :: String -> [String] -> (Context String) -> Compiler String
getBlock lang args ctx = let
        blockContext = ctx `mappend` defaultContext
        [name, fmt] = args
    in do
    tpl <- loadBody $ fromFilePath ("templates/blocks/"++ name ++".html")
    content <- load $ fromFilePath (lang ++ "/blocks/"++ name ++ "." ++ fmt)
    compiledBlock <- applyTemplate tpl blockContext content
    return $ itemBody compiledBlock

blockLoader :: String -> Context String
blockLoader lang =
    functionField "block" (\args item -> getBlock lang args (constField "lang" lang))

elementList :: String -> String -> String -> Compiler String
elementList lang plural singular = do
    elts <- loadAll (fromGlob (lang ++ "/" ++ plural ++ "/*.md"))
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
                >>= loadAndApplyTemplate (
                    fromFilePath $ "templates/"++ singular ++".html") bigCtx
                >>= loadAndApplyTemplate "templates/default.html" bigCtx
                >>= relativizeUrls

makeElements = makeElementsWithContext mempty

makeCalendar :: String -> Rules ()
makeCalendar lang =
    create [fromFilePath $ lang ++ "/calendar.ics"] $ do
    route idRoute
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

makeSinglePages :: String -> Rules ()
makeSinglePages lang =
    let r = gsubRoute ("pages/") (const "")
    in
        match (fromGlob $ lang ++ "/pages/*.md") $ do
        route $ r `composeRoutes` (setExtension "html")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (globalContext lang)
            >>= relativizeUrls
