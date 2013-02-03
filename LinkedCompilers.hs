{-# LANGUAGE OverloadedStrings #-}

module LinkedCompilers where

import           Control.Applicative    ((<$>))
import           Data.List              (intersperse)
import qualified Data.Map               as M
import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Monoid            (mappend)

import           Utils

import Hakyll

confSpeakersCtx :: String -> Context String
confSpeakersCtx lang =
    field "speakers" (\conf -> getSpeakerCompiler lang conf)

getSpeakerCompiler lang conf = do
    speakerList <- getSpeakerList conf
    speakers <- getSpeakers lang speakerList
    tpl <- loadBody "templates/speaker-item.html"
    content <- applyTemplateListWithContexts tpl (makeItemContextPairList speakers)
    return content


makeMetadataContext :: Metadata -> Context String
makeMetadataContext m =
    (Context $ \k _ -> do
        return $ fromMaybe "" $ M.lookup k m)

makeUrlField :: Identifier -> Context String
makeUrlField id =
    field "url" $ \_ -> do
        fp <- getRoute id
        return $ fromMaybe "" $ fmap toUrl fp


makeItemContextPairList :: [(Identifier, Metadata)] -> [(Context String, Item String)]
makeItemContextPairList ims = map f ims
    where
    f p = (makeItemContext p, Item (fst p) "")
    makeItemContext p =
        makeUrlField (fst p) `mappend`
        makeMetadataContext (snd p)

applyTemplateListWithContexts :: Template
                              -> [(Context a, Item a)]
                              -> Compiler String
applyTemplateListWithContexts =
    applyJoinTemplateListWithContexts ""

applyJoinTemplateListWithContexts :: String
                                  -> Template
                                  -> [(Context a, Item a)]
                                  -> Compiler String
applyJoinTemplateListWithContexts delimiter tpl pairs = do
    items <- mapM (\p -> applyTemplate tpl (fst p) (snd p)) pairs
    return $ concat $ intersperse delimiter $ map itemBody items

getSpeakerList :: Item String -> Compiler [String]
getSpeakerList conf = do
    metadata <- getMetadata $ itemIdentifier conf
    return $ splitSpeakers metadata
    where
        getSpeakers = (fromMaybe "") . (M.lookup "speaker")
        splitSpeakers = splitOn (==',') . getSpeakers

getSpeakers :: String -> [String] -> Compiler [(Identifier, Metadata)]
getSpeakers lang names = do
    speakersMetadata <- getAllMetadata (fromGlob $ lang ++ "/speakers/*.md")
    return $ filter (isWithinSpeakers names) speakersMetadata

isWithinSpeakers :: [String] -> (Identifier, Metadata) -> Bool
isWithinSpeakers speakers speaker =
   itemIdFromIdentifier (fst speaker) `elem` speakers


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

itemIdFromIdentifier :: Identifier -> String
itemIdFromIdentifier i = itemId
    where
        itemId = head $ splitOn (=='.') fileName
        stringId = show i
        fileName = last $ splitOn (=='/') stringId

getEventsCompiler :: String -> String -> Compiler String
getEventsCompiler lang speaker = do
    hisEvents <- getEvents lang speaker
    tpl <- loadBody $ "templates/event-item.html"
    content <- applyTemplateList tpl defaultContext hisEvents
    return content

getEvents :: String -> String -> Compiler [Item String]
getEvents lang speaker = do
    allEvents <- loadAll (fromGlob $ lang ++ "/events/*.md")
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


