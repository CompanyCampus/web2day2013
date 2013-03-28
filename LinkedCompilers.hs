{-# LANGUAGE OverloadedStrings #-}

module LinkedCompilers where

import           Control.Applicative    ((<$>))
import           Data.List              (intersperse)
import qualified Data.Map               as M
import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Monoid            (mappend, mempty)

import           Utils

import Hakyll

confSpeakersCtx :: String -> Context String
confSpeakersCtx lang =
    field "speakers" (\conf -> getSpeakerCompiler lang conf) `mappend`
    roomClassCtx `mappend`
    field "topicBlock" (\conf -> getTopicCompiler lang conf)

getRoomClass id = do
    md <- getMetadata id
    return $ case (M.lookup "location" md) of
        Just "Room 1" -> "event-maxi"
        Just "Room 2" -> "event-mini"
        Just "Trempolino" -> "event-trempolino"
        _ -> ""

getConstRoomClassCtx id =
    field "eventcolor" (const (getRoomClass id))

roomClassCtx = field "eventcolor" (getRoomClass . itemIdentifier)

getSpeakerCompiler lang conf = do
    speakerList <- getSpeakerList conf
    speakers <- getSpeakers lang speakerList
    tpl <- loadBody "templates/speaker-item.html"
    content <- applyTemplateListWithContexts tpl (makeItemContextPairList speakers)
    return content



makeDefaultContext :: (Identifier, Metadata) -> Context String
makeDefaultContext (i, m) =
        makeUrlField i `mappend`
        makeMetadataContext m
    where
        makeMetadataContext m =
            (Context $ \k _ -> do
                return $ fromMaybe "" $ M.lookup k m)

        makeUrlField id =
            field "url" $ \_ -> do
                fp <- getRoute id
                return $ fromMaybe "" $ fmap toUrl fp

makeItemContextPairList :: [(Identifier, Metadata)] -> [(Context String, Item String)]
makeItemContextPairList ims =
    makeItemContextPairListWith ims (const mempty)

makeItemContextPairListWith :: [(Identifier, Metadata)]
                            -> (Identifier -> Context String)
                            -> [(Context String, Item String)]
makeItemContextPairListWith ims a = map f ims
    where
    f p = (makeDefaultContext p `mappend` (a $ fst p), Item (fst p) "")

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
    field "confs" (\speaker -> speakerEventsCompiler lang speaker)

speakerEventsCompiler :: String -> Item String -> Compiler String
speakerEventsCompiler lang speaker = getEventsCompiler lang $ itemIdFromItem speaker

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
    hisEvents <- getSpeakerEvents lang speaker
    tpl <- loadBody $ "templates/event-item.html"
    content <- applyTemplateList tpl ctx hisEvents
    return content
    where
        ctx =
            roomClassCtx `mappend`
            defaultContext

getSpeakerEvents :: String -> String -> Compiler [Item String]
getSpeakerEvents lang speaker = do
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

topicEventsCtx :: String -> Context String
topicEventsCtx lang =
    field "events" (\topic -> getTopicEventsCompiler lang (itemIdFromIdentifier $ itemIdentifier topic))

getTopicEventsCompiler :: String -> String -> Compiler String
getTopicEventsCompiler lang topic = do
    events <- getTopicEvents lang topic
    tpl <- loadBody "templates/event-item.html"
    content <- applyTemplateListWithContexts tpl (
        makeItemContextPairListWith events getConstRoomClassCtx)
    return content

getTopicEvents :: String -> String -> Compiler [(Identifier, Metadata)]
getTopicEvents lang topic = do
    allEvents <- getAllMetadata (fromGlob $ lang ++ "/events/*.md")
    return $ filter (hasTopic topic) allEvents

hasTopic :: String -> (Identifier, Metadata) -> Bool
hasTopic topic (_,m) =
    pipe m
    where
        getTopic = M.lookup "topic"
        matchTopic = fmap (== topic)
        end = fromMaybe False
        pipe = end . matchTopic . getTopic

getTopicCompiler :: String
                 -> Item String
                 -> Compiler String
getTopicCompiler lang conf = do
        md <- getMetadata $ itemIdentifier conf
        maybe (return "") (getTopicCompilerBlock lang) (M.lookup "topic" md)

getTopicCompilerBlock :: String
                      -> String
                      -> Compiler String
getTopicCompilerBlock lang topic = do
        i <- makeItem ""
        md <- getMetadata $ getTopicId topic
        tpl <- loadBody "templates/topic-item.html"
        item <- applyTemplate tpl (makeDefaultContext (getTopicId topic, md)) i
        return $ itemBody item
    where
        getTopicId t = fromFilePath $ lang ++ "/topics/" ++ t ++ ".md"
