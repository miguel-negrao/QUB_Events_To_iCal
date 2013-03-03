module Main where

import Text.HTML.TagSoup
import Data.Time
import System.Locale
import Network.HTTP
import Data.Default
import Data.ByteString.Lazy as BT (writeFile)
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Time
import Data.UUID.V4
import Control.Applicative
import Text.ICalendar

newVEvent :: IO VEvent
newVEvent = do
   uuid <- T.pack . show <$> Data.UUID.V4.nextRandom
   stamp <- getCurrentTime
   let uid = UID uuid def
       dtstamp = DTStamp stamp def
   return $ VEvent dtstamp uid def def def def def def def def def def def
                   def def def def def def def def def def def def def def
                   def def def def -- Yeah...

newQUBEvent title sdate  edate  venue  description = do
        ev <- newVEvent
        return ev { 
                veDTStart = Just $ DTStartDateTime (UTCDateTime sdate) def,
                veDTEndDuration = Just $ Left $ DTEndDateTime (UTCDateTime edate) def,
                veLocation = Just $ Location (T.pack venue) Nothing Nothing def,
                veSummary = Just $ Summary (T.pack title) Nothing Nothing def,
                veDescription = Just $ Description (T.pack description) Nothing Nothing def
        }                 

vEventsToVCalendar ::[ VEvent] -> VCalendar
vEventsToVCalendar evs =
   def { vcEvents = M.fromList $ map makeEventTuple evs }
   
makeEventTuple ev = ( (uidValue $ veUID ev, Nothing), ev)    
   
setSummary :: Text -> VEvent -> VEvent
setSummary s ev = ev { veSummary = Just (Summary s def def def) }

main::IO()
main = do
        events <- getEvents
        calEvents <- mapM (\(a,b,c,d,e) -> newQUBEvent a b c d e) events
        let cal = vEventsToVCalendar calEvents
        print cal
        BT.writeFile "qubEvents.ics" $ printICal def cal
        
printEvents::IO()
printEvents = do
         events <- getEvents
         sequence_ $ map print $ events                
        
getEvents = do
        --s <- readFile "/Volumes/Data/Downloads/events.html"
        s <- openURL "http://www.qub.ac.uk/schools/SchoolofCreativeArts/Events/"
        let tags = parseTags s
        let eventBlocks = sections (~== "<div class=\"event_listing\">") tags
        --_ <- sequence $ map print $ map f eventBlocks
        --return ()
        return $ map f eventBlocks
        where f eventTags = ( title, sdate,  edate,  venue,  description ++ "\n" ++ booking)
                where title = removeCR $ innerText $ takeWhile (~/= "</h2>") $ head $ sections (~== "<h2>") eventTags
                      sdate = parseDate $ fromTagText $ head $ tail $ head $
                                sections (~== "<span class=\"event_start_date\">") eventTags
                      edate = parseDate $ drop 3 $ fromTagText $ head $ tail $ head $ 
                                sections (~== "<span class=\"event_end_date\">") eventTags
                      description = drop 2 $ innerText $ takeWhile (~/= "<strong>") $ drop 2 $ head $
                                sections (~== "Description") eventTags
                      venue = removeCR $ drop 2 $ innerText $ takeWhile (~/= "<strong>") $ drop 2 $ head $
                                sections (~== "Venue") eventTags
                      booking = innerText $ takeWhile (~/= "<div style=\"clear:both;\">") $ head $
                                sections (~== "Booking info") eventTags

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

parseDate::String -> UTCTime 
parseDate dateString = readTime defaultTimeLocale "%-d %b %Y %l:%M%p" dateString

removeCR :: String -> String
removeCR = filter (\x -> x /= '\r' && x  /= '\n')