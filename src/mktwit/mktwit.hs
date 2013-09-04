-- mktwit.hs: Cuts a set of text down to chunks of no more than 140 characters suitable for tweeting.
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, BangPatterns #-}
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Int(Int64)
import Control.Monad
import System.Console.CmdLib

data Main = Main { infile :: String
                 , outfile :: String }
    deriving(Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
        infile  %> [ Help "File to be converted to tweets",
                     ArgHelp "FILENAME",
                     Short ['i'] ],
        outfile %> [ Help "File name for output, defaults to tweets.txt",
                     ArgHelp "FILENAME",
                     Short ['o'],
                     Long ["out","output"] ]
                     ]

instance RecordCommand Main where
    mode_summary _ = "Cuts a set of text down to chunks of no more than 140 characters suitable for tweeting."

main = getArgs >>= executeR Main {} >>= \opts ->
    do
       text <- L.readFile $ infile opts
       tweets <- return $ genTweets text
       L.writeFile (out opts) tweets
  where out opts | null $ outfile opts = "tweets.txt"
                 | otherwise = outfile opts


genTweets :: L.ByteString -> L.ByteString
genTweets text | L.null text = ""
               | otherwise = L.intercalate "\n\n" $ genTweets' $ L.words text
  where
    genTweets' :: [L.ByteString] -> [L.ByteString]
    genTweets' []     = []
    genTweets' [w]    = [w]
    genTweets' (w:ws) = go (L.length w, w) ws

    go :: (Int64,L.ByteString) -> [L.ByteString] -> [L.ByteString]
    go (_len, !tweet) [] = [tweet]
    go (!len, !tweet) (w:ws) | wlen + len <= 139 = go (len + wlen + 1,w') ws
                             | otherwise = tweet : go (wlen, w) ws
      where wlen = L.length w
            w'   = tweet `L.append` " " `L.append` w