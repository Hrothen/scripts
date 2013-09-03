-- mktwit.hs: Cuts a set of text down to chunks of no more than 140 characters suitable for tweeting.
{-# LANGUAGE DeriveDataTypeable #-}
import qualified Data.ByteString.Lazy.Char8 as L
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
  where out opts = case (outfile opts) of
                    null -> "tweets.txt"
                    _    -> outfile opts


genTweets :: L.ByteString -> L.ByteString
genTweets text | L.null text = L.pack ""
               | otherwise = L.intercalate (L.pack "\n\n") $ genTweets' $ L.words text
  where genTweets' txt = foldr p [] txt
          where p word [] = [word]
                p word words@(w:ws) | L.length word + L.length w <= 139 =
                                        (word `L.append` L.pack " " `L.append` w):ws
                                    | otherwise = word:words