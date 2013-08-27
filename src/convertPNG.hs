{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Aeson
import Data.List(unzip4)
import Codec.Picture.Png(decodePng)
--import Codec.Picture.Types(DynamicImage(..),Pixel(..),PixelRGBA16(..),Image(..),
--                           ColorConvertible(..),pixelFold,promotePixel)
import Codec.Picture.Types
import qualified Data.ByteString as L
import System.Environment(getArgs)
import System.Console.CmdLib
import Control.Monad


data Main = Main { start :: String, input :: String,
                   output :: String, phases :: Phase,
                   singlePhasePNG :: Bool }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
        start          %> [ Help "Start position of the macro.",
                            ArgHelp "(X,Y)",
                            Default "(0,0)",
                            Short ['s'],
                            Long ["start"] ],
        input          %> [ Help "Images to be converted to blueprints.",
                            ArgHelp "FILENAME FILENAME ...",
                            Required True,
                            Short ['i'] ],
        output         %> [ Help "Name to use for blueprints, if not specified uses input name",
                            ArgHelp "TEXT",
                            Short ['o'],
                            Long ["output"] ],
        phases         %> [ Help "Phase to create a blueprint for.",
                            Long ["phase"],
                            Default All,
                            Short ['p'] ],
        singlePhasePNG %> [ Help "If True, a pixel represents a single instruction. If False \
                                 \a pixel represents an instruction for each phase",
                            Long ["single-phase-png"],
                            Default False ]
        ]

instance RecordCommand Main where
    mode_summary _ = "Simple program to convert RGBA encoded .png images into quickfort blueprints"

data Phase = All
           | Dig
           | Build
           | Place
           | Query
    deriving (Typeable, Data, Eq)

instance ColorConvertible PixelRGBA8 PixelRGBA16 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGBA8 r g b a) = PixelRGBA16 (fromIntegral r)
                                                    (fromIntegral g)
                                                    (fromIntegral b)
                                                    (fromIntegral a)

main = getArgs >>= executeR Main {} >>= \opts ->
    do
        putStrLn (input opts)


type LongPixel = (Int,Int,Int,Int)

convertPNGToCSV :: L.ByteString -> L.ByteString -> Maybe L.ByteString
convertPNGToCSV img schema = undefined

--doThing :: DynamicImage -> (a -> b) -> L.ByteString

parseDig :: [Int] -> (Int -> String) -> Maybe String
parseDig commands mapping = undefined

parseBuild :: [Int] -> (Int -> String) -> Maybe String
parseBuild commands mapping = undefined

parsePlace :: [Int] -> (Int -> String) -> Maybe String
parsePlace commands mapping = undefined

parseQuery :: [Int] -> (Int -> String) -> Maybe String
parseQuery commands mapping = undefined

unzipImage4 :: L.ByteString -> Maybe ([Int],[Int],[Int],[Int])
unzipImage4 str = convert $ decodePng str
    where convert (Left s) = Nothing
          convert (Right img) = Just (unzip4 $ imageToList img)


--imageToList :: (Pixel p,ColorConvertible p PixelRGBA16) => 
--                (Image p) -> [LongPixel]
imageToList :: DynamicImage -> [LongPixel]
imageToList (ImageRGBA8 img) = reverse $ pixelFold pixPred [] img
imageToList (ImageRGBA16 img) = reverse $ pixelFold pixPred [] img
imageToList _ = []


pixPred :: (ColorConvertible a  PixelRGBA16) => [LongPixel] -> x -> x -> a -> [LongPixel]
pixPred acc _ _ pix = (promoteToTuple pix):acc

promoteToTuple :: (ColorConvertible pixel PixelRGBA16) => pixel -> LongPixel
promoteToTuple p = pixelToTuple p' 
    where p' = (promotePixel p)::PixelRGBA16


pixelToTuple :: PixelRGBA16 -> LongPixel
pixelToTuple (PixelRGBA16 r g b a) = (fromIntegral r, fromIntegral g, fromIntegral b,
                        fromIntegral a)