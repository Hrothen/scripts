{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Aeson
import Data.List(unzip4)
import Codec.Picture.Png(decodePng)
import Codec.Picture.Types(DynamicImage(..),Pixel(..),PixelRGBA16(..),Image(..),
                           ColorConvertible(..),pixelFold,promotePixel)
import qualified Data.ByteString.Lazy as L
import System.Environment(getArgs)


{-
Possible command line arguments:
--
-}

main = do
    args <- getArgs


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
          convert (Right img) = Just (unzip4 img)


imageToList :: (Pixel p,ColorConvertible p PixelRGBA16) => 
                Image p -> [LongPixel]
imageToList = reverse . pixelFold pred []
    where pred acc _ _ pix = (promoteToTuple pix):acc


promoteToTuple :: (ColorConvertible pixel PixelRGBA16) => pixel -> LongPixel
promoteToTuple p = pixelToTuple p' 
    where p' = (promotePixel p)::PixelRGBA16


pixelToTuple :: PixelRGBA16 -> LongPixel
pixelToTuple (PixelRGBA16 r g b a) = (fromIntegral r, fromIntegral g, fromIntegral b,
                        fromIntegral a)