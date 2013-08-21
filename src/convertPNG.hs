{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.List(unzip4)
import Codec.Picture.Png(decodePng)
import Codec.Picture.Types(DynamicImage(..),Pixel(..),PixelRGBA16(..),Image(..),
                           ColorConvertible(..),pixelFold,promotePixel)
import qualified Data.ByteString.Lazy as L
import System.Environment(getArgs)


--main = do
--    args <- getArgs
--    rawpng <- readFile (head args)
--    mapping <- readFile ""
--    qfCSV <- return (convertPNGToCSV)


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

unzipImage4 :: Pixel pixel => Image pixel -> ([Int],[Int],[Int],[Int])
unzipImage4 = unzip4 . reverse . pixelFold pred []
    where pred acc _ _ pix = (promoteToTuple pix):acc


promoteToTuple :: (ColorConvertible pixel PixelRGBA16) => pixel -> LongPixel
promoteToTuple p = pixelToTuple $ promotePixel p


pixelToTuple :: PixelRGBA16 -> LongPixel
pixelToTuple (PixelRGBA16 r g b a) = (fromIntegral r, fromIntegral g, fromIntegral b,
                        fromIntegral a)