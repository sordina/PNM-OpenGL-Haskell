module Graphics.Rendering.OpenGL.PNM.Reader
where

import Control.Monad
import Data.List as L
import Data.Bits as B
import Data.Char as A
import Text.Parsec as P
import qualified Data.ByteString.Lazy.Char8 as BS

import Utilities.QuickCheck as Q
import Graphics.Rendering.OpenGL.PNM.Format as PF

type FileHandle = String

-- TODO: Use bytestrings here.
readFilePNM :: FileHandle -> IO (Either ParseError PF.PNM)
readFilePNM = liftM (runParser pnmParser () "Parsing PNM string")
              . liftM BS.unpack . BS.readFile

parsePNM :: String -> Either ParseError PF.PNM
parsePNM = runParser pnmParser () "Parsing PNM string"

prop_parse = Q.mkPropComp resolutionParser "10 20" (10,20)

pnmParser = do
  pf <- pfParser <?> "Invalid file descriptor. Valid choices: [P1..P6]."
  P.newline

  resolution <- resolutionParser <?> "Invalid resolution specified. Format is: \"<W> <H>\"."
  P.newline

  -- TODO: Rewind and fall back to regular parsing if max fails.
  max <- maxParser <?> "Invalid maximum value."
  P.newline

  pixelData <- dataParser pf

  return PNM { getDescriptor = pf,
               getResolution = resolution,
               getMax        = Nothing,
               getData       = pixelData }

pfParser = P.choice (L.map mkPFParser PF.descriptorLookup)
  where
    mkPFParser (s,p) = P.try $ P.string s >> return p

prop_pfParser = Q.mkPropComp pfParser "P3" PF.P3

resolutionParser = do
  x <- digits
  P.space
  y <- digits
  return (read x, read y)

maxParser = liftM (read :: String -> Integer) digits

prop_resolutionParser = Q.mkPropComp resolutionParser "10 20" (10,20)

digits = P.many1 digit

binaryChar = P.oneOf "01"
p1bits = P.sepBy1 binaryChar P.spaces

dataParser P1 = liftM (map bitToColor) (P.many1 p1bits)
dataParser P2 = return []
dataParser P3 = return []
dataParser P4 = return []
dataParser P5 = return []
dataParser P6 = P.many1 tripplet

prop_dataParser1 = Q.mkPropComp (dataParser P1) "1 0 1 \n 1 1" [o,z,o,o,o]
  where
    z = PF.Color 0 0 0
    o = PF.Color 1 1 1

-- TODO: Figure out why this is a string...
bitToColor "0" = PF.Color 0 0 0
bitToColor "1" = PF.Color 1 1 1
bitToColor  b  = error $ "Invalid bit: [" ++ b ++ "]."

tripplet = do
  [r,g,b] <- liftM (map (fromIntegral . A.ord)) (P.count 3 P.anyChar)
  return $ PF.Color r g b
