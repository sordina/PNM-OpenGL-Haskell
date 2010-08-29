module Graphics.Rendering.OpenGL.PNM.Reader
where

import Data.List as L
import Data.Bits as B
import Text.ParserCombinators.Parsec as P
import qualified Data.ByteString.Lazy.Char8 as C

import Graphics.Rendering.OpenGL.PNM.Format as PF
import Utilities.QuickCheck as Q

-- Types

type FileHandle = String

-- Definitions

readFilePNM :: FileHandle -> IO PF.PNM
readFilePNM = undefined

parsePNM :: C.ByteString -> PF.PNM
parsePNM = undefined

prop_parse = Q.mkPropComp resolutionParser "10 20" (10,20)

pnmParser = do
  pf <- pfParser <?> "Invalid file descriptor. Valid choices: [P1..P6]."
  P.newline

  resolution <- resolutionParser <?> "Invalid resolution specified. Format is: \"<W> <H>\"."
  P.newline

  pixelData <- dataParser pf

  let maxVal = Nothing

  return $ PNM { getDescriptor = pf,
                 getResolution = resolution,
                 getMax        = maxVal,
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

prop_resolutionParser = Q.mkPropComp resolutionParser "10 20" (10,20)

digits = P.many1 digit

binaryChar = P.oneOf "01"
p1bits = P.sepBy1 binaryChar P.spaces

dataParser P1 = do
  bits <- P.many1 p1bits
  let nums = map bitToColor bits
  return []

dataParser P2 = return []
dataParser P3 = return []
dataParser P4 = return []
dataParser P5 = return []
dataParser P6 = return []

prop_dataParser1 = Q.mkPropComp (dataParser P1) "1 0 1 1 1" [o,z,o,o,o]
  where
    z = (0,0,0)
    o = (1,1,1)

-- TODO: Figure out why this is a string...
bitToColor "0" = (0,0,0)
bitToColor "1" = (1,1,1)
bitToColor  b  = error $ "Invalid bit: [" ++ b ++ "]."
