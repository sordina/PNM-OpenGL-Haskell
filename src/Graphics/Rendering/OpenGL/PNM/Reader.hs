module Graphics.Rendering.OpenGL.PNM.Reader
where

import Data.List as L
import Data.Bits as B
import qualified Data.ByteString.Lazy.Char8 as C
import Text.ParserCombinators.Parsec as P
import Graphics.Rendering.OpenGL.PNM.Format as PF

-- Types

type FileHandle = String

-- Definitions

readFilePNM :: FileHandle -> IO PF.PNM
readFilePNM = undefined

parsePNM :: C.ByteString -> PF.PNM
parsePNM = undefined

pnmParser = do
  pf <- P.choice (L.map mkPFParser PF.descriptorLookup)
        <?> "Invalid file descriptor. Valid choices: [P1..P6]."
  P.newline

  resolution <- resolutionParser
                <?> "Invalid resolution specified. Format is: \"<W> <H>\"."
  P.newline

  pixelData <- dataParser pf

  let maxVal = Nothing

  return $ PNM { getDescriptor = pf,
                 getResolution = resolution,
                 getMax        = maxVal,
                 getData       = pixelData }

mkPFParser (s,p) = P.string s >> return p

resolutionParser = do
  x <- digits
  P.space
  y <- digits
  return (read x, read y)

prop_resolutionParser = case runParser resolutionParser () "resolution test" "10 20" of
                             Right (10,20) -> True
                             otherwise     -> False

digits = P.many1 digit

dataParser P1 = return []
dataParser P2 = return []
dataParser P3 = return []
dataParser P4 = return []
dataParser P5 = return []
dataParser P6 = return []
