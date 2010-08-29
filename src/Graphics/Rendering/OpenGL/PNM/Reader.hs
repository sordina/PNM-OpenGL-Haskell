module Graphics.Rendering.OpenGL.PNM.Reader
where

import Data.List as L
import Data.Bits as B
import Data.ByteString.Lazy.Char8 as C
import Text.ParserCombinators.Parsec as P
import Graphics.Rendering.OpenGL.PNM.Format as PF

-- Types

type FileHandle = String

-- Definitions

readFilePNM :: FileHandle -> IO PF.PNM
readFilePNM = undefined

parsePNM :: ByteString -> PF.PNM
parsePNM = undefined

--pnmParser = P.newline <|> P.newline
pnmParser = do
  match <- P.choice $ L.map P.string PF.descriptorStrings
  P.newline
  return $ case match of
           "P1" -> Just PF.P1
           "P2" -> Just PF.P2
           "P3" -> Just PF.P3
           "P4" -> Just PF.P4
           "P5" -> Just PF.P5
           "P6" -> Just PF.P6
           otherwise -> Nothing
