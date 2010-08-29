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

pnmParser = do
  pf <- P.choice $ L.map mkPFParser PF.descriptorLookup
  P.newline
  return ()

mkPFParser (s,p) = P.string s >> return p
