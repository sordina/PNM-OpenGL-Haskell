{-# Language PackageImports #-}

module Graphics.Rendering.OpenGL.PNM.Reader
where

import "mtl" Control.Monad.Identity (Identity)
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
  pf         <- andNL pfParser         <?> "Invalid file descriptor. Valid choices: [P2..P6]."
  resolution <- andNL resolutionParser <?> "Invalid resolution specified. Format is: \"<W> <H>\"."
  max        <-       maxParser        <?> "Invalid maximum value."
  pixelData  <-       dataParser pf    <?> "Invalid pixel data."

  return PNM { getDescriptor = pf,
               getResolution = resolution,
               getMax        = max,
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

maxParser :: ParsecT [Char] u Identity (Maybe Integer) -- Signature to specialize 'read'
maxParser = try (andNL $ liftM (Just . read) digits) <|> return Nothing

prop_maxParser1 = Q.mkPropComp maxParser "10\n" (Just 10)
prop_maxParser2 = Q.mkPropComp maxParser "10"    Nothing

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

andNL parser = do
  res <- parser
  P.newline
  return res
