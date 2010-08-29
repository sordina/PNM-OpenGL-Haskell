module Graphics.Rendering.OpenGL.PNM.Format

  (PNM(..),
   Descriptor(..),
   descriptor,
   descriptors,
   descriptorStrings,
   descriptorLookup,
   Resolution,
   PixelData,
   Color(..))

where

data PNM = PNM {
    getDescriptor :: Descriptor,
    getResolution :: Resolution,
    getMax :: Maybe Integer,
    getData :: PixelData
  }
  deriving (Eq, Ord, Show)

data Descriptor = P1 | P2 | P3 | P4 | P5 | P6
  deriving (Eq, Ord, Show, Enum)

type Resolution = (Integer, Integer)

type PixelData = [Color]

data Color = Color {
  red   :: Integer,
  green :: Integer,
  blue  :: Integer }
  deriving (Eq, Ord, Show)

descriptors :: [Descriptor]
descriptors = enumFrom P1

descriptor :: Descriptor -> String
descriptor P1 = "Portable bitmap ASCII"
descriptor P2 = "Portable graymap  ASCII"
descriptor P3 = "Portable pixmap ASCII"
descriptor P4 = "Portable bitmap Binary"
descriptor P5 = "Portable graymap  Binary"
descriptor P6 = "Portable pixmap"

descriptorStrings :: [String]
descriptorStrings = map show descriptors

descriptorLookup :: [(String,Descriptor)]
descriptorLookup = zip descriptorStrings descriptors
