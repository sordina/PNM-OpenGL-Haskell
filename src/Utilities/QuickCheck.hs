module Utilities.QuickCheck
where

import Text.ParserCombinators.Parsec

mkPropComp p s r = case runParser p () "quickcheck parser property" s of
                             Right r   -> True
                             otherwise -> False
