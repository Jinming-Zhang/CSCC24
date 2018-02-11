module A2Trie where

import           Data.Char (ord, chr)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

-- It's OK to import stuff from the standard library. Put your import lines here.

-- The following two utility functions will help you.  They treat IntMap as a
-- map from Char.
charMapLookup :: Char -> IntMap a -> Maybe a
charMapLookup c dict = IntMap.lookup (ord c) dict

charMapInsert :: Char -> a -> IntMap a -> IntMap a
charMapInsert c v dict = IntMap.insert (ord c) v dict

-- Question 1.
data Trie a = TrieOf (Maybe a) (IntMap (Trie a))
    deriving Show

trieLookup :: [Char] -> Trie a -> Maybe a
trieLookup = error "TODO"

trieInsert :: [Char] -> a -> Trie a -> Trie a
trieInsert = error "TODO"
