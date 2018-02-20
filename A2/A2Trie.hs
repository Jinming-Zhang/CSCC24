module A2Trie where

import           Data.Char (ord, chr)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

-- It's OK to import stuff from the standard library. Put your import lines here.
import           Data.Maybe
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
-- if reaches the end of [char], the value we want is in the root
trieLookup [] (TrieOf value  map) = value
trieLookup (char : str) (TrieOf value  map)
    | (ord char) `elem` (IntMap.keys map) = trieLookup str (getMap char map)
    | otherwise = Nothing

-- get the map returned by lookup
getMap :: Char -> IntMap (Trie a) -> Trie a
getMap char map = snd tree 
    where 
        tree = lst !! 0
            where 
                lst = [(c, m) | (c, m) <- (IntMap.toList map), c == (ord char)]



trieInsert :: [Char] -> a -> Trie a -> Trie a
-- nomore char to insert, create the Trie of just the value
trieInsert [] value (TrieOf oldv oldm) = (TrieOf (Just value) oldm)

trieInsert (char : str) value (TrieOf oldv oldm)
    -- head character is in the dictionary
    | (isJust (charMapLookup char oldm)) = (TrieOf oldv (charMapInsert char (trieInsert str value (fromJust (charMapLookup char oldm))) oldm))
        --where
            --newm = (charMapInsert char (trieInsert str value (fromJust (charMapLookup char oldm))) oldm)
    -- if not, insert it to the dictionay
    | otherwise = (TrieOf oldv newm)
        where
            newm = (charMapInsert char (trieInsert str value (TrieOf (Nothing) IntMap.empty)) oldm)



at = TrieOf Nothing (IntMap.fromList [(ord 'p', p), (ord 't', t)])
  where
    p = TrieOf Nothing (IntMap.fromList [(ord 'i', pi)])
      where
        pi = TrieOf (Just 1) (IntMap.fromList [(ord 't', pit)])
          where
            pit = TrieOf (Just 9) IntMap.empty
    t = TrieOf Nothing (IntMap.fromList [(ord 'o', to)])
      where
        to = TrieOf Nothing (IntMap.fromList [(ord 'p', top), (ord 'n', ton)])
          where
            top = TrieOf (Just 5) IntMap.empty
            ton = TrieOf (Just 7) IntMap.empty