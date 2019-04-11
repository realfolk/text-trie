{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2019.04.09
-- |
-- Module      :  FromListBench.Text
-- Copyright   :  Copyright (c) 2008--2009 wren gayle romano, 2019 michael j. klein
-- License     :  BSD3
-- Maintainer  :  lambdamichael@gmail.com
-- Stability   :  experimental
--
-- Benchmarking for left- vs right-fold for @fromList@ with
-- encoding from `Text` to UTF16 (Little Endian) `ByteString`
-- for use with `Trie`
----------------------------------------------------------------

module FromListBench.Text.Encode (test) where

import qualified Data.Trie as Tr
import Data.Trie.Convenience (insertIfAbsent)
import Data.List             (foldl', sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Microbench
import Control.Exception     (evaluate)
----------------------------------------------------------------

fromListR, fromListL :: [(T.Text, a)] -> Tr.Trie a
fromListR = foldr  (uncurry (Tr.insert . E.encodeUtf16LE)) Tr.empty
fromListL = foldl' (flip $ uncurry $ insertIfAbsent . E.encodeUtf16LE) Tr.empty

getList, getList'  :: T.Text -> Int -> [(T.Text, Int)]
getList  xs n = map (\k -> (k,0)) . T.inits . T.take n $ xs
getList' xs n = map (\k -> (k,0)) . T.tails . T.take n $ xs

test :: IO ()
test  = do
    -- 100000 is large enough to trigger Microbench's stop condition,
    -- and small enough to not lock up the system in trying to create it.
    xs <- evaluate $ T.replicate 100000 (T.singleton 'a')

    microbench "fromListR obverse" (Tr.null . fromListR . getList xs)
    microbench "fromListL obverse" (Tr.null . fromListL . getList xs)

    putStrLn ""
    microbench "fromListR reverse" (Tr.null . fromListR . getList' xs)
    microbench "fromListL reverse" (Tr.null . fromListL . getList' xs)

    -- Sorting forces it into the obverse order at O(n log n) cost
    putStrLn ""
    putStrLn ""
    microbench "fromListR obverse sorted" (Tr.null . fromListR . sort . getList xs)
    microbench "fromListL obverse sorted" (Tr.null . fromListL . sort . getList xs)
    putStrLn ""
    microbench "fromListR reverse sorted" (Tr.null . fromListR . sort . getList' xs)
    microbench "fromListL reverse sorted" (Tr.null . fromListL . sort . getList' xs)

----------------------------------------------------------------
----------------------------------------------------------- fin.
