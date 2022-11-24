{-# LANGUAGE TypeApplications #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Data.List.Extra (intercalate, groupOn, groupSortOn, dropWhileEnd)
import SimpleCmd

main :: IO ()
main = do
  out <- cmdLines "df" []
  case out of
    [] -> error' "no output from df!"
    (h:ls) -> do
      putStrLn h
      putStrLn $
        intercalate "\n" $
        map (intercalate "\n") $
        groupFS ls

groupFS :: [String] -> [[String]]
groupFS ls =
  let
    -- group by Used
    used = groupSortOn (read @Int . column 2) ls :: [[String]]
    -- subgroup by Filesystem
    subgroups = map (groupOn (column 0)) used :: [[[String]]]
  in map combineMounts <$> subgroups

combineMounts :: [String] -> String
combineMounts ms =
  let mounts = map (column 5) ms
  in dropWhileEnd (/= ' ') (head ms) ++ intercalate ", " mounts

column :: Int -> String -> String
column n = (!! n) . words
