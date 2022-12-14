{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Data.Tuple.Extra ((&&&))
import Data.List.Extra (groupSortOn, intercalate, isSuffixOf, sortOn)
import Fmt
import Numeric.Natural
import SimpleCmd

data DfFileSystem = FS {
  fsName :: String,
  fsBlocks :: Natural,
  fsUsed :: Natural,
  fsAvailable :: Natural,
  fsUsePercent :: Natural,
  fsMount :: String
  }
  deriving Show

readFSLine :: String -> (String, String, String, String, String, String)
readFSLine l =
  case words l of
    [name, blocks, used, available, percent, mount] ->
      (name, blocks, used, available, percent, mount)
    _ -> error' "df output with more than 6 columns"

readFS :: String -> DfFileSystem
readFS cs =
  case readFSLine cs of
    (fsName, blocks, used, available, percent, fsMount) ->
      let fsBlocks = read blocks
          fsUsed = read used
          fsAvailable = read available
          fsUsePercent = read $ init percent
      in FS {..}

-- 500GB:
-- Filesystem     1K-blocks      Used Available Use% Mounted on
-- /dev/dm-0      498426880 215097136 281873616  44% /sysroot

-- 12GB VM:
-- Filesystem     1K-blocks    Used Available Use% Mounted on
-- /dev/vda3       12579840 9469076   2759116  78% /

renderFS :: DfFileSystem -> IO ()
renderFS FS {..} =
  fmtLn $
  "" +| padRightF 14 ' ' fsName |+
  "" +| padLeftF 10 ' ' (show fsBlocks) |+
  "" +| padLeftF 10 ' ' (show fsUsed) |+
  "" +| padLeftF 10 ' ' (show fsAvailable) |+
  "" +| padLeftF 5 ' ' (show fsUsePercent ++ "%") |+
  "" +| ' ' : fsMount |+
  ""

-- FIXME --tmpfs or --all
-- FIXME -h
main :: IO ()
main = do
  out <- cmdLines "df" []
  case out of
    [] -> error' "no output from df!"
    (h:ls) -> do
      let --header = readFSLine h
          fs = map readFS ls
      putStrLn h
      mapM_ renderFS $
        groupFS $
        filter (not . ("tmpfs" `isSuffixOf`) . fsName) fs

groupFS :: [DfFileSystem] -> [DfFileSystem]
groupFS ls =
  map combineMounts $ groupSortOn fsUsed ls

combineMounts :: [DfFileSystem] -> DfFileSystem
combineMounts ms =
  let fsmounts = map (fsName &&& fsMount) ms
  in chosen {fsMount = intercalate ", " $ map renderMount fsmounts}
  where
    chosen = head $ last $ sortOn length $ groupSortOn fsName ms

    -- FIXME group filenames: /run/host{,/var{,/home}}
    renderMount (name,mount) =
      if name == fsName chosen then mount else mount ++ "(" ++ name ++ ")"
