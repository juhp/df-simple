{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Tuple.Extra ((&&&))
import Data.List.Extra (dropPrefix, groupSortOn, intercalate, isSuffixOf,
                        sortOn)
import Fmt
import Numeric.Natural
import SimpleCmd

data DfFileSystem a = FS {
  fsName :: String,
  fsBlocks :: a,
  fsUsed :: a,
  fsAvailable :: a,
  fsPercent :: a,
  fsMount :: String
  }

class DfData a where
  readPercent :: String -> a
  showPercent :: a -> String
  readMega :: String -> a
  showMega :: a -> String

instance DfData Text where
  readPercent = T.pack . init
  showPercent = T.unpack
  readMega = T.pack
  showMega = T.unpack

instance DfData Natural where
  readPercent = read . init
  showPercent p = show p ++ "%"
  readMega s = round $ (read s :: Double) / 1000
  showMega n = "" +| commaizeF (toInteger n) |+ "M"

readFS :: DfData a => Bool -> String -> DfFileSystem a
readFS nonheader l =
  case words l of
    name : blocks : used : available : percent : mount : rest ->
      if nonheader && not (null rest)
      then error' $ "df output with more than 6 columns:" +-+ show l
      else FS name (readMega blocks) (readMega used) (readMega available) (readPercent percent) (unwords (mount : rest))
    _ -> error' $ "unexpected df header columns: " ++ show l

-- 500GB:
-- Filesystem     1K-blocks      Used Available Use% Mounted on
-- /dev/dm-0      498426880 215097136 281873616  44% /sysroot

-- 12GB VM:
-- Filesystem     1K-blocks    Used Available Use% Mounted on
-- /dev/vda3       12579840 9469076   2759116  78% /

columnSpacing :: Int
columnSpacing = 1

renderOutput :: DfFileSystem Text -> [DfFileSystem Natural] -> IO ()
renderOutput header fss =
  if null fss
  then error' "no fs output!"
  else
    let render fs =
          let
            nameMax = maxStringField fsName fsName
            blocksMax = maxDataField fsBlocks fsBlocks + columnSpacing
            usedMax = maxDataField fsUsed fsUsed + columnSpacing
            availMax = maxDataField fsAvailable fsAvailable + columnSpacing
            percentMax = 5
          in renderFS nameMax blocksMax usedMax availMax percentMax fs :: IO ()
    in do
      render header
      mapM_ render fss
  where
    maxStringField hfield field =
      maximum $ map length $ hfield header : map field fss

    maxDataField hfield field =
      maximum $ map length $ showMega (hfield header) : map (showMega . field) fss

    renderFS :: DfData a => Int -> Int -> Int -> Int -> Int -> DfFileSystem a
             -> IO ()
    renderFS nameMax blocksMax usedMax availMax percentMax (FS {..}) =
      fmtLn $
      "" +| padRightF nameMax ' ' fsName |+
      "" +| padLeftF blocksMax ' ' (showMega fsBlocks) |+
      "" +| padLeftF usedMax ' ' (showMega fsUsed) |+
      "" +| padLeftF availMax ' ' (showMega fsAvailable) |+
      "" +| padLeftF percentMax ' ' (showPercent fsPercent) |+
      "" +| replicate columnSpacing ' ' ++ fsMount |+ ""

-- FIXME --tmpfs or --all
-- FIXME -h
main :: IO ()
main = do
  out <- cmdLines "df" []
  case out of
    [] -> error' "no output from df!"
    (_h:ls) -> do
      let header = FS "Filesystem" "Size" "Used" "Free" "Use" "Mount"
          fs = map (readFS True) ls
      renderOutput header $
        groupFS $
        filter (not . ("tmpfs" `isSuffixOf`) . fsName) fs

groupFS :: Ord a => [DfFileSystem a] -> [DfFileSystem a]
groupFS ls =
  map combineMounts $ groupSortOn fsUsed ls

combineMounts :: [DfFileSystem a] -> DfFileSystem a
combineMounts ms =
  chosen {fsMount = renderMounts}
  where
    fsmount = fsName &&& fsMount

    commonfss = sortOn length $ groupSortOn fsName ms

    others = init commonfss
    large = last commonfss
    chosen = head large
    mounts = map fsMount large

    renderMounts =
      concatMap ((++ "; ") . intercalate ", " . map (renderMount . fsmount)) others ++ renderCommonPaths mounts

    renderMount (name,mount) =
      if name == fsName chosen then mount else name +-+ mount

    renderCommonPaths [] = ""
    renderCommonPaths [p] = p
    renderCommonPaths paths =
      let
        -- FIXME should be dir
        prefix = commonPrefixAll paths
        subpaths = map (dropPrefix prefix) paths
      in
        if length prefix < 2
        then head paths ++ ", " ++ renderCommonPaths (tail paths)
        else prefix ++ "{" ++
             (if null (head subpaths)
              then "," ++ renderCommonPaths (tail subpaths)
              else intercalate "," subpaths)
             ++ "}"

-- https://stackoverflow.com/questions/21717646/longest-common-prefix-in-haskell

commonPrefix :: Eq e => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []

commonPrefixAll :: Eq a => [[a]] -> [a]
commonPrefixAll = foldl1 commonPrefix
