module Lib (parse, solve) where

import Compact qualified
import Data.Word (Word8)
import Defragment qualified
import Disk qualified

type Diskmap = [Word8]

parse :: String -> Diskmap
parse =
  -- "concatMap" and "lines" in order to eat any extra newlines
  concatMap (fmap (read . pure)) . lines

solve :: Diskmap -> IO ()
solve mp = do
  let disk = Disk.mkDisk mp
  putStrLn
    $ "Part 1: Compacted fs checksum is "
    ++ show (Disk.checksum $ Compact.compact disk)
  putStrLn
    $ "Part 1: Defragmented fs checksum is "
    ++ show (Disk.checksum $ Defragment.defragment disk)
