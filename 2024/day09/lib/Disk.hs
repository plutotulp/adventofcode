module Disk where

import Data.Foldable qualified as Foldable
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq, (|>))
import Data.Word (Word8)
import Segment (Id, Segment, Size)
import Segment qualified

type Disk = Seq Segment

data BlockType = File | Free
  deriving (Eq)

mkDisk :: [Word8] -> Disk
mkDisk = snd . foldl' step ((File, 0), mempty)
  where
    step ((btype, fid), acc) s =
      case (btype, fromIntegral s) of
        (File, sz) ->
          ((Free, fid + 1), acc |> (Just fid, sz))
        (Free, 0) ->
          -- disregard free space of zero size
          ((File, fid), acc)
        (Free, sz) ->
          ((File, fid), acc |> (Nothing, sz))

checksum :: Disk -> Word
checksum = fst . foldl' step (0, 0)
  where
    step (acc, off) seg =
      let ssz = Segment.size seg
          fid = Maybe.fromMaybe 0 $ Segment.fileId seg
          val = sum ((* fromIntegral fid) <$> [off .. (off + ssz - 1)])
          off' = off + ssz
          !acc' = acc + val
       in (acc', off')

totalSize :: Disk -> Size
totalSize =
  sum . fmap Segment.size . Foldable.toList

freeSize :: Disk -> Size
freeSize =
  sum . fmap Segment.size . filter Segment.isFree . Foldable.toList

fileIds :: Disk -> [Id]
fileIds = Maybe.mapMaybe Segment.fileId . Foldable.toList

showDisk :: Disk -> String
showDisk = concatMap Segment.showSeg
