module Segment where

import Data.Maybe qualified as Maybe

type Id = Int -- file ID

type Size = Word -- size in blocks

type Segment = (Maybe Id, Size)

mkFree :: Size -> Segment
mkFree sz = (Nothing, sz)

isFree :: Segment -> Bool
isFree = Maybe.isNothing . fst

isFile :: Segment -> Bool
isFile = not . isFree

fileId :: Segment -> Maybe Id
fileId = fst

size :: Segment -> Size
size = snd

split :: Segment -> Size -> (Segment, Segment)
split (mid, bsz) ssz = (block1, block2)
  where
    block1 = (mid, min bsz ssz)
    block2 = (mid, max 0 (bsz - ssz))

combine :: Segment -> Segment -> Maybe Segment
combine (Just bid1, sz1) (Just bid2, sz2)
  | bid1 == bid2 = Just (Just bid1, sz1 + sz2)
combine (Nothing, sz1) (Nothing, sz2) =
  Just (Nothing, sz1 + sz2)
combine _ _ = Nothing

showSeg :: Segment -> String
showSeg s =
  concat $
    replicate (fromIntegral $ size s) $
      maybe "." show $
        fileId s
