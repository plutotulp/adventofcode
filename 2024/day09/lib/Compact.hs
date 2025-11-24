module Compact (compact, combineSegments, removeZeroLengthFreeSpace) where

import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Foldable qualified as Foldable
import Data.Sequence (Seq, ViewL (EmptyL, (:<)), ViewR (EmptyR, (:>)), (<|), (|>))
import Data.Sequence qualified as Seq
import Disk (Disk)
import Disk qualified
import Segment (Segment, Size)
import Segment qualified

compact :: Disk -> Disk
compact disk = run disk
  where
    run =
      Seq.fromList
        . removeZeroLengthFreeSpace
        . combineSegments
        . Foldable.toList
        . reAddFreeSpace
        . flip State.execState mempty
        . spool

    reAddFreeSpace = (|> Segment.mkFree (Disk.freeSize disk))

combineSegments :: [Segment] -> [Segment]
combineSegments = go
  where
    go (b1 : b2 : bs) =
      let moveOn = b1 : go (b2 : bs)
          tryMore cs = go (cs : bs)
       in maybe moveOn tryMore $ Segment.combine b1 b2
    go bs = bs

removeZeroLengthFreeSpace :: [Segment] -> [Segment]
removeZeroLengthFreeSpace = filter check
  where
    check s = Segment.isFile s || (Segment.size s == 0)

-- Compacter state is the sequence of compacted disk blocks.
type CompacterState = Seq Segment

spool :: Seq Segment -> State CompacterState ()
spool ss0 = do
  let (files, ss1) = Seq.spanl Segment.isFile ss0
  State.modify' (<> files)
  case Seq.viewl ss1 of
    freeSegment :< ss2 -> do
      case Seq.viewr $ Seq.dropWhileR Segment.isFree ss2 of
        ss3 :> block -> insert (Segment.size freeSegment) block ss3
        EmptyR -> pure ()
    EmptyL -> pure ()

insert :: Size -> Segment -> Seq Segment -> State CompacterState ()
insert sz block ss0 =
  let bs = Segment.size block
   in case compare bs sz of
        LT -> do
          State.modify' (|> block)
          spool $ Segment.mkFree (sz - bs) <| ss0
        EQ -> do
          State.modify' (|> block)
          spool ss0
        GT -> do
          let (b1, b2) = Segment.split block sz
          State.modify' (|> b1)
          spool $ ss0 |> b2
