module Defragment where

import Compact qualified
import Data.Foldable qualified as Foldable
import Data.IntSet qualified as IntSet
import Data.Sequence (ViewL (EmptyL, (:<)), ViewR (EmptyR, (:>)), (|>))
import Data.Sequence qualified as Seq
import Disk (Disk)
import Disk qualified
import Segment (Id)
import Segment qualified

trace :: a -> b -> b
trace = const id

defragment :: Disk -> Disk
defragment d0 = find d0 mempty ids
  where
    ids = fileIdsDesc d0

    find lhs rhs [] = lhs <> Seq.fromList rhs
    find lhs rhs (i : is) =
      case Seq.viewr lhs of
        ss :> s ->
          case Segment.fileId s of
            Just si
              | si == i ->
                  -- Found file (segment). Try to move it.
                  move mempty ss rhs s is
            Just _ ->
              -- No match, keep moving left while looking.
              find ss (s : rhs) (i : is)
            Nothing ->
              -- No match, keep moving left while looking.
              let rhs' = Compact.combineSegments $ s : rhs
               in find ss rhs' (i : is)
        EmptyR ->
          -- Does not happen unless logic is broken, because ids list
          -- is made from disk segments. The file *must* be present.
          error $ "file id " ++ show i ++ " not present in disk"

    move lhs rhs rrhs seg is =
      case Seq.viewl rhs of
        EmptyL ->
          -- Could not move segment s anywhere to the left. Give up
          -- moving this file (segment) and move on to the next one.
          let lhs' = lhs <> Seq.fromList (seg : rrhs)
           in find lhs' mempty is
        s :< ss ->
          case Segment.fileId s of
            Just _ ->
              -- Segment is not free.
              move (lhs |> s) ss rrhs seg is
            Nothing ->
              -- Free segment. Move in if big enough.
              let ssz = Segment.size s
                  segSz = Segment.size seg
               in case compare ssz segSz of
                    LT ->
                      -- No room. Keep looking for free slot.
                      move (lhs |> s) ss rrhs seg is
                    EQ ->
                      -- Fits exactly. Move segment here and start looking
                      -- for next file to move.
                      let rrhs' =
                            Seq.fromList $
                              Compact.combineSegments $
                                Segment.mkFree segSz : rrhs
                          lhs' =
                            (lhs |> seg) <> ss <> rrhs'
                       in find lhs' mempty is
                    GT ->
                      -- More room than needed. Move segment here, mark
                      -- free space, and start looking for next file to
                      -- move.
                      let rhs' =
                            Seq.fromList $
                              Compact.combineSegments $
                                Segment.mkFree (ssz - segSz) : Foldable.toList ss
                          rrhs' =
                            Seq.fromList $
                              Compact.combineSegments $
                                Segment.mkFree segSz : rrhs
                          lhs' =
                            ((lhs |> seg) <> rhs' <> rrhs')
                       in find lhs' mempty is

fileIdsDesc :: Disk -> [Id]
fileIdsDesc = IntSet.toDescList . IntSet.fromList . Disk.fileIds
