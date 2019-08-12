{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module:      Greedy
Description: Greedy algorithm for balancing a list of expenses.
Maintainer:  alexander.vandenbroucke@gmail.com
-}

module Greedy (
  -- * Greedy Algorithm
  -- $theory
  balance) where

import Types (
  Expense(MkExpense,exp_payer,exp_amount),
  Transfer(MkTransfer, trans_from, trans_to, trans_amount),
  Transferable(executeTransfer))

import           Control.Monad.ST
import           Data.Ord (Down(Down), comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Heap as H
import qualified Data.Vector.Mutable as MV

-------------------------------------------------------------------------------
-- Delta

-- | Relative Expenses
-- A 'Delta' is a wrappper datatype around 'Expense'. It indicates the switch
-- from absolute expenses (modelled by 'Expense') to relative expenses with
-- respect to the average expenses (total expenses divided by the number of
-- people).
newtype Delta a
  = MkDelta
    {
      unDelta :: Expense a
    }
  deriving (Functor, Eq, Ord, Show, Transferable a)

-- | Turn an absolute 'Expense' into a relative 'Delta' given the total
-- average expenses.
fromExpense :: Double -> Expense a -> Delta a
fromExpense avg (MkExpense payer amount) =
  MkDelta (MkExpense payer (amount - avg))

-- | Get the amount in the 'Delta'.
deltaAmount :: Delta a -> Double
deltaAmount = exp_amount . unDelta
{-# INLINEABLE deltaAmount #-}

-- | Get the account in the 'Delta'.
deltaAccount :: Delta a -> a
deltaAccount = exp_payer . unDelta
{-# INLINEABLE deltaAccount #-}

-- | Convert a list of expenses to a list of deltas.
-- For every expense the corresponding delta amount is the difference of the
-- expense to the average expense of the whole list.
toDeltas :: [Expense a] -> V.Vector (Delta a)
toDeltas expenses =
  let n     = length expenses
      total = sum (map exp_amount expenses)
      avg   = total / fromIntegral n
  in V.fromList (map (fromExpense avg) expenses)


-------------------------------------------------------------------------------
-- Greedy Balancing algorithm

-- $theory
-- The high-level idea of the algorithm is as follows: given a list of
-- expenses, find the maximum and minimum expense, and transfer the expenses
-- of the maximum to the minimum, but limit the transfered amount to the
-- smallest deficiency, where deficiency is defined as the absolute value of
-- the difference between the expense and the average expense.
-- Then apply the transfer to the list of expenses, and repeat until the
-- list is @e@-balanced (see 'Types.balanced').
--
-- Note: in some cases, this algorithm will generate transfers that have
-- an amount lower than the given epsilon, for instance, when transfering from
-- many accounts with small deficiencies to a single account with a large
-- deficiency. 
--
-- Note: A related concept to @e@-balance is @d@-deficiency, which is defined
-- as follows:
-- A set of expenses @E@ is @d@-deficient (@d@ > 0) if
--
-- > forall e in |e - avg E| < d where avg E = (sum E)/|E|
--
-- We can then prove that if E is (e/2)-deficient, then it is e-balanced.
--
-- > Let 2d = e, then
-- > E is d-deficient
-- > iff forall e' in E: |e' - avg E| < d
-- > iff forall e1,e2 in E: |e1 - e2| < | avg E + d - (avgE - d) |
-- >                                  = | d + d |
-- >                                  = 2d
-- >                                  = e
-- > iff E is e-balanced (QED)
--
-- However, we can also simply look at the differences of the deficiencies,
-- and apply regular @e@-balance, that is, a list of expenses @E@ is
-- @e@-balanced iff @[e' - avg E | e' <- E]@ is @e@-balanced.


-- | In-place partition a modifiable of deltas into non-positive and positive
-- deltas.
--
-- Partitioning is O(n).
partition
  :: MV.MVector s (Delta a)
  -> ST s (MV.MVector s (Delta a), MV.MVector s (Delta a))
partition deltas = go 0 (MV.length deltas) where
  -- next and posPart are indices dividing the array into 3 regions:
  --
  -- > --------------------------------------
  -- > |  <= 0     | unknown      | > 0     |
  -- > --------------------------------------
  -- > 0            ^              ^         L
  -- >              |              |
  -- >              next           posPart
  --
  -- Thus the following invariants are satisfied:
  -- 
  -- > deltas[i] <= 0                   if i in [0,next)
  -- > deltas[i] is not yet partitioned if i in [next,pos)
  -- > deltas[i] > 0                    if i in [pos,L)
  -- > 0 <= next < posPart <= L
  go !next !posPart
    | next < posPart = do
        MkDelta e <- MV.read deltas next
        if exp_amount e <= 0 then go (next+1) posPart else do
          MV.swap deltas next (posPart-1)
          go next (posPart - 1)
    | otherwise = return $
        let negativePartition = MV.slice 0 posPart deltas
            positivePartition =
              MV.slice posPart (MV.length deltas - posPart) deltas
        in (negativePartition,positivePartition)


-- | Create a list of transfers to e-balance a list of deltas.
-- Given a list of deltas (where every payer must be unique), create a list
-- of transfers, such that the result of applying those transfers to the deltas
-- is e-balanced (see 'Types.balanced').
--
-- Balancing is O(n log n)
balanceDelta :: Eq a => V.Vector (Delta a) -> Double -> ST s [Transfer a]
balanceDelta deltas0 !epsilon
  | V.length deltas0 == 0 = return []
  | otherwise = do
      deltas <- V.thaw deltas0
      (nonPos,pos) <- partition deltas
      minDelta <- mkMinHeap nonPos
      maxDelta <- mkMaxHeap pos
      let loop !transfers = do
            d <- MV.read deltas 0
            payer <- findMin minDelta
            payee <- findMax maxDelta
            if (deltaAmount payee - deltaAmount payer) < epsilon
              then return transfers
              else do
                let amount =
                      min (deltaAmount payee) (negate (deltaAmount payer))
                    transfer =
                      MkTransfer
                      {
                        trans_from   = deltaAccount payer,
                        trans_to     = deltaAccount payee,
                        trans_amount = amount
                      }
                transferMin transfer minDelta
                transferMax transfer maxDelta
                loop (transfer:transfers)

      loop []


-- | Create a list of transfers to e-balance a list of expenses.
-- Given a list of expenses (where every payer must be unique), create a list
-- of transfers, such that the result of applying those transfers to the deltas
-- is e-balanced (see 'Types.balanced').
--
-- This algorithm is O(n log n)
balance :: Eq a => [Expense a] -> Double -> [Transfer a]
balance expenses eps = runST (balanceDelta (toDeltas expenses) eps)

-------------------------------------------------------------------------------
-- Heap datastructures

-- A min heap of 'Delta's
newtype MinHeap s a = MkMinHeap {
  unMinHeap :: MV.MVector s (Delta a)
}

-- A max heap of 'Delta's
newtype MaxHeap s a = MkMaxHeap {
  unMaxHeap :: MV.MVector s (Delta a)
}

-- | Compare deltas by the smallest amount
comparingMin :: Delta a -> Delta a -> Ordering
comparingMin = comparing (Down . deltaAmount)
{-# INLINEABLE comparingMin #-}

-- | Compare deltas by the largest amount
comparingMax :: Delta a -> Delta a -> Ordering
comparingMax = comparing deltaAmount
{-# INLINEABLE comparingMax #-}

-- | Create a min heap.
--
-- O(n)
mkMinHeap :: MV.MVector s (Delta a) -> ST s (MinHeap s a)
mkMinHeap mv = do
  H.heapify comparingMin mv 0 (MV.length mv)
  return (MkMinHeap mv)
{-# INLINEABLE mkMinHeap #-}

-- | Create a max heap
--
-- O(n)
mkMaxHeap :: MV.MVector s (Delta a) -> ST s (MaxHeap s a)
mkMaxHeap mv = do
  H.heapify comparingMax mv 0 (MV.length mv)
  return (MkMaxHeap mv)
{-# INLINEABLE mkMaxHeap #-}

-- | Find the delta with the smallest amount
--
-- O(1)
findMin :: MinHeap s a -> ST s (Delta a)
findMin (MkMinHeap mv) = MV.read mv 0
{-# INLINEABLE findMin #-}

-- | Find the delta with the largest amount
--
-- O(1)
findMax :: MaxHeap s a -> ST s (Delta a)
findMax (MkMaxHeap mv) = MV.read mv 0
{-# INLINEABLE findMax #-}

-- | Apply the transfer to the delta with the smallest amount.
--
-- O(n log n), since it has to preserve the heap property.
transferMin :: Eq a => Transfer a -> MinHeap s a -> ST s ()
transferMin t (MkMinHeap mv) = do
  MV.modify mv (executeTransfer t) 0
  H.popTo comparingMin mv 0 (MV.length mv) 0

-- | Apply the transfer to the delta with the largest amount.
--
-- O(n log n), since it has to preserve the heap property.
transferMax :: Eq a => Transfer a -> MaxHeap s a -> ST s ()
transferMax t (MkMaxHeap mv) = do
  MV.modify mv (executeTransfer t) 0
  H.popTo comparingMax mv 0 (MV.length mv) 0
