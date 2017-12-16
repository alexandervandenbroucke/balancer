{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

{- | Greedy algorithm for balancing a list of expenses. -}

module Greedy (
  -- * Greedy Algorithm
  -- $theory
  balance) where

import Types (
  Expense(MkExpense,exp_payer,exp_amount),
  Transfer(MkTransfer, trans_from, trans_to, trans_amount),
  Transferable(executeTransfer))

import           Control.Monad.ST
import qualified Data.Vector as V
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


-- | Create a list of transfers to e-balance a list of deltas.
-- Given a list of deltas (where every payer must be unique), create a list
-- of transfers, such that the result of applying those transfers to the deltas
-- is e-balanced (see 'Types.balanced').
balanceDelta :: Eq a => V.Vector (Delta a) -> Double -> ST s [Transfer a]
balanceDelta deltas0 !epsilon
  | V.length deltas0 == 0 = return []
  | otherwise = do
      deltas <- V.thaw deltas0
      let minMax !i !iMin !iMax !dMin !dMax
            | i == MV.length deltas = return (iMin,iMax,dMin,dMax)
            | otherwise = do
                d <- MV.read deltas i
                if | deltaAmount d < deltaAmount dMin
                     -> minMax (i+1) i iMax d dMax
                   | deltaAmount d > deltaAmount dMax
                     -> minMax (i+1) iMin i dMin d
                   | otherwise
                     -> minMax (i+1) iMin iMax dMin dMax
      let loop !transfers = do
            d <- MV.read deltas 0
            (payerIndex,payeeIndex,payer,payee) <- minMax 1 0 0 d d
            if (deltaAmount payee - deltaAmount payer) < epsilon
              then return transfers
              else do
                let amount =
                      min (abs (deltaAmount payee)) (abs (deltaAmount payer))
                    transfer =
                      MkTransfer
                      {
                        trans_from   = deltaAccount payer,
                        trans_to     = deltaAccount payee,
                        trans_amount = amount
                      }
                MV.modify deltas (executeTransfer transfer) payerIndex
                MV.modify deltas (executeTransfer transfer) payeeIndex
                loop (transfer:transfers)

      loop []
-- | Create a list of transfers to e-balance a list of expenses.
-- Given a list of expenses (where every payer must be unique), create a list
-- of transfers, such that the result of applying those transfers to the deltas
-- is e-balanced (see 'Types.balanced').
balance :: Eq a => [Expense a] -> Double -> [Transfer a]
balance expenses eps = runST (balanceDelta (toDeltas expenses) eps)
