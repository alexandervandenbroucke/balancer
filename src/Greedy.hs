{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | Greedy algorithm for balancing a list of expenses.
-}
module Greedy (
  -- * Greedy Algorithm
  -- $theory
  balance) where

import Types (
  Expense(MkExpense,exp_payer,exp_amount),
  Transfer(MkTransfer, trans_from, trans_to, trans_amount),
  Transferable(executeTransfer))

import Data.List (minimumBy,maximumBy)
import Data.Function (on)

-------------------------------------------------------------------------------
-- Delta

newtype Delta a
  = MkDelta
    {
      unDelta :: Expense a
    }
  deriving (Functor, Eq, Ord, Show, Transferable a)

-- | Create a Delta directly from a payer and  an amount.
mkDelta :: a -> Double -> Delta a
mkDelta payer amount = MkDelta (MkExpense payer amount)

-- | Get the amount in the 'Delta.
deltaAmount :: Delta a -> Double
deltaAmount = exp_amount . unDelta

createTransfer :: Delta a -> Delta a -> Transfer a
createTransfer (MkDelta expense1) (MkDelta expense2) =
  let amount = min (abs (exp_amount expense1)) (abs (exp_amount expense2))
  in MkTransfer (exp_payer expense2) (exp_payer expense1) amount

-- | Convert a list of expenses to a list of deltas.
-- For every expense the corresponding delta amount is the difference of the
-- expense to the average expense of the whole list.
toDeltas :: [Expense a] -> [Delta a]
toDeltas expenses =
  let n     = length expenses
      total = sum (map exp_amount expenses)
      avg   = total / fromIntegral n
  in [mkDelta payer (amount-avg) | MkExpense payer amount <- expenses]
    
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
-- Note: A related concept to @e@-balance is @d@-defiency, which is defined as
-- follows:
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
balanceDelta :: Eq a => [Delta a] -> Double -> [Transfer a]
balanceDelta deltas epsilon =
  let payer = minimumBy (compare `on` deltaAmount) deltas
      payee = maximumBy (compare `on` deltaAmount) deltas
  in if (deltaAmount payee - deltaAmount payer) < epsilon then
       []
     else
       let t = createTransfer payee payer
       in t:balanceDelta (map (executeTransfer t) deltas) epsilon

-- | Create a list of transfers to e-balance a list of expenses.
-- Given a list of expenses (where every payer must be unique), create a list
-- of transfers, such that the result of applying those transfers to the deltas
-- is e-balanced (see 'Types.balanced').
balance :: Eq a => [Expense a] -> Double -> [Transfer a]
balance = balanceDelta . toDeltas
