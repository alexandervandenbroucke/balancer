{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Types (
  -- * Expenses
  Expense(MkExpense,exp_amount,exp_payer),
  aggregate,
  balanced,
  -- * Transfers
  Transfer(MkTransfer,trans_to,trans_from,trans_amount),
  prettyTransfers,
  -- * Transferable
  Transferable(executeTransfer), executeTransfers)
where
  
import Data.List (groupBy, sortBy)
import Data.Function (on)
import Control.Monad (forM_)


-------------------------------------------------------------------------------
-- Expenses

-- | An expense is an amount of money (a 'Double') expended by someone (of type
-- @a@).
data Expense a =
  MkExpense
  {
    exp_payer  :: !a,
    exp_amount :: !Double
  }
  deriving (Functor,Show,Eq,Ord)

-- | Aggregate a list of expenses by payer.
--   In the returned list, all Expenses will have a unique payer.
aggregate :: Ord a => [Expense a] -> [Expense a]
aggregate expenses =
  let sorted = sortBy (compare `on` exp_payer) expenses
      grouped = groupBy ((==) `on` exp_payer) sorted
  in [MkExpense (exp_payer (head l)) (sum (map exp_amount l)) | l <- grouped]

-- | Check if a list of expenses is balanced up to a small epsilon.
--   That is, the amounts differ by at most epsilon.
--   More formally, we say that a list of expenses E is e-balanced if
--
--   >  forall e1 e2. |e1 - e2| < e
--   
balanced :: Double -> [Expense a] -> Bool
balanced epsilon expenses = 
  let amounts = map exp_amount expenses
  in (maximum amounts - minimum amounts) < epsilon


-------------------------------------------------------------------------------
-- Transfers

-- | A transfer transfers an amount of money ('Double') from a person of type
-- @a@ to another person of type @a@.
data Transfer a =
  MkTransfer
  {
    trans_from   :: a,
    trans_to     :: a,
    trans_amount :: Double
  }
  deriving (Functor,Show)

-- | Pretty print a list of @'Transfer' 'String']@.
prettyTransfers :: [Transfer String] -> IO ()
prettyTransfers transfers = do
  forM_ transfers $ \(MkTransfer from to amount) ->
    putStrLn (from ++ " -> " ++ to ++ ": " ++ show amount)

-------------------------------------------------------------------------------
-- Transferable

-- | Types that can accept a transfer.
class Transferable a t | t -> a where
  -- | The result of executing a transfer where the from and to are identical
  --   must be the identity function.
  executeTransfer :: Transfer a -> t -> t

-- | Execute a list of transfers.
--   By calling 'executeTransfer' on each transfer in succession.
executeTransfers :: Transferable a t => [Transfer a] -> t -> t
executeTransfers transfers expenses = foldr executeTransfer expenses transfers

instance Eq a => Transferable a (Expense a) where
  -- | The result of executing a 'Transfer' on a 'Transferable' is the
  -- following:
  --   1. If the payer and the payee of the transfer are identical, the
  --      expense unchanged.
  --   2. If the payer of the transfer is the payer of the expense, the
  --      expense amount increases by the transfer amount.
  --   3. If the payee of the transfer is the payer of the expense, the
  --      expense is reduced by the transfer amount.
  --   4. Otherwise, the expense is unchanged.
  executeTransfer (MkTransfer from to amount) e@(MkExpense payer expense)
    | from == to    = e
    | from == payer = MkExpense payer (expense + amount)
    | to   == payer = MkExpense payer (expense - amount)
    | otherwise     = e
  {-# INLINEABLE executeTransfer #-}
  {-# SPECIALISE executeTransfer :: Transfer String -> Expense String -> Expense String #-}
