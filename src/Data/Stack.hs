-- Ref: https://gitlab.com/rwlock/Stack/-/blob/master/src/Data/Stack.hs
module Data.Stack (
  Stack,
  fromList,
  new,
  push,
  peek,
  pop,
  isEmpty,
  size,
)
where

import Data.List (genericLength)
import Numeric.Natural

-- | Abstract Stack data type
data Stack a = Stack !Natural [a] deriving (Eq, Read, Show)

instance Semigroup (Stack a) where
  (Stack sz1 items1) <> (Stack sz2 items2) = Stack (sz1 + sz2) (items1 <> items2)

instance Monoid (Stack a) where
  mempty = Stack 0 []

-- | /O(n)/. Create new Stack from list (head is top)
fromList :: [a] -> Stack a
fromList as = Stack (genericLength as) as

-- | /O(1)/. Create new empty Stack
new :: Stack a
new = Stack 0 []

{- | /O(1)/. Push item onto Stack

> (∀s)(∀x)(pop (push x s) == Just (s,x))
-}
push :: a -> Stack a -> Stack a
push item (Stack sz items) = Stack (succ sz) (item : items)

{- | /O(1)/. Pop most recently added item without removing from the Stack

> peek new == Nothing
> (∀x)(∀s)(peek (push s x) == Just x)
> (∀s)(peek s == fmap snd (pop s))
-}
peek :: Stack a -> Maybe a
peek (Stack _ []) = Nothing
peek (Stack _ (h : _)) = Just h

{- | /O(1)/. Pop most recently added item from Stack

> pop new == Nothing
> (∀x)(∀s)(pop (push s x) == Just (s,x))
-}
pop :: Stack a -> Maybe (Stack a, a)
pop (Stack _ []) = Nothing
pop (Stack sz (h : t)) = Just (Stack (pred sz) t, h)

{- | /O(1)/. Test if stack is empty

> isEmpty new == True
> (∀x)(∀s)(isEmpty (push s x) == True)
> (∀s)((size s == 0) ⇔ (isEmpty s == True))
-}
isEmpty :: Stack a -> Bool
isEmpty (Stack _ []) = True
isEmpty (Stack _ _) = False

{- | /O(1)/. Compute number of elements contained in the Stack

> size new == 0
> (∀x)(∀s)((size s == n) ⇒ (size (push s x) == n+1))
-}
size :: Stack a -> Natural
size (Stack sz _) = sz
