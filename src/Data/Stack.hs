-- Ref: https://gitlab.com/rwlock/Stack/-/blob/master/src/Data/Stack.hs
module Data.Stack (
  Stack,
  stackFromList,
  stackNew,
  stackPush,
  stackPeek,
  stackPop,
  stackIsEmpty,
  stackSize,
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
stackFromList :: [a] -> Stack a
stackFromList as = Stack (genericLength as) as

-- | /O(1)/. Create new empty Stack
stackNew :: Stack a
stackNew = Stack 0 []

{- | /O(1)/. Push item onto Stack

> (∀s)(∀x)(stackPop (stackPush x s) == Just (s,x))
-}
stackPush :: a -> Stack a -> Stack a
stackPush item (Stack sz items) = Stack (succ sz) (item : items)

{- | /O(1)/. Pop most recently added item without removing from the Stack

> stackPeek stackNew == Nothing
> (∀x)(∀s)(stackPeek (stackPush s x) == Just x)
> (∀s)(stackPeek s == fmap snd (stackPop s))
-}
stackPeek :: Stack a -> Maybe a
stackPeek (Stack _ []) = Nothing
stackPeek (Stack _ (h : _)) = Just h

{- | /O(1)/. Pop most recently added item from Stack

> stackPop stackNew == Nothing
> (∀x)(∀s)(stackPop (stackPush s x) == Just (s,x))
-}
stackPop :: Stack a -> Maybe (Stack a, a)
stackPop (Stack _ []) = Nothing
stackPop (Stack sz (h : t)) = Just (Stack (pred sz) t, h)

{- | /O(1)/. Test if stack is empty

> stackIsEmpty stackNew == True
> (∀x)(∀s)(stackIsEmpty (stackPush s x) == True)
> (∀s)((stackSize s == 0) ⇔ (stackIsEmpty s == True))
-}
stackIsEmpty :: Stack a -> Bool
stackIsEmpty (Stack _ []) = True
stackIsEmpty (Stack _ _) = False

{- | /O(1)/. Compute number of elements contained in the Stack

> stackSize stackNew == 0
> (∀x)(∀s)((stackSize s == n) ⇒ (stackSize (stackPush s x) == n+1))
-}
stackSize :: Stack a -> Natural
stackSize (Stack sz _) = sz
