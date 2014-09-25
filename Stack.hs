-- From Simon Peyton-Jones's "A taste of haskell"
module Stack(Stack,
   -- swap1,
   swap2,
   swap3,
   swap4,
   swap5) where

type Stack w = [w]

-- swap1 :: Stack w -> Stack w
-- swap1 [] = []
-- swap1 (w : []) = w : []
-- swap1 (w1 : w2 : ws) = w2 : w1 : ws

swap2 :: Stack w -> Stack w
swap2 [] = []
swap2 [w] = [w]
swap2 (w1 : w2 : ws) = w2 : w1 : ws

swap3 :: Stack w -> Stack w
swap3 (w1 : w2 : ws) = w2 : w1 : ws
swap3 ws = ws

swap4 :: Stack w -> Stack w
swap4 ws = case ws of
   [] -> []
   [w] -> [w]
   (w1:w2:ws) -> w2:w1:ws

swap5 :: Stack w -> Stack w
swap5 ws = case ws of
   (w1:w2:ws) -> w2:w1:ws
   ws -> ws


-- type TS = Stack Int
-- prop_swap1 :: TS -> Bool
-- prop_swap1 s = swap1 (swap1 s) == s
