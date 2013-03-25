

--fuck gen = MWC.initialize (V.singleton 100) >>= MWC.uniformR (0, 100) :: ST t Int
--
--newtype State s a = State { run :: s -> (a, s) }
--data State s a = State (s -> (a, s))

--run (State f) = f

-- >>= :: m a -> (a -> m b) -> m b
-- >>= :: State s a -> (a -> State s b) -> State s b
-- >>= :: State (s -> (a, s)) -> (a -> State (s -> (b, s))) -> State (s -> (b, s))

--instance Monad (State s) where
--    return a = State $ \s -> (a, s)
--    State run >>= f = State $ \s -> let (a, s') = run s 
--                                        State f' = f a
--                                    in  f' s
--data Stack a = Stack [a] deriving (Show)

--instance Monad Stack where
--    return a = Stack [a]
--    Stack [] >>= f = Stack []
--    Stack (n:ns) >>= f = Stack (n' ++ rest)
--                        where   Stack n' = f n
--                                Stack rest = Stack ns >>= f


--push :: Int -> State Stack ()
--push a = do
--    put a
--    return ()


--push :: Int -> State Stack ()
--push a = StateT $ \as -> ((), a:as)

--pop :: ST Stack Int
--pop = StateT $ \(x:xs) -> (x, xs)

--f = pop >> push 4

--pop (Stack []) = ((), Stack [])
--pop (Stack (x:xs)) = (x, Stack xs)
--push n (Stack ns) = ((), Stack (n:ns))
