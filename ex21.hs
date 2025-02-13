-- This section from Aj.Chin's code
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State g) = State $ \s -> 
        let (x, s') = g s 
        in (f x, s')

instance Applicative (State s) where
    pure x = State $ \s -> (x, s)

    State h <*> State g = State $ \s -> 
        let (f, s')   = h s
            (x', s'') = g s'
        in (f x', s'')

instance Monad (State s) where
    State h >>= f = State $ \s -> 
        let (x, s') = h s
            State g = f x
        in g s'

--implement a queue data structure

type Queue a = ([a], [a], Int)  -- input stack, output stack, size

-- Helper function to move elements if needed
ensureOutput :: State (Queue a) ()
ensureOutput = State $ \(ins, outs, sz) ->
    if null outs then ((), ([], reverse ins, sz)) else ((), (ins, outs, sz))

-- Get the queue size
size :: State (Queue a) Int
size = State $ \(ins, outs, sz) -> (sz, (ins, outs, sz))

-- Check if the queue is empty
isEmpty :: State (Queue a) Bool
isEmpty = State $ \(ins, outs, sz) -> (sz == 0, (ins, outs, sz))

-- Enqueue an element in O(1)
enqueue :: a -> State (Queue a) ()
enqueue x = State $ \(ins, outs, sz) -> ((), (x : ins, outs, sz + 1))

-- Dequeue an element in amortized O(1)
dequeue :: State (Queue a) (Maybe a)
dequeue = do
    ensureOutput  -- Move elements from input to output if necessary
    State $ \(ins, outs, sz) -> case outs of
        []     -> (Nothing, (ins, outs, sz))  -- Queue is empty
        (x:xs) -> (Just x, (ins, xs, sz - 1)) -- Dequeue normally

-- Create a queue from a list in O(n)
mkQueue :: [a] -> State (Queue a) ()
mkQueue xs = State $ \_ -> ((), (xs, [], length xs))

-- Empty the queue
empty :: State (Queue a) ()
empty = State $ \_ -> ((), ([], [], 0))


--implement function joinState
joinState :: State s (State s a) -> State s a
joinState (State f) = State $ \s -> 
    let (State g, s') = f s in g s'  

--prove that state monad satisfies functor laws

-- Identity Law: fmap id = id
-- fmap id (State g)
--    = State (\s -> let (x, s') = g s in (id x, s'))
--    = State (\s -> let (x, s') = g s in (x, s'))  -- since id x = x
--    = State g

-- Composition Law: fmap (f . g) = fmap f . fmap g
-- Right-hand side (fmap f . fmap g)
--   First, apply fmap g
--      fmap g (State h) = State (\s -> let (x, s') = h s in (g x, s'))
--          h s returns (x, s')
--          Applying g to x gives (g x, s')
--   Apply fmap f to State (\s -> (g x, s'))
--      fmap f (State (\s -> let (x, s') = h s in (g x, s')))
--          = State (\s -> let (y, s') = (\s'' -> let (x', s'') = h s'' in (g x', s'')) s
--                         in (f y, s'))
--          = State (\s -> let (x, s') = h s in (f (g x), s'))
-- Left-hand side (fmap (f . g))
--      fmap (f . g) (State h)
--          = State (\s -> let (x, s') = h s in ((f . g) x, s'))
--          = State (\s -> let (x, s') = h s in (f (g x), s'))
