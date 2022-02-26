-- Problem 46 and 47
not' :: Bool -> Bool
not' True = False
not' False = True

and' :: Bool -> Bool -> Bool
and' False _ = False
and' _ False = False
and' _ _ = True

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

nand' :: Bool -> Bool -> Bool
nand' a b = not' (and' a b)

nor' :: Bool -> Bool -> Bool
nor' a b = not' (or' a b)

xor' :: Bool -> Bool -> Bool
xor' True True = False
xor' False False = False
xor' _ _ = True

impl' :: Bool -> Bool -> Bool
impl' a = or' (not' a)

equ' True True = True
equ' False False = True
equ' _ _ = False

table :: (Bool -> Bool -> Bool) -> IO ()
table expr = mapM_ putStrLn [show p ++ "\t" ++ show q ++ "\t" ++ show (expr p q) | p <- [False, True], q <- [False, True]]

