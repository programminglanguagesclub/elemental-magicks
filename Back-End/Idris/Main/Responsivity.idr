module Main.Responsivity

%default total

-------------------------------------------------------------------------------
public export
data InfIO : Type where
 Do : IO a -> (a -> Inf InfIO) -> InfIO
-------------------------------------------------------------------------------
public export
(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do
-------------------------------------------------------------------------------
data Fuel
 = Dry
 | More (Lazy Fuel)
-------------------------------------------------------------------------------
run : Fuel -> InfIO -> IO ()
run (More fuel) (Do c f) =
 do
 res <- c
 run fuel (f res)
run Dry p = putStrLn "Out of fuel"
-------------------------------------------------------------------------------
tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)
-------------------------------------------------------------------------------
forever : Fuel
forever = assert_total (More forever)
-------------------------------------------------------------------------------
export
runForever : InfIO -> IO ()
runForever x = run forever x
-------------------------------------------------------------------------------
exampleResponsiveServerLoop : InfIO
exampleResponsiveServerLoop = do
  x <- getLine
  putStrLn x
  exampleResponsiveServerLoop
-------------------------------------------------------------------------------
exampleResponsiveServer : IO ()
exampleResponsiveServer = runForever exampleResponsiveServerLoop

