



main : IO ()
main = do
  line <- getLine
  if (undefined == line)
     then return ()
     else do
       putStrLn $ reverseWords line
       main 
  
  
  {-do
  args <- getArgs
  content <- readFile (args !! 0)
  pure (let linesOfFile = lines content in ())
  -}

















