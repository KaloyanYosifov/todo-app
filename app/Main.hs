module Main where
import           Control.Monad (unless)
import           Text.Printf   (printf)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex = mapWithIndex' 0
    where mapWithIndex' _ _ []     = []
          mapWithIndex' i f (x:xs) = f i x:mapWithIndex' (i + 1) f xs

tasks :: String -> String
tasks = unlines . mapWithIndex (printf "[%i] %s") . lines

addTaskFlow :: IO ()
addTaskFlow = do
    text <- readFile "./todos.txt"
    putStrLn $ tasks text
    putStrLn "Add new task"
    task <- getLine
    writeFile "./todos.txt" $ text ++ task ++ "\n"
    main

main :: IO ()
main = do
    putStrLn "Pick commands"
    putStrLn "- Type \"exit\" to exit the program"
    putStrLn "- Type \"add\" to add a new task"
    command <- getLine
    case command of
      "exit" -> return ()
      "add"  -> addTaskFlow
      _      -> main
