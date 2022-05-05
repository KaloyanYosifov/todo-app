module Main where
import           Text.Printf (printf)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex = mapWithIndex' 0
    where mapWithIndex' _ _ []     = []
          mapWithIndex' i f (x:xs) = f i x:mapWithIndex' (i + 1) f xs

tasks :: String -> String
tasks = unlines . mapWithIndex (printf "[%i] %s") . lines

main :: IO ()
main = do
    text <- readFile "./todos.txt"
    putStrLn $ tasks text
    putStrLn "Add new task"
    task <- getLine
    writeFile "./todos.txt" $ text ++ task ++ "\n"
    return ()
