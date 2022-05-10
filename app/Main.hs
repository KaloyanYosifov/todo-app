module Main where
import           Control.Monad (unless)
import           Data.List     (delete)
import           Text.Printf   (printf)
import           Text.Read     (readMaybe)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex = mapWithIndex' 0
    where mapWithIndex' _ _ []     = []
          mapWithIndex' i f (x:xs) = f i x:mapWithIndex' (i + 1) f xs

tasks :: String -> String
tasks = unlines . mapWithIndex (printf "[%i] %s") . lines

printAndWaitForAction :: String -> IO ()
printAndWaitForAction text = do
    putStrLn text
    getChar
    return ()

deleteTaskFlow :: IO ()
deleteTaskFlow = do
    text <- readFile "./todos.txt"
    putStrLn $ tasks text
    putStrLn "Pick a task to delete"
    taskId <- getLine
    case readMaybe taskId :: Maybe Int of
      Just x -> do
            let tasksList = lines text
            if x < 0 || x >= length tasksList
                then
                    printAndWaitForAction "Invalid task Id! Press enter to continue"
                else
                    writeFile "./todos.txt" $ unlines $ delete (tasksList !! x) tasksList

            main

      Nothing -> main

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
    putStrLn "- Type \"delete\" to add a new task"
    command <- getLine
    case command of
      "exit"   -> return ()
      "add"    -> addTaskFlow
      "delete" -> deleteTaskFlow
      _        -> main
