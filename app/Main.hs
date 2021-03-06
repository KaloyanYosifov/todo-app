module Main where
import           Data.List   (delete)
import           Text.Printf (printf)
import           Text.Read   (readMaybe)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex = mapWithIndex' 0
    where mapWithIndex' _ _ []     = []
          mapWithIndex' i f (x:xs) = f i x:mapWithIndex' (i + 1) f xs

tasks :: String -> String
tasks = unlines . mapWithIndex (printf "[%i] %s") . lines

printSeparator :: IO ()
printSeparator =   putStrLn "\n----------------------------------\n"

isInRange :: Int -> [String] -> Bool
isInRange index xs = index >= 0 && index < length xs

printAndWaitForAction :: String -> IO ()
printAndWaitForAction text = do
    printSeparator
    putStrLn text
    putStrLn "Press enter to continue"
    getChar
    return ()

deleteTaskFlow :: IO ()
deleteTaskFlow = do
    text <- readFile "./todos.txt"
    printSeparator
    putStrLn $ tasks text
    printSeparator
    putStrLn "Pick a task to delete"
    taskId <- getLine
    case readMaybe taskId :: Maybe Int of
      Just x -> do
            let tasksList = lines text
            if not $ isInRange x tasksList
                then
                    printAndWaitForAction "Invalid task Id!"
                else
                    do
                    writeFile "./todos.txt" $ unlines $ delete (tasksList !! x) tasksList
                    printAndWaitForAction "Sucessfully deleted task!"

            main

      Nothing -> main

editTaskFlow :: IO ()
editTaskFlow = do
    text <- readFile "./todos.txt"
    printSeparator
    putStrLn $ tasks text
    printSeparator
    putStrLn "Pick a task to edit"
    taskId <- getLine
    case readMaybe taskId :: Maybe Int of
      Just x -> do
            let tasksList = lines text
            if not $ isInRange x tasksList
                then
                    printAndWaitForAction "Invalid task Id!"
                else
                    do
                    putStrLn "Start typing..."
                    editedTask <- getLine
                    writeFile "./todos.txt" $ unlines $ mapWithIndex (\index todo -> if index == x then editedTask else todo) tasksList

            main

      Nothing -> main
    main

addTaskFlow :: IO ()
addTaskFlow = do
    text <- readFile "./todos.txt"
    printSeparator
    putStrLn $ tasks text
    printSeparator
    putStrLn "Add new task"
    task <- getLine
    writeFile "./todos.txt" $ text ++ task ++ "\n"
    printAndWaitForAction "Sucessfully added a task!"
    main

main :: IO ()
main = do
    putStrLn "Pick commands"
    putStrLn "- Type \"exit\" to exit the program"
    putStrLn "- Type \"add\" to add a new task"
    putStrLn "- Type \"edit\" to edit a task"
    putStrLn "- Type \"delete\" to add a new task"
    command <- getLine
    case command of
      "exit"   -> return ()
      "add"    -> addTaskFlow
      "edit"   -> editTaskFlow
      "delete" -> deleteTaskFlow
      _        -> main
