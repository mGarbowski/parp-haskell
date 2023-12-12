module Computer where
import Data.List
import System.IO

-- haskeline imports for nicer terminal experience
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline

type Command = String

runComputer :: IO ()
runComputer = do
    execCommandLoop

execCommandLoop :: IO ()
execCommandLoop = runInputT defaultSettings (loop)
  where
    loop = do
            input <- getInputLine "/home/sysy$ "
            case input of
               Nothing -> return ()
               Just inp -> do
                 liftIO $ processCommandInput inp
                 loop

processCommandInput :: Command -> IO ()
processCommandInput input
    | input == "ls" = putStrLn ""
    | input == "ls -la" = putStrLn "-r-------- 1 root root 123 Nov 11 12:34 .pass"
    | input == "ls -a" = putStrLn ".pass"
    | input == "cat .pass" = putStrLn "Permission denied"
    | "cd " `isPrefixOf` input = putStrLn $ "Cannot cd to " ++ (drop 3 input)
    | "cat " `isPrefixOf` input = putStrLn $ "cat: " ++ drop 4 input ++ ": No such file or directory"
    | "sudo " `isPrefixOf` input = passwordLoop (drop 5 input)
    | input == "clear" = putStrLn "\ESC[2J" -- ANSI escape code to clear the terminal
    | input == "exit" = do putStrLn "shutting down"
                           return ()
    | input == "shutdown" = processCommandInput "exit"
    | otherwise = putStrLn $ "Unknown command: " ++ input


passwordLoop :: String -> IO ()
passwordLoop input = runInputT defaultSettings (loop input)
  where
    loop input = do
        pass <- getInputLine "[sudo] password for sysy: "
        case pass of
            Nothing -> return () -- Ctrl-D exits the password loop
            Just "KirgisWalter" -> liftIO $ do
                if input == "cat .pass" then displayPass
                else do
                    processCommandInput input
                    execCommandLoop
            Just "shutdown" -> liftIO $ processCommandInput "shutdown"
            Just "exit" -> liftIO $ processCommandInput "exit"
            Just _ -> do
                outputStrLn "Invalid password."
                loop input

displayPass :: IO()
displayPass = putStrLn("security code: 852611")