module Main where

import System.Environment ( getArgs )
import Sandboxes

main :: IO ()
main = do
  createEnv
  (action, extra) <- getArguments
  case action of
    "create"   -> runCommand createSandbox extra
    "c"        -> runCommand createSandbox extra
    "--create" -> runCommand createSandbox extra
    "-c"       -> runCommand createSandbox extra

    "set"      -> runCommand setSandbox extra
    "s"        -> runCommand setSandbox extra
    "--set"    -> runCommand setSandbox extra
    "-s"       -> runCommand setSandbox extra

    "list"     -> listSandboxes
    "l"        -> listSandboxes
    "--list"   -> listSandboxes
    "-l"       -> listSandboxes

    "backup"   -> runCommand backupSystemDirs extra    
    "b"        -> runCommand backupSystemDirs extra
    "--backup" -> runCommand backupSystemDirs extra    
    "-b"       -> runCommand backupSystemDirs extra

    "help"     -> help
    "h"        -> help
    "--help"   -> help
    "-h"       -> help
    _          -> help
    

runCommand :: (String -> t) -> String -> t
runCommand command extra =
  if extra == ""
    then error "This action required 1 argument"
    else command extra
     
     
getArguments :: IO (String, String)
getArguments = do
  a <- getArgs
  case length a of
    0 -> return ("not Enough", "")
    1 -> return (a !! 0, "")
    _ -> return (a !! 0, a !! 1)

help :: IO ()
help = do
  putStrLn "Available Commands:"
  putStrLn "  create | c [Name]    Create a new Sandbox"
  putStrLn "  set    | s [Name]    Set a Sandbox"
  putStrLn "  list   | l           List all Sandboxes"
  putStrLn "  backup | b [Name]    Backup your system GHC / Cabal dirs"