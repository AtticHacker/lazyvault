{-
Copyright 2013 Kevin van Rooijen
    
This file is part of LazyVault.

LazyVault is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, 
either version 3 of the License, or (at your option) any later version.

LazyVault is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with LazyVault. If not, see http://www.gnu.org/licenses/.
-}

module Sandboxes where

import System.Posix.Files ( getSymbolicLinkStatus
                          , isSymbolicLink
                          , createSymbolicLink
                          )

import System.Directory ( doesDirectoryExist
                        , createDirectory
                        , getAppUserDataDirectory
                        , removeDirectoryRecursive
                        , getDirectoryContents
                        , removeFile
                        , renameDirectory
                        )
import Data.Foldable ( for_ )
import System.FilePath.Posix ((</>))
import System.IO

data CurrentDir = DSym | DDir | DEmpty deriving (Show)

lazyDir          :: IO FilePath
lazyDirSandboxes :: IO FilePath
homeGhc          :: IO FilePath
homeCabal        :: IO FilePath
currentSBFile    :: IO FilePath

lazyDir          = getAppUserDataDirectory   "lazyVault"
lazyDirSandboxes = getAppUserDataDirectory $ "lazyVault" </> "sandboxes"
currentSBFile    = getAppUserDataDirectory $ "lazyVault" </> ".current"
homeGhc          = getAppUserDataDirectory   "ghc"
homeCabal        = getAppUserDataDirectory   "cabal"


(<:>) :: [a -> Bool] -> [a] -> [a]
xs <:>  a = foldl (\acc x -> filter x acc) a xs

binPaths :: String
binPaths = "paths=`find /home/kevin/.lazyVault/* -type d | grep 'cabal/bin' | sed ':a;N;$!ba;s/\\n/ /g'`\nexport PATH=$PATH:${paths// /:}"

createEnv :: IO ()
createEnv = do
  lazyDirExist <- lazyDir >>= doesDirectoryExist
  if lazyDirExist == True
    then return ()
    else do
      sequence [ lazyDir, lazyDirSandboxes ] >>=
        mapM_ createDirectory
      lazyDir >>= \d -> writeFile (d </> "binPaths") binPaths
      lazyDir >>= \d -> writeFile (d </> ".current") "none"

createSandbox :: FilePath -> IO ()
createSandbox name = do
  createEnv
  lazyD      <- lazyDirSandboxes
  let projectDir = lazyD </> name
  let ghc   = projectDir </> "ghc"
      cabal = projectDir </> "cabal"
  exist <- (doesDirectoryExist projectDir)
  if exist == True
    then error "Project already Exists"
    else  mapM_ createDirectory $ [projectDir,ghc,cabal]

sandboxExist :: String -> IO Bool
sandboxExist name = do
  lazy <- lazyDirSandboxes
  doesDirectoryExist $ (lazy </> name)


writeCurrent :: String -> IO ()
writeCurrent sb = lazyDir >>= \a -> writeFile (a </> ".current") sb

removeSandbox :: IO String -> IO ()
removeSandbox sb = do
  dir <- sb
  exist <- lazyDirSandboxes >>= \l -> doesDirectoryExist $ l </> dir
  if exist == True
    then do
      putStrLn "Are you sure you want to remove this sandbox?"
      answer <- getLine
      case answer of
        "no" -> return ()
        "yes" -> do
          lazyDirSandboxes >>= \l -> removeDirectoryRecursive $ l </> dir
          file      <- currentSBFile
          handle    <- openFile file ReadMode
          contents  <- hGetContents handle
          if contents == dir
            then do
              writeCurrent "none"
              sequence [homeGhc, homeCabal] >>= mapM_ removeFile
            else return ()
          hClose handle
        _ ->  do putStrLn "Please answer yes or no"
                 removeSandbox sb
    else error "Sandbox doesn't exist."

setSandbox :: String -> IO ()
setSandbox name = do
  sandboxExist name >>= \bool ->
    if bool == True
      then do
        dirList <- mapM checkDir [homeGhc, homeCabal]
        let dirTuples = zip dirList [homeGhc, homeCabal]
        for_ dirTuples $ \dir ->
          case (fst dir) of
            DSym   -> snd dir >>= removeFile 
            DDir   -> error "Move your .ghc and/or .cabal directory. Or use backup [Name]" 
            DEmpty -> return ()
        [ghc, cabal] <- getSandboxDirs name
        [newGhc, newCabal] <- sequence [homeGhc, homeCabal]
        createSymbolicLink ghc newGhc
        createSymbolicLink cabal newCabal
        writeCurrent name
        return ()
      else print "Sanbox Doesn't exist"

listSandboxes :: IO ()
listSandboxes = do
  sandBoxes <- lazyDirSandboxes >>= getDirectoryContents
  file      <- currentSBFile
  handle    <- openFile file ReadMode
  contents  <- hGetContents handle
  putStrLn "Available sandboxes:"
  putStrLn $ contents ++ " **"
  hClose handle
  for_ ([(/="."),(/=".."),(/=contents)] <:> sandBoxes) putStrLn

backupSystemDirs :: String -> IO ()
backupSystemDirs name = do
  lazy       <- lazyDirSandboxes
  ghcExist   <- checkDir homeGhc
  cabalExist <- checkDir homeCabal
  createDirectory $ lazy </> name
  for_ [ (ghcExist,   "ghc")
       , (cabalExist, "cabal")] $ \a ->
    case fst a of
      DEmpty -> return ()
      DSym   -> homeP (snd a) >>= removeFile
      DDir   -> homeP (snd a) >>= \path -> 
        renameDirectory path $ lazy </> name </> (snd a)
  where homeP a = getAppUserDataDirectory a

getSandboxDirs :: String -> IO [String]
getSandboxDirs name = do
  location <- lazyDirSandboxes >>= \a -> return $ (</>name) a
  return location >>= \a -> return $ map (a </>) ["ghc","cabal"]

checkDir :: IO FilePath -> IO CurrentDir
checkDir path = do
  pathname <- path
  exist    <- doesDirectoryExist pathname
  if exist == False
    then return DEmpty
    else getSymbolicLinkStatus pathname >>= \b ->
      if isSymbolicLink b == False
        then return DDir
        else return DSym
