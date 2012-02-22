#!/usr/bin/env runhaskell

import Control.Monad
import Control.Exception
import Control.Applicative

import Data.Char
import Data.List
import Data.Maybe

import Data.Accessor

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.ModuleName hiding (main)
import Distribution.PackageDescription
import Distribution.Verbosity
import Distribution.Simple.Utils
import System.Process
import System.Exit
import System.FilePath
import System.Directory

protoDir :: FilePath
protoDir = "path/to/proto/files"

destDir :: FilePath
destDir = "dist/build/autogen"

-- accessors to make subsubrecord access a bit less mess...
accLocalPkgDescr :: Accessor LocalBuildInfo PackageDescription
accLocalPkgDescr = accessor localPkgDescr (\a r -> r {localPkgDescr = a})

-- go through Maybe hoping that it is not empty
accLibrary :: Accessor PackageDescription Library
accLibrary = accessor (fromJust . library) (\a r -> r {library = Just a})

accExposedModules :: Accessor Library [ModuleName]
accExposedModules = accessor exposedModules (\a r -> r {exposedModules = a})

-- compile .proto file and return a list of generated modules
protoCompile :: Verbosity -> FilePath -> IO [String]
protoCompile v src = do
  notice v $ "Compiling proto definition: " ++ src
  let args = ["--proto_path=" ++ protoDir,"--haskell_out=" ++ destDir, protoDir </> src]
  out <- readProcess "hprotoc" args ""
  return $ map (map (\c -> if c == '/' then '.' else c) . (takeWhile (/= '.'))) . 
    catMaybes . map (stripPrefix $ destDir ++ "/") . lines $ out

main = let hooks = simpleUserHooks 
        in defaultMainWithHooks hooks {confHook = myConfHook}

myConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
myConfHook (pd, hbi) cf = do
  lbi <- mainConf (pd, hbi) cf
  let verb = fromFlag $ configVerbosity $ cf
  protoFiles <- filter (".proto" `isSuffixOf`) <$> getDirectoryContents protoDir
  modList <- nub . map fromString . concat <$> mapM (protoCompile verb) protoFiles
  return $ (accLocalPkgDescr ^: accLibrary ^: accExposedModules ^: (++modList)) lbi

mainConf = confHook simpleUserHooks
