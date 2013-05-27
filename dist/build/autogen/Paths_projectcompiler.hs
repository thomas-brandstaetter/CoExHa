module Paths_projectcompiler (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/tb/.cabal/bin"
libdir     = "/Users/tb/.cabal/lib/projectcompiler-0.0.1/ghc-7.4.2"
datadir    = "/Users/tb/.cabal/share/projectcompiler-0.0.1"
libexecdir = "/Users/tb/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "projectcompiler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "projectcompiler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "projectcompiler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "projectcompiler_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
