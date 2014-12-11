module Paths_markup (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/athan/dev/markup-0.0.1/.cabal-sandbox/bin"
libdir     = "/home/athan/dev/markup-0.0.1/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.3/markup-0.0.1"
datadir    = "/home/athan/dev/markup-0.0.1/.cabal-sandbox/share/x86_64-linux-ghc-7.8.3/markup-0.0.1"
libexecdir = "/home/athan/dev/markup-0.0.1/.cabal-sandbox/libexec"
sysconfdir = "/home/athan/dev/markup-0.0.1/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "markup_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "markup_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "markup_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "markup_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "markup_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
