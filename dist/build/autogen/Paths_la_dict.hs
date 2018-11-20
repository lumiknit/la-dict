{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_la_dict (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/lumiknit/Library/Haskell/bin"
libdir     = "/Users/lumiknit/Library/Haskell/ghc-8.0.2-x86_64/lib/la-dict-0.1.0.0"
dynlibdir  = "/Users/lumiknit/Library/Haskell/ghc-8.0.2-x86_64/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/lumiknit/Library/Haskell/share/ghc-8.0.2-x86_64/la-dict-0.1.0.0"
libexecdir = "/Users/lumiknit/Library/Haskell/libexec"
sysconfdir = "/Users/lumiknit/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "la_dict_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "la_dict_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "la_dict_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "la_dict_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "la_dict_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "la_dict_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
