{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_sorths (
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

bindir     = "/home/brunomasunaga/Documents/UFABC/Paradigmas/sorting/.stack-work/install/x86_64-linux-tinfo6/lts-12.4/8.4.3/bin"
libdir     = "/home/brunomasunaga/Documents/UFABC/Paradigmas/sorting/.stack-work/install/x86_64-linux-tinfo6/lts-12.4/8.4.3/lib/x86_64-linux-ghc-8.4.3/sorths-0.1.0.0-BFbltPBr4C8AJpz83bo5pz-sorths"
dynlibdir  = "/home/brunomasunaga/Documents/UFABC/Paradigmas/sorting/.stack-work/install/x86_64-linux-tinfo6/lts-12.4/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/brunomasunaga/Documents/UFABC/Paradigmas/sorting/.stack-work/install/x86_64-linux-tinfo6/lts-12.4/8.4.3/share/x86_64-linux-ghc-8.4.3/sorths-0.1.0.0"
libexecdir = "/home/brunomasunaga/Documents/UFABC/Paradigmas/sorting/.stack-work/install/x86_64-linux-tinfo6/lts-12.4/8.4.3/libexec/x86_64-linux-ghc-8.4.3/sorths-0.1.0.0"
sysconfdir = "/home/brunomasunaga/Documents/UFABC/Paradigmas/sorting/.stack-work/install/x86_64-linux-tinfo6/lts-12.4/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sorths_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sorths_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sorths_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sorths_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sorths_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sorths_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
