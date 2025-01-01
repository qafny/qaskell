{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_interpreter (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/liyili2/project/ham_simulation_projects/qaskell_impl/interpreter/.stack-work/install/x86_64-linux/17e1b7b71fe375bbdf074ecf10d39110afa32b717ba973f0d2ba2ea4b05d1ba7/9.6.5/bin"
libdir     = "/home/liyili2/project/ham_simulation_projects/qaskell_impl/interpreter/.stack-work/install/x86_64-linux/17e1b7b71fe375bbdf074ecf10d39110afa32b717ba973f0d2ba2ea4b05d1ba7/9.6.5/lib/x86_64-linux-ghc-9.6.5/interpreter-0.1.0.0-6UUqPUDwk5pIQIFzq3m1XQ"
dynlibdir  = "/home/liyili2/project/ham_simulation_projects/qaskell_impl/interpreter/.stack-work/install/x86_64-linux/17e1b7b71fe375bbdf074ecf10d39110afa32b717ba973f0d2ba2ea4b05d1ba7/9.6.5/lib/x86_64-linux-ghc-9.6.5"
datadir    = "/home/liyili2/project/ham_simulation_projects/qaskell_impl/interpreter/.stack-work/install/x86_64-linux/17e1b7b71fe375bbdf074ecf10d39110afa32b717ba973f0d2ba2ea4b05d1ba7/9.6.5/share/x86_64-linux-ghc-9.6.5/interpreter-0.1.0.0"
libexecdir = "/home/liyili2/project/ham_simulation_projects/qaskell_impl/interpreter/.stack-work/install/x86_64-linux/17e1b7b71fe375bbdf074ecf10d39110afa32b717ba973f0d2ba2ea4b05d1ba7/9.6.5/libexec/x86_64-linux-ghc-9.6.5/interpreter-0.1.0.0"
sysconfdir = "/home/liyili2/project/ham_simulation_projects/qaskell_impl/interpreter/.stack-work/install/x86_64-linux/17e1b7b71fe375bbdf074ecf10d39110afa32b717ba973f0d2ba2ea4b05d1ba7/9.6.5/etc"

getBinDir     = catchIO (getEnv "interpreter_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "interpreter_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "interpreter_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "interpreter_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "interpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "interpreter_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
