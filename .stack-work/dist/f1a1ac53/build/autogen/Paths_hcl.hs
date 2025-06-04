{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hcl (
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
bindir     = "D:\\Work\\c51_6_HCL\\.stack-work\\install\\3250c20f\\bin"
libdir     = "D:\\Work\\c51_6_HCL\\.stack-work\\install\\3250c20f\\lib\\x86_64-windows-ghc-9.4.8\\hcl-0.1.0.0-nVPrpibTuz9ZD9y1q5I2g"
dynlibdir  = "D:\\Work\\c51_6_HCL\\.stack-work\\install\\3250c20f\\lib\\x86_64-windows-ghc-9.4.8"
datadir    = "D:\\Work\\c51_6_HCL\\.stack-work\\install\\3250c20f\\share\\x86_64-windows-ghc-9.4.8\\hcl-0.1.0.0"
libexecdir = "D:\\Work\\c51_6_HCL\\.stack-work\\install\\3250c20f\\libexec\\x86_64-windows-ghc-9.4.8\\hcl-0.1.0.0"
sysconfdir = "D:\\Work\\c51_6_HCL\\.stack-work\\install\\3250c20f\\etc"

getBinDir     = catchIO (getEnv "hcl_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hcl_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hcl_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hcl_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hcl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hcl_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
