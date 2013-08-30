{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad (when)
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Aeson
import Data.Aeson.TH
import Distribution.ModuleName (ModuleName)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity
import Distribution.Version
import Distribution.License
import Distribution.Compiler
import Language.Haskell.Extension
import System.Environment (getArgs)

deriveJSON id ''PackageDescription
deriveJSON id ''PackageIdentifier
deriveJSON id ''PackageName
deriveJSON id ''Version
deriveJSON id ''SourceRepo
deriveJSON id ''RepoKind
deriveJSON id ''RepoType
deriveJSON id ''BuildType
deriveJSON id ''Library
deriveJSON id ''Executable
deriveJSON id ''TestSuite
deriveJSON id ''License
deriveJSON id ''CompilerFlavor
deriveJSON id ''TestSuiteInterface
deriveJSON id ''BuildInfo
deriveJSON id ''ModuleName
deriveJSON id ''Dependency
deriveJSON id ''Language
deriveJSON id ''Extension
deriveJSON id ''KnownExtension
deriveJSON id ''TestType
deriveJSON id ''VersionRange

main :: IO ()
main = do
  args <- getArgs
  when (length args == 0) $ fail "missing .cabal file"

  let (source:_) = args
  gdesc <- readPackageDescription normal source
  let desc = flattenPackageDescription gdesc
      bs = encode . toJSON $ desc
  BL.putStrLn bs

