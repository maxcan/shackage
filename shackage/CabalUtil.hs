{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module CabalUtil where

import Import hiding (zip)
import Prelude (zip)
import Control.Monad (when)
import qualified Data.ByteString.Lazy as BL
-- import Data.List
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
import Distribution.Text hiding (Text)
import Language.Haskell.Extension
-- import System.Environment (getArgs)
import Filesystem
-- import Text.ParserCombinators.ReadP (readP_to_S)
import Filesystem.Path.CurrentOS (fromText, toText)
import Data.Version
import Data.Maybe (catMaybes)

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
deriveJSON id ''Benchmark
deriveJSON id ''BenchmarkType
deriveJSON id ''BenchmarkInterface

fp2Text :: FilePath -> Text; fp2Text = either id id . toText

readTree :: FilePath -> IO (Either Text [Text])
readTree fp = isDirectory fp >>= \case
    False -> return $ Left "we need a dir"
    True -> do
        packageDirFps <- listDirectory fp
        let packages = fmap (fp2Text . filename) packageDirFps
        packageVersions <- fmap rights $ mapM readPackageVersions packageDirFps
        mapM_ (readPackage fp) $ zip packages packageVersions
        return $ error "dsf"

readPackage :: FilePath -> (Text, (Text, [Text])) -> IO ()
readPackage root (pkg, (ver, oldVers)) = do
    let fp = root <> fromText pkg <> fromText ver <> fromText (pkg <> ".cabal")
    gdesc <- readPackageDescription normal $ unpack $ fp2Text fp
    let desc = flattenPackageDescription gdesc
    BL.putStrLn . encode . toJSON $ desc

readPackageVersions :: FilePath -> IO (Either Text (Text, [Text]))
readPackageVersions fp = do
    versions  :: [Version] <- fmap (catMaybes . fmap (readPvp . fp2Text . filename))
                             (listDirectory fp)
    case  reverse . map (pack . display) $ sort versions of
        [] -> return $ Left $ " no legal versions for fp: " <> (show fp :: Text)
        hd:tl -> return $ Right (hd, tl)

readPvp :: Text -> Maybe Version
readPvp = simpleParse . unpack
-- Distribution.Version Prelude Data.Version Text.ParserCombinators.ReadP>
-- let readV v = let l = readP_to_S parseVersion v in fst $ head $ drop (length l - 1) l

main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) $ fail "missing .cabal file"

    let (source:_) = args
    gdesc <- error "cabalutil" -- readPackageDescription normal source
    let desc = flattenPackageDescription gdesc
        bs = encode . toJSON $ desc
    BL.putStrLn bs
