{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module CabalUtil (readTree) where

import Import hiding (zip)
import Prelude (zip)
import Control.Monad (filterM, when)
import qualified Data.ByteString.Lazy as BSL
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

readTree :: FilePath -> IO (Either Text Value)
readTree fp = isDirectory fp >>= \case
    False -> return $ Left "we need a dir"
    True -> do
        packageDirFps <- filterM isDirectory =<< listDirectory fp
        let packages = fmap (fp2Text . filename) packageDirFps
        packageVersions <- fmap rights $ mapM readPackageVersions packageDirFps
        allDescs <- mapM (readPackage fp) $ zip packages packageVersions
        return . Right $ toJSON allDescs

readPackage :: FilePath -> (Text, (Text, FilePath, [Text])) -> IO PackageDescription
readPackage root (pkg, (ver, verFp, oldVers)) = do
    let fp = verFp <> fromText (pkg <> ".cabal")
    -- let fp = root <> fromText pkg <> fromText ver <> fromText (pkg <> ".cabal")
    gdesc <- readPackageDescription normal $ unpack $ fp2Text fp
    return $ flattenPackageDescription gdesc
    -- BSL.putStrLn . encode . toJSON $ desc

readPackageVersions :: FilePath -> IO (Either Text (Text, FilePath, [Text]))
readPackageVersions fp = do
    versions :: [(Version, FilePath)] <- fmap
        (catMaybes . fmap readPvp)
        (listDirectory fp)
    -- case  reverse . map (pack . display) $ sortWith fst versions of
    case sortWith fst versions of
        [] -> return $ Left $ " no legal versions for fp: " <> (show fp :: Text)
        (hdVer, hdFp):tl -> return $
            Right (pack $ display hdVer, hdFp, map (pack . display . fst) tl)

readPvp :: FilePath -> Maybe (Version, FilePath)
readPvp fp = fmap (, fp) . simpleParse . unpack . fp2Text $ filename fp
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
    BSL.putStrLn bs
