{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.ByteString            (ByteString)

import qualified Data.ByteString.Char8      as BS

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             ( first
                                            , second
                                            , bimap
                                            )
                                            
import           Data.Monoid                (Last (Last))

import           Control.Exception          ( try
                                            , catch
                                            )
import           Control.Monad                  ( join )

import qualified Data.Attoparsec.ByteString as AB
import          Data.Functor.Contravariant  ((>$<))

import           Waargonaut                 ( Json(..)
                                            , parseWaargonaut
                                            )

import qualified Waargonaut.Decode          as D

import           Waargonaut.Decode.Error    ( DecodeError (ParseFailed)
                                            )

import           Level06.AppM               ( AppM(..)
                                            , liftEither
                                            )

import           Level06.Types              ( ConfigError (..)
                                            , PartialConf (PartialConf)
                                            , partialConfDecoder
                                            )

import           System.IO.Error            ( IOError )

-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> readConfFile "badFileName.no"
-- Left (undefined "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile =
  AppM
  . fmap (first BadConfigFilePath)
  . try
  . BS.readFile

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile =
  AppM
    . fmap (either Left decodePartConf)
    . runAppM
    . readConfFile
  where
    decodePartConf
      :: ByteString
      -> Either ConfigError PartialConf
    decodePartConf =
      first (BadConfFile . fst)
      -- the expression above will reveive
      -- (from the expression bellow) an input of type
      -- :: Either (DecodeError, D.CursorHistory) PartialConf
      . D.runPureDecode partialConfDecoder parseFunc
      . D.mkCursor

    parseFunc :: ByteString -> Either DecodeError Json
    parseFunc =
      first (ParseFailed . pack . show) 
      . AB.parseOnly parseWaargonaut

    --
    -- Previous attempts to solve the exercises:
    --

    -- decodePartConf' :: ByteString -> Either ConfigError PartialConf
    -- decodePartConf' = first (BadConfFile . fst) . decodePartConf

    -- decodePartConf
    --   :: ByteString
    --   -> Either (DecodeError, D.CursorHistory) PartialConf
    -- decodePartConf = D.runPureDecode partialConfDecoder parseFunc . D.mkCursor

    -- ios :: FilePath -> IO (Either ConfigError ByteString)
    -- ios =
    --   runAppM . readConfFile
      
    -- fileToJson :: FilePath -> AppM ConfigError Json
    -- fileToJson =
    --   (>>= liftEither . first BadConfFile . parseFunc)
    --   . readConfFile


-- Go to 'src/Level06/Conf.hs' next.
