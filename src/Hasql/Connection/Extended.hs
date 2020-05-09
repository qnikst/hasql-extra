{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hasql.Connection.Extended
  ( module Hasql.Connection
  -- * Additional types.
  , PostgresConfig(..)
  , Host(..)
  , Port(..)
  , User(..)
  , Password(..)
  , Database(..)
  , postgresConfigToSettings
  -- $setup
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString
import           Data.Coerce
import qualified Data.Text.Encoding  as Text
import           Data.Word
import           Hasql.Connection
import           System.Environment.Extended

import           Sirius.NamingConventions (aesonOptions)

-- $setup
-- >>> import qualified Data.Yaml as Yaml
-- >>> import qualified Data.ByteString.Char8 as B8
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications


-- | Postgres connection configuration.
data PostgresConfig= PostgresConfig
  { postgresConfigHost     :: !Host
  , postgresConfigPort     :: !Port
  , postgresConfigUser     :: !User
  , postgresConfigPassword :: !Password
  , postgresConfigDatabase :: !Database
  } deriving (Show)

instance FromEnv PostgresConfig where
  fromEnv env = PostgresConfig
    <$> (env .-> "POSTGRES_HOST")
    <*> (env .-> "POSTGRES_PORT")
    <*> (env .-> "POSTGRES_USER")
    <*> (env .-> "POSTGRES_PASSWORD")
    <*> (env .-> "POSTGRES_DATABASE")


postgresConfigToSettings :: PostgresConfig -> Settings
postgresConfigToSettings PostgresConfig {..} = settings
  (coerce postgresConfigHost)
  (coerce postgresConfigPort)
  (coerce postgresConfigUser)
  (coerce postgresConfigPassword)
  (coerce postgresConfigDatabase)

-----------------------------------------------------------------------
--   Boilerplate instances

-- | Database host.
newtype Host = Host ByteString deriving (Show)
instance ParseEnv Host where parseEnv = parseCoerced @ByteString
instance FromJSON Host where
  parseJSON = withText "host" $
    pure . Host . Text.encodeUtf8
instance ToJSON Host where
  toJSON (Host b) = String $ Text.decodeUtf8 b

-- | Database port.
newtype Port = Port Word16 deriving (FromJSON, ToJSON, Show)
instance ParseEnv Port where
  parseEnv = coerce @(String -> Either String Word16) parseEnv

-- | Password representaion.
newtype Password = Password ByteString deriving (Show)
instance ParseEnv Password where parseEnv = parseCoerced @ByteString
instance FromJSON Password where
  parseJSON = withText "password" $
    pure . Password . Text.encodeUtf8
instance ToJSON Password where
  toJSON (Password q) = String $ Text.decodeUtf8 q

-- | User representation.
newtype User = User ByteString deriving (Show)
instance ParseEnv User where parseEnv = parseCoerced @ByteString
instance FromJSON User where
  parseJSON = withText "user" $
    pure . User . Text.encodeUtf8
instance ToJSON User where
  toJSON (User q) = String $ Text.decodeUtf8 q

-- | Database representation
newtype Database = Database ByteString deriving (Show)
instance ParseEnv Database where parseEnv = parseCoerced @ByteString
instance FromJSON Database where
  parseJSON = withText "database" $
    pure . Database . Text.encodeUtf8
instance ToJSON Database where
  toJSON (Database q) = String $ Text.decodeUtf8 q

-- | Instance for reading postgres config
--
-- >>> :{
--   Yaml.decodeEither' @ PostgresConfig $ B8.unlines
--     [ "host: db"
--     , "port: 6578"
--     , "user: cheops-user"
--     , "password: test-password"
--     , "database: cheops"
--     ]
-- :}
-- Right (PostgresConfig {postgresConfigHost = Host "db", postgresConfigPort = Port 6578, postgresConfigUser = User "cheops-user", postgresConfigPassword = Password "test-password", postgresConfigDatabase = Database "cheops"})
deriveJSON (aesonOptions "postgresConfig") ''PostgresConfig
