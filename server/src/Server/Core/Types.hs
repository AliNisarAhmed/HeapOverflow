{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.Core.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import RIO

newtype Salt = Salt {getSalt :: Text}
  deriving (Eq, Show, Read)

newtype Password = Password {getPassword :: Text}
  deriving (Eq, Show, FromJSON, ToJSON, Generic)

newtype HashedPassword = HashedPassword {getHashedPassword :: Text}
  deriving (Eq, Show, Read, FromJSON, ToJSON, Generic)
