{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Server.Database.Model where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Sql
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import GHC.Generics (Generic)
import RIO
import Server.Core.Types (Salt (..))

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

User json 
    firstname Text 
    surname Text 
    username Text 
    email Text 
    pwd Text
    salt Text
    UniqueEmail email 
    UserFullName firstname surname
    UniqueUsername username
    deriving Eq Show 

Question json
    title Text
    content Text
    userId UserId
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Eq Show

Answer json 
    questionId QuestionId 
    content Text 
    authorId UserId 
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Eq Show

|]
