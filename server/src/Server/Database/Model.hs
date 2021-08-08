{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module Server.Database.Model where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
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
import Data.Text (Text)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

User json 
    firstname Text 
    surname Text 
    email Text 
    deriving Eq Show 

Question json
    title Text
    content Text
    user_id Int
    created_at UTCTime default=now()
    updated_at UTCTime default=now()
    deriving Eq Show

Answer json 
    questionId QuestionId 
    context Text 
    user_id Int 
    created_at UTCTime default=now()
    updated_at UTCTime default=now()
    deriving Eq Show

|]
