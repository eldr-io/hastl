## Using UUIDs as Database Primary Key Id fields

You may want to switch Hastl to use unique UUIDs as the id for all of the database models, rather than the standard BIGSERIAL that is just an incremental number.

To accomplish this in hastl, you can use the "implicitIdDef" functionality of persistent. 

To get started, create a new UUID type that will be our custom UUID implementation that persistent will map our id fields to, e.g. `src/Api/UUID.hs`:

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Api.UUID where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Text qualified as T
import Database.Persist (LiteralType (Escaped), PersistField (fromPersistValue, toPersistValue), PersistValue (PersistLiteral_), SqlType (SqlOther))
import Database.Persist.ImplicitIdDef (ImplicitIdDef, mkImplicitIdDef)
import Database.Persist.Postgresql (PersistFieldSql (sqlType))
import Servant qualified as Web.Internal
import Web.PathPieces

newtype UUID = UUID ByteString

instance ToJSON UUID where
  toJSON (UUID bs) = toJSON (B8.unpack bs)

instance FromJSON UUID where
  parseJSON v = UUID . B8.pack <$> parseJSON v

instance Show UUID where
  show (UUID bs) = show bs

instance Read UUID where
  readsPrec _ s = [(UUID (read s), "")]

instance Eq UUID where
  (UUID a) == (UUID b) = a == b

instance Ord UUID where
  compare (UUID a) (UUID b) = compare a b

fromByteString :: ByteString -> Maybe UUID
fromByteString bs = Just (UUID bs)

instance Web.PathPieces.PathPiece UUID where
  toPathPiece (UUID bs) = T.pack $ B8.unpack bs
  fromPathPiece s = Just (UUID (B8.pack (T.unpack s)))

instance Web.Internal.ToHttpApiData UUID where
  toUrlPiece (UUID bs) = T.pack $ B8.unpack bs

instance Web.Internal.FromHttpApiData UUID where
  parseUrlPiece s = Right (UUID (B8.pack (T.unpack s)))

instance PersistField UUID where
  toPersistValue (UUID bs) =
    PersistLiteral_ Escaped bs
  fromPersistValue pv =
    case pv of
      PersistLiteral_ Escaped bs ->
        Right (UUID bs)
      _ ->
        Left "nope"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "UUID"

uuidDef :: ImplicitIdDef
uuidDef = mkImplicitIdDef @UUID "gen_random_uuid()"
```
This defines several helpful instances for our custom UUID type, and importantly also maps it to the database function "gen_random_uuid()" which will cause
Postgres to automatically generate a new UUID for us on insertion.

With the new UUID field defined we can now add the setImplicitDef macro, referencing the uuidDef from `src/Api/UUID.hs` to our models persistent block in `src/Models.hs`:

```hs
share
  [ mkPersist (setImplicitIdDef uuidDef sqlSettings)
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
Guest json
    name Text
    email Text
...
```
and now if we re-run our migrations (note that persistent may not be able to automatically change BigSerial Int ids to this new type), hastl should now be using UUIDs for all Ids.
