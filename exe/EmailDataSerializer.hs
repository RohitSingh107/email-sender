


{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module EmailDataSerializer
    ( EmailData (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

data EmailData = EmailData
    { toEmailData :: Maybe Text
    , subjectEmailData :: Maybe Text
    , ccEmailData :: Maybe [Text]
    , bccEmailData :: Maybe [Text]
    , bodyEmailData :: Maybe Text
    , attachmentsEmailData :: Maybe [Text]
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe EmailData
decodeTopLevel = decode

instance ToJSON EmailData where
    toJSON (EmailData toEmailData subjectEmailData ccEmailData bccEmailData bodyEmailData attachmentsEmailData) =
        object
        [ "to" .= toEmailData
        , "subject" .= subjectEmailData
        , "cc" .= ccEmailData
        , "bcc" .= bccEmailData
        , "body" .= bodyEmailData
        , "attachments" .= attachmentsEmailData
        ]

instance FromJSON EmailData where
    parseJSON (Object v) = EmailData
        <$> v .:? "to"
        <*> v .:? "subject"
        <*> v .:? "cc"
        <*> v .:? "bcc"
        <*> v .:? "body"
        <*> v .:? "attachments"
