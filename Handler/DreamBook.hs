{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.DreamBook (dreamBookAutocomplete, getDreamBook) where

import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text, append, pack, unpack)
import qualified Database.Esqueleto              as E
import           Database.Esqueleto.Internal.Sql (unsafeSqlBinOp)
import           Foundation                      (Handler)
import           Prelude                         hiding (head, init, last,
                                                  readFile, tail, writeFile)
import           Text.Read                       (readMaybe)
import           Yesod                           (Value, lookupGetParam,
                                                  returnJson, runDB, toJSON)

import           Handler.Common                  (Status (..), StatusCode (InvalidParameters))

import           Model

-- data - https://docs.google.com/spreadsheets/d/1QEVJvwrRpGvuvVh2XAKaNQWem5HwYh7q9D7Cs9-2lFE/edit#gid=0
-- TODO add language parameter

ilike :: forall a b c. E.SqlExpr (E.Value a) -> E.SqlExpr (E.Value b) -> E.SqlExpr (E.Value c)
ilike = unsafeSqlBinOp "ILIKE"

sqlSelect :: Maybe Text -> Int -> Handler [Text]
sqlSelect Nothing _ = return []
sqlSelect (Just term) limit = do
   entities <- runDB $ E.selectDistinct $
               E.from $ \dream -> do
               E.where_ $ dream E.^. DreamWord `ilike` E.val (append term (pack "%"))
               E.orderBy [E.asc (dream E.^. DreamWord)]
               E.limit $ fromIntegral limit
               return $ dream E.^. DreamWord
   return $ map (\(E.Value v) -> v) entities

parseLimitParam :: Maybe Text -> Int -> Int
parseLimitParam val defValue = fromMaybe defValue (val >>= readMaybe . unpack :: Maybe Int)

dreamBookAutocomplete :: Handler Value
dreamBookAutocomplete = do
    term  <- lookupGetParam "term"
    limit <- lookupGetParam "limit"
    terms <- sqlSelect term (parseLimitParam limit 5)
    returnJson $ toJSON terms

getDreamBook :: Handler Value
getDreamBook = do
    wordOpt <- lookupGetParam "word"
    case wordOpt of
        Nothing -> return $ toJSON $ Status (fromEnum InvalidParameters) "'word' argument not specified" Nothing
        Just word -> do
            resp <- runDB $ E.select $
                E.from $ \dream -> do
                E.where_ (dream E.^. DreamWord E.==. E.val word)
                return dream
            returnJson $ toJSON (map (\(E.Entity _ v) -> v) resp)
