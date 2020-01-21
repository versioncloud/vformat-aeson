{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Exception
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Maybe
import           GHC.Generics
import           Text.Format
import           Text.Format.Aeson

main :: IO ()
main = do
  putStrLn "\n<<<<<<<<<<<<<<<<<<<<<< Test <<<<<<<<<<<<<<<<<<<<<<"

  it "format JSON Null" $ format "{}" Null == ("null" :: String)
  it "format JSON Bool" $ format "{}" (toJSON True) == ("true" :: String)
  it "format JSON String" $
    format "{}" (toJSON ("hello world" :: String)) == ("hello world" :: String)
  it "format JSON Integer Number" $
    format "{}" (toJSON (123456 :: Int)) == ("123456" :: String)
  it "format JSON Float Number" $
    format "{:.4f}" (toJSON (123.456789 :: Float)) == ("123.4568" :: String)
  it "format JSON Array" $
    format1 "{} {} {}" (toJSON $ Flag Blue 100 50) == ("Blue 100 50" :: String)
  it "format JSON Object" $
    let country = toJSON $ Country "X" $ Flag Blue 100 50
        result = "X Blue 100 50" :: String
    in format1 "{name} {flag!0} {flag!1} {flag!2}" country == result
  it "format whole JSON as string" $
    let country = toJSON $ Country "X" $ Flag Blue 100 50
    in isJust (decode (pack $ format "{}" country) :: Maybe Value)

  putStrLn "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"


data Color = Red | Yellow | Blue deriving Generic

instance ToJSON Color


data Flag = Flag Color Int Int deriving Generic

instance ToJSON Flag


data Country = Country { name :: String, flag :: Flag } deriving Generic

instance ToJSON Country


it :: String -> Bool -> IO ()
it = flip assert . putStrLn
