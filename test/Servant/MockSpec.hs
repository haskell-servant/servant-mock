{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Servant.MockSpec where

#if MIN_VERSION_servant_server(0,20,0)
import           Data.Acquire (Acquire)
#endif
import           Data.Aeson as Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.Wai
import           Servant.API
import           Test.Hspec hiding (pending)
import           Test.Hspec.Wai hiding (Body)
import           Test.QuickCheck

import           Servant
import           Servant.Test.ComprehensiveAPI
import           Servant.Mock

import Data.ByteString.Conversion.To
import Data.String

-- This declaration simply checks that all instances are in place.
_ = mock comprehensiveAPI (Proxy :: Proxy '[NamedContext "foo" '[]
#if MIN_VERSION_servant_server(0,20,0)
    , Acquire Int
#endif
    ])

data Body
  = Body
  | ArbitraryBody
  deriving (Generic)

instance ToJSON Body

instance Arbitrary Body where
  arbitrary = return ArbitraryBody

data TestHeader
  = TestHeader
  | ArbitraryHeader
  deriving (Show)

-- Needed for servant-0.8.1
instance ToByteString TestHeader where
  builder = fromString . show

instance ToHttpApiData TestHeader where
  toHeader = toHeader . show
  toUrlPiece = toUrlPiece . show
  toQueryParam = toQueryParam . show


instance Arbitrary TestHeader where
  arbitrary = return ArbitraryHeader

spec :: Spec
spec = do
  describe "mock" $ do
    context "Get" $ do
      let api :: Proxy (Get '[JSON] Body)
          api = Proxy
          app = serve api (mock api (Proxy :: Proxy '[]))
      with (return app) $ do
        it "serves arbitrary response bodies" $ do
          get "/" `shouldRespondWith` 200{
            matchBody = MatchBody $ \ _ b ->
              if b == Aeson.encode ArbitraryBody
                then Nothing
                else Just ("body not correct\n")
          }

    context "response headers" $ do
      let withHeader :: Proxy (Get '[JSON] (Headers '[Header "foo" TestHeader] Body))
          withHeader = Proxy
          withoutHeader :: Proxy (Get '[JSON] (Headers '[] Body))
          withoutHeader = Proxy
          toApp :: (HasMock api '[]) => Proxy api -> IO Application
          toApp api = return $ serve api (mock api (Proxy :: Proxy '[]))
      with (toApp withHeader) $ do
        it "serves arbitrary response bodies" $ do
          get "/" `shouldRespondWith` 200{
            matchHeaders = return $ MatchHeader $ \ h _ ->
             if h == [("Content-Type", "application/json;charset=utf-8"), ("foo", "ArbitraryHeader")]
                then Nothing
                else Just ("headers not correct\n")
          }

      with (toApp withoutHeader) $ do
        it "works for no additional headers" $ do
          get "/" `shouldRespondWith` 200{
            matchHeaders = return $ MatchHeader $ \ h _ ->
             if h == [("Content-Type", "application/json;charset=utf-8")]
                then Nothing
                else Just ("headers not correct\n")
          }
