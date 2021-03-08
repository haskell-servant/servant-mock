{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "overlapping-compat.h"

-- |
-- Module     : Servant.Mock
-- Copyright  : 2015 Alp Mestanogullari
-- License    : BSD3
--
-- Maintainer  : Alp Mestanogullari <alpmestan@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Automatically derive a mock webserver that implements some API type,
-- just from the said API type's definition.
--
-- Using this module couldn't be simpler. Given some API type, like:
--
-- > type API = "user" :> Get '[JSON] User
--
-- that describes your web application, all you have to do is define
-- a 'Proxy' to it:
--
-- > myAPI :: Proxy API
-- > myAPI = Proxy
--
-- and call 'mock', which has the following type:
--
-- @
-- 'mock' :: 'HasMock' api context => 'Proxy' api -> 'Proxy' context -> 'Server' api
-- @
--
-- What this says is, given some API type @api@ that it knows it can
-- "mock", 'mock' hands you an implementation of the API type. It does so
-- by having each request handler generate a random value of the
-- appropriate type (@User@ in our case). All you need for this to work is
-- to provide 'Arbitrary' instances for the data types returned as response
-- bodies, hence appearing next to 'Delete', 'Get', 'Patch', 'Post' and 'Put'.
--
-- To put this all to work and run the mock server, just call 'serve' on the
-- result of 'mock' to get an 'Application' that you can then run with warp.
--
-- @
-- main :: IO ()
-- main = Network.Wai.Handler.Warp.run 8080 $
--   'serve' myAPI ('mock' myAPI Proxy)
-- @
module Servant.Mock ( HasMock(..) ) where

import Prelude ()
import Prelude.Compat

import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Proxy
import           GHC.TypeLits
import           Network.HTTP.Types.Status
import           Network.Wai
import           Servant
import           Servant.API.ContentTypes
import           Servant.API.Modifiers
import           Test.QuickCheck.Arbitrary  (Arbitrary (..), vector)
import           Test.QuickCheck.Gen        (Gen, generate)

-- | 'HasMock' defines an interpretation of API types
--   than turns them into random-response-generating
--   request handlers, hence providing an instance for
--   all the combinators of the core /servant/ library.
class (HasServer api context
#if MIN_VERSION_servant_server(0,18,0)
      , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
#endif
      ) => HasMock api context where
  -- | Calling this method creates request handlers of
  --   the right type to implement the API described by
  --   @api@ that just generate random response values of
  --   the right type. E.g:
  --
  --   @
  --   type API = "user" :> Get '[JSON] User
  --         :<|> "book" :> Get '[JSON] Book
  --
  --   api :: Proxy API
  --   api = Proxy
  --
  --   -- let's say we will start with the frontend,
  --   -- and hence need a placeholder server
  --   server :: Server API
  --   server = mock api Proxy
  --   @
  --
  --   What happens here is that @'Server' API@
  --   actually "means" 2 request handlers, of the following types:
  --
  --   @
  --   getUser :: Handler User
  --   getBook :: Handler Book
  --   @
  --
  --   So under the hood, 'mock' uses the 'IO' bit to generate
  --   random values of type 'User' and 'Book' every time these
  --   endpoints are requested.
  mock :: Proxy api -> Proxy context -> Server api

instance (HasMock a context, HasMock b context) => HasMock (a :<|> b) context where
  mock _ context = mock (Proxy :: Proxy a) context :<|> mock (Proxy :: Proxy b) context

instance (KnownSymbol path, HasMock rest context) => HasMock (path :> rest) context where
  mock _ = mock (Proxy :: Proxy rest)

instance (KnownSymbol s, FromHttpApiData a, HasMock rest context, SBoolI (FoldLenient mods)) => HasMock (Capture' mods s a :> rest) context where
  mock _ context = \_ -> mock (Proxy :: Proxy rest) context

instance (KnownSymbol s, FromHttpApiData a, HasMock rest context) => HasMock (CaptureAll s a :> rest) context where
  mock _ context = \_ -> mock (Proxy :: Proxy rest) context

instance (AllCTUnrender ctypes a, HasMock rest context, SBoolI (FoldLenient mods))
    => HasMock (ReqBody' mods ctypes a :> rest) context where
  mock _ context = \_ -> mock (Proxy :: Proxy rest) context

-- | @since 0.8.5
instance (MimeUnrender ctype chunk, FramingUnrender fr, FromSourceIO chunk a, HasMock rest context)
    => HasMock (StreamBody' mods fr ctype a :> rest) context where
  mock _ context = \_ -> mock (Proxy :: Proxy rest) context

instance HasMock rest context => HasMock (RemoteHost :> rest) context where
  mock _ context = \_ -> mock (Proxy :: Proxy rest) context

instance HasMock rest context => HasMock (IsSecure :> rest) context where
  mock _ context = \_ -> mock (Proxy :: Proxy rest) context

instance HasMock rest context => HasMock (Vault :> rest) context where
  mock _ context = \_ -> mock (Proxy :: Proxy rest) context

instance HasMock rest context => HasMock (HttpVersion :> rest) context where
  mock _ context = \_ -> mock (Proxy :: Proxy rest) context

instance (KnownSymbol s, FromHttpApiData a, HasMock rest context, SBoolI (FoldRequired mods), SBoolI (FoldLenient mods))
      => HasMock (QueryParam' mods s a :> rest) context where
  mock _ context = \_ -> mock (Proxy :: Proxy rest) context

instance (KnownSymbol s, FromHttpApiData a, HasMock rest context)
      => HasMock (QueryParams s a :> rest) context where
  mock _ context = \_ -> mock (Proxy :: Proxy rest) context

instance (KnownSymbol s, HasMock rest context) => HasMock (QueryFlag s :> rest) context where
  mock _ context = \_ -> mock (Proxy :: Proxy rest) context

instance (KnownSymbol h, FromHttpApiData a, HasMock rest context, SBoolI (FoldRequired mods), SBoolI (FoldLenient mods))
    => HasMock (Header' mods h a :> rest) context where
  mock _ context = \_ -> mock (Proxy :: Proxy rest) context

instance (Arbitrary a, KnownNat status, ReflectMethod method, AllCTRender ctypes a
#if MIN_VERSION_servant_server(0,18,0)
         , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
#endif
         )
    => HasMock (Verb method status ctypes a) context where
  mock _ _ = mockArbitrary

instance (ReflectMethod method
#if MIN_VERSION_servant_server(0,18,0)
         , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
#endif
         ) => HasMock (NoContentVerb method) context where
  mock _ _ = mockArbitrary

instance (Arbitrary a, KnownNat status, ReflectMethod method, MimeRender ctype chunk, FramingRender fr, ToSourceIO chunk a
#if MIN_VERSION_servant_server(0,18,0)
         , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
#endif
         )
    => HasMock (Stream method status fr ctype a) context where
  mock _ _ = mockArbitrary

instance OVERLAPPING_
    (GetHeaders (Headers headerTypes a), Arbitrary (HList headerTypes),
     Arbitrary a, KnownNat status, ReflectMethod method, AllCTRender ctypes a
#if MIN_VERSION_servant_server(0,18,0)
    , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
#endif
    )
    => HasMock (Verb method status ctypes (Headers headerTypes a)) context where
  mock _ _ = mockArbitrary

instance
#if MIN_VERSION_servant_server(0,18,0)
    HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters =>
#endif
    HasMock Raw context where
  mock _ _ = Tagged $ \_req _respond -> do
    bdy <- genBody
    _respond $ responseLBS status200 [] bdy

    where genBody = pack <$> generate (vector 100 :: Gen [Char])

instance
#if MIN_VERSION_servant_server(0,18,0)
    HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters =>
#endif
    HasMock EmptyAPI context where
  mock _ _ = emptyServer

instance HasMock api context => HasMock (Summary d :> api) context where
    mock _ context = mock (Proxy :: Proxy api) context

instance HasMock api context => HasMock (Description d :> api) context where
    mock _ context = mock (Proxy :: Proxy api) context

instance (AtLeastOneFragment api, FragmentUnique (Fragment d :> api), HasMock api context) => HasMock (Fragment d :> api) context where
    mock _ context = mock (Proxy :: Proxy api) context

instance ( HasContextEntry context (NamedContext name subContext)
#if MIN_VERSION_servant_server(0,18,0)
         , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
#endif
         , HasMock rest subContext) =>
  HasMock (WithNamedContext name subContext rest) context where

  mock _ _ = mock (Proxy :: Proxy rest) (Proxy :: Proxy subContext)

mockArbitrary :: (MonadIO m, Arbitrary a) => m a
mockArbitrary = liftIO (generate arbitrary)

-- utility instance
instance (Arbitrary (HList ls), Arbitrary a)
      => Arbitrary (Headers ls a) where
  arbitrary = Headers <$> arbitrary <*> arbitrary

instance Arbitrary (HList '[]) where
  arbitrary = pure HNil

instance (Arbitrary a, Arbitrary (HList hs))
      => Arbitrary (HList (Header h a ': hs)) where
  arbitrary = HCons <$> fmap Header arbitrary <*> arbitrary

instance Arbitrary NoContent where
  arbitrary = pure NoContent
