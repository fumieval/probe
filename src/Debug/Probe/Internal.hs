{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures#-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Debug.Probe.Internal where

import GHC.TypeNats
import Codec.Winery
import Data.Typeable
import qualified Data.ByteString as B
import qualified Data.ByteString.FastBuilder as BB
import System.IO
import qualified Data.Vector as V
import qualified Data.Text as T
import GHC.Stack

data Nat' = Z | S Nat'

type family ConvertNat n :: Nat' where
  ConvertNat 0 = 'Z
  ConvertNat n = 'S (ConvertNat (n - 1))

data Recording xs r where
  RArg :: x -> Recording xs r -> Recording (x ': xs) r
  RResult :: r -> Recording '[] r

type Probed n a = Recording (ProbeArgs n a) (ProbeResult n a)

instance Serialise r => Serialise (Recording '[] r) where
  schemaGen _ = SProduct . pure <$> schemaGen (Proxy @ r)
  toBuilder (RResult r) = toBuilder r
  extractor = RResult <$> extractor
  decodeCurrent = RResult <$> decodeCurrent

instance (Typeable xs, Typeable r, Serialise x, Serialise r, Serialise (Recording xs r)) => Serialise (Recording (x ': xs) r) where
  schemaGen _ = cons <$> schemaGen (Proxy @ x) <*> schemaGen (Proxy @ r) where
    cons s (SProduct ss) = SProduct (V.cons s ss)
    cons _ _ = error "Serialise (Recording (x ': xs) r): impossible"
  toBuilder (RArg r xs) = toBuilder r <> toBuilder xs
  extractor = undefined
  decodeCurrent = RArg <$> decodeCurrent <*> decodeCurrent

class Probe (n :: Nat') a where
  type ProbeArgs n a :: [*]
  type ProbeResult n a :: *
  probe_ :: a -> (Probed n a -> a) -> a

instance Probe 'Z a where
  type ProbeArgs 'Z a = '[]
  type ProbeResult 'Z a = a
  probe_ a cont = cont (RResult a)

instance Probe n r => Probe ('S n) (a -> r) where
  type ProbeArgs ('S n) (a -> r) = a ': ProbeArgs n r
  type ProbeResult ('S n) (a -> r) = ProbeResult n r
  probe_ f cont a = probe_ @ n (f a) (\r -> cont (RArg a r) a)

dumpWinery :: Serialise (Recording xs r) => FilePath -> CallStack -> Recording xs r -> IO ()
dumpWinery path stack r = withFile path WriteMode $ \h -> do
  let bs = serialise (map (T.pack . fst) $ getCallStack $ popCallStack stack, r)
  BB.hPutBuilder h (toBuilder $ B.length bs)
  B.hPutStr h bs
