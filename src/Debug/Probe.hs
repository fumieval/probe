{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Debug.Probe (probeFile, probe) where

import Debug.Probe.Internal
import Codec.Winery (Serialise)
import GHC.Stack (HasCallStack, callStack)
import GHC.TypeLits (Nat)
import System.IO.Unsafe

probeFile :: forall (n :: Nat) a. (Probe (ConvertNat n) a, Serialise (Probed (ConvertNat n) a), HasCallStack)
  => FilePath -> a -> a
probeFile path a = probe_ @ (ConvertNat n) a $ \r -> unsafePerformIO
  $ a <$ dumpWinery path callStack r

probe :: forall (n :: Nat) a. (Probe (ConvertNat n) a, Serialise (Probed (ConvertNat n) a), HasCallStack)
  => a -> a
probe = probeFile @ n "probe.winery"
