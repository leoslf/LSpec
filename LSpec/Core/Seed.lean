import LSpec.Prelude

namespace LSpec.Core

abbrev Seed := Int

def Seed.new : IO Seed :=
  (Int.ofNat ∘ UInt64.toNat ∘ ByteArray.toUInt64LE!) <$> IO.getRandomBytes 8

