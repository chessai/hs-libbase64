{-# language
    CApiFFI
  , MagicHash
  , ScopedTypeVariables
#-}

{-# options_ghc -fno-warn-orphans #-}

module Main (main) where

import Data.Proxy (Proxy(..))
import Test.QuickCheck.Classes.Base (lawsCheck, storableLaws)
import Test.QuickCheck (Arbitrary(..))
import GHC.Ptr (Ptr(Ptr))
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CSize(..), CInt(..), CUChar)
import Foreign.Storable (peek, pokeByteOff)
import Foreign.Marshal.Alloc (malloc, mallocBytes)
import qualified Foreign.Marshal.Utils as Foreign
import System.Exit (exitFailure)
import Control.Monad (forM_)

import LibBase64Bindings (Base64State(Base64State), base64_encode, base64_decode)

main :: IO ()
main = do
  lawsCheck $ storableLaws (Proxy :: Proxy Base64State)

  out :: CString <- mallocBytes 2000
  let outlen = CSize 0
  let flags = 0

  putStrLn "Starting unit tests..."
  forM_ unitTestVector $ \(plain, _base64) -> do
    roundtrip (out, outlen) flags plain

instance Arbitrary Base64State where
  arbitrary = Base64State <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

unitTestVector :: [(CString, CString)]
unitTestVector =
  [
    (Ptr ""#,        Ptr ""#),
    (Ptr "f"#,       Ptr "Zg=="#),
    (Ptr "fo"#,      Ptr "Zm8="#),
    (Ptr "foo"#,     Ptr "Zm9v"#),
    (Ptr "foob"#,    Ptr "Zm9vYg=="#),
    (Ptr "fooba"#,   Ptr "Zm9vYmE="#),
    (Ptr "foobar"#,  Ptr "Zm9vYmFy"#),
    (mobyDickPlain,  mobyDickBase64)
  ]

foreign import capi "string.h strlen"
  strlen :: CString -> IO CSize

foreign import capi "string.h strncmp"
  strncmp :: CString -> CString -> CSize -> IO CInt

-- TODO: we want to accumulate all errors, so we should terminate early with a
-- more informative return value
roundtrip :: (CString, CSize) -> CInt -> CString -> IO ()
roundtrip (out, outlen_pure) flags src = do
  tmp :: CString <- mallocBytes 1500
  tmplen :: Ptr CSize <- malloc
  srclen <- Foreign.new =<< strlen src
  outlen <- Foreign.new outlen_pure

  do
    srclen_pure <- peek srclen
    base64_encode src srclen_pure out outlen flags

  do
    outlen_after_encode <- peek outlen
    decodeResult <- base64_decode out outlen_after_encode tmp tmplen flags
    if Foreign.toBool decodeResult
    then do
      pure ()
    else do
      outString <- peekCString out
      putStrLn $ "FAIL: decoding of\n\n" ++ outString ++ "\n\nDecoding error!"
      exitFailure

  tmplen_final <- peek tmplen
  srclen_final <- peek srclen

  do
    if tmplen_final /= srclen_final
    then do
      srcString <- peekCString src
      putStrLn $ "FAIL: roundtrip of " ++ srcString ++ ":\n"
                 ++ "  length expected: " ++ show srclen_final ++ "\n"
                 ++ "  but got: " ++ show tmplen_final
      exitFailure
    else do
      pure ()

  do
    cmpResult <- strncmp src tmp tmplen_final
    if cmpResult /= 0
    then do
      pokeByteOff tmp (fromIntegral tmplen_final) (0 :: CUChar)
      srcString <- peekCString src
      tmpString <- peekCString tmp
      putStrLn $ "FAIL: roundtrip of " ++ srcString ++ ": got " ++ tmpString
      exitFailure
    else do
      pure ()

mobyDickPlain :: CString
mobyDickPlain = Ptr
  "Call me Ishmael. Some years ago--never mind how long precisely--having\n\
  \little or no money in my purse, and nothing particular to interest me on\n\
  \shore, I thought I would sail about a little and see the watery part of\n\
  \the world. It is a way I have of driving off the spleen and regulating\n\
  \the circulation. Whenever I find myself growing grim about the mouth;\n\
  \whenever it is a damp, drizzly November in my soul; whenever I find\n\
  \myself involuntarily pausing before coffin warehouses, and bringing up\n\
  \the rear of every funeral I meet; and especially whenever my hypos get\n\
  \such an upper hand of me, that it requires a strong moral principle to\n\
  \prevent me from deliberately stepping into the street, and methodically\n\
  \knocking people's hats off--then, I account it high time to get to sea\n\
  \as soon as I can. This is my substitute for pistol and ball. With a\n\
  \philosophical flourish Cato throws himself upon his sword; I quietly\n\
  \take to the ship. There is nothing surprising in this. If they but knew\n\
  \it, almost all men in their degree, some time or other, cherish very\n\
  \nearly the same feelings towards the ocean with me.\n"#;

mobyDickBase64 :: CString
mobyDickBase64 = Ptr
  "Q2FsbCBtZSBJc2htYWVsLiBTb21lIHllYXJzIGFnby0tbmV2ZXIgbWluZCBob3cgbG9uZ\
  \yBwcmVjaXNlbHktLWhhdmluZwpsaXR0bGUgb3Igbm8gbW9uZXkgaW4gbXkgcHVyc2UsIG\
  \FuZCBub3RoaW5nIHBhcnRpY3VsYXIgdG8gaW50ZXJlc3QgbWUgb24Kc2hvcmUsIEkgdGh\
  \vdWdodCBJIHdvdWxkIHNhaWwgYWJvdXQgYSBsaXR0bGUgYW5kIHNlZSB0aGUgd2F0ZXJ5\
  \IHBhcnQgb2YKdGhlIHdvcmxkLiBJdCBpcyBhIHdheSBJIGhhdmUgb2YgZHJpdmluZyBvZ\
  \mYgdGhlIHNwbGVlbiBhbmQgcmVndWxhdGluZwp0aGUgY2lyY3VsYXRpb24uIFdoZW5ldm\
  \VyIEkgZmluZCBteXNlbGYgZ3Jvd2luZyBncmltIGFib3V0IHRoZSBtb3V0aDsKd2hlbmV\
  \2ZXIgaXQgaXMgYSBkYW1wLCBkcml6emx5IE5vdmVtYmVyIGluIG15IHNvdWw7IHdoZW5l\
  \dmVyIEkgZmluZApteXNlbGYgaW52b2x1bnRhcmlseSBwYXVzaW5nIGJlZm9yZSBjb2Zma\
  \W4gd2FyZWhvdXNlcywgYW5kIGJyaW5naW5nIHVwCnRoZSByZWFyIG9mIGV2ZXJ5IGZ1bm\
  \VyYWwgSSBtZWV0OyBhbmQgZXNwZWNpYWxseSB3aGVuZXZlciBteSBoeXBvcyBnZXQKc3V\
  \jaCBhbiB1cHBlciBoYW5kIG9mIG1lLCB0aGF0IGl0IHJlcXVpcmVzIGEgc3Ryb25nIG1v\
  \cmFsIHByaW5jaXBsZSB0bwpwcmV2ZW50IG1lIGZyb20gZGVsaWJlcmF0ZWx5IHN0ZXBwa\
  \W5nIGludG8gdGhlIHN0cmVldCwgYW5kIG1ldGhvZGljYWxseQprbm9ja2luZyBwZW9wbG\
  \UncyBoYXRzIG9mZi0tdGhlbiwgSSBhY2NvdW50IGl0IGhpZ2ggdGltZSB0byBnZXQgdG8\
  \gc2VhCmFzIHNvb24gYXMgSSBjYW4uIFRoaXMgaXMgbXkgc3Vic3RpdHV0ZSBmb3IgcGlz\
  \dG9sIGFuZCBiYWxsLiBXaXRoIGEKcGhpbG9zb3BoaWNhbCBmbG91cmlzaCBDYXRvIHRoc\
  \m93cyBoaW1zZWxmIHVwb24gaGlzIHN3b3JkOyBJIHF1aWV0bHkKdGFrZSB0byB0aGUgc2\
  \hpcC4gVGhlcmUgaXMgbm90aGluZyBzdXJwcmlzaW5nIGluIHRoaXMuIElmIHRoZXkgYnV\
  \0IGtuZXcKaXQsIGFsbW9zdCBhbGwgbWVuIGluIHRoZWlyIGRlZ3JlZSwgc29tZSB0aW1l\
  \IG9yIG90aGVyLCBjaGVyaXNoIHZlcnkKbmVhcmx5IHRoZSBzYW1lIGZlZWxpbmdzIHRvd\
  \2FyZHMgdGhlIG9jZWFuIHdpdGggbWUuCg=="#;
