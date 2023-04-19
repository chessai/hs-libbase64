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

{-
static int
assert_roundtrip (int flags, const char *src)
{
  char tmp[1500];
  size_t tmplen;
  size_t srclen = strlen(src);

  // Encode the input into global buffer:
  base64_encode(src, srclen, out, &outlen, flags);

  // Decode the global buffer into local temp buffer:
  if (!base64_decode(out, outlen, tmp, &tmplen, flags)) {
    printf("FAIL: decoding of '%s': decoding error\n", out);
    return true;
  }

  // Check that 'src' is identical to 'tmp':
  if (srclen != tmplen) {
    printf("FAIL: roundtrip of '%s': "
      "length expected %lu, got %lu\n", src,
      (unsigned long)srclen,
      (unsigned long)tmplen
    );
    return true;
  }
  if (strncmp(src, tmp, tmplen) != 0) {
    tmp[tmplen] = '\0';
    printf("FAIL: roundtrip of '%s': got '%s'\n", src, tmp);
    return true;
  }

  return false;
}
-}

{-
  // Test vectors:
  struct {
    const char *in;
    const char *out;
  } vec[] = {

    // These are the test vectors from RFC4648:

    // The first paragraph from Moby Dick,
    // to test the SIMD codecs with larger blocksize:
    { moby_dick_plain, moby_dick_base64 },
  };

  for (size_t i = 0; i < sizeof(vec) / sizeof(vec[0]); i++) {

    // Encode plain string, check against output:
    fail |= assert_enc(flags, vec[i].in, vec[i].out);

    // Decode the output string, check if we get the input:
    fail |= assert_dec(flags, vec[i].out, vec[i].in);

    // Do a roundtrip on the inputs and the outputs:
    fail |= assert_roundtrip(flags, vec[i].in);
    fail |= assert_roundtrip(flags, vec[i].out);
  }
-}

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
