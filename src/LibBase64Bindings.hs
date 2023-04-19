{-# language
    CApiFFI
#-}

-- | This module wraps the C <https://github.com/aklomp/libbase64 libbase64> library for SIMD
--   encoding/decoding. It is intended only to be a wrapper. It provides nothing
--   in the way of a nice API or abstractions to make working with with it
--   "easy". Instead, it is meant to be a building block for other libraries to
--   take up that more opinionated work.
module LibBase64Bindings
  (
    base64_encode,
    base64_stream_encode_init,
    base64_stream_encode,
    base64_stream_encode_final,

    base64_decode,
    base64_stream_decode_init,
    base64_stream_decode,

    Base64State(..),

    _BASE64_FORCE_AVX2,
    _BASE64_FORCE_NEON32,
    _BASE64_FORCE_NEON64,
    _BASE64_FORCE_PLAIN,
    _BASE64_FORCE_SSSE3,
    _BASE64_FORCE_SSE41,
    _BASE64_FORCE_SSE42,
    _BASE64_FORCE_AVX,
  ) where

import Data.Bits (shiftL)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..), CUChar)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

-- | "base64_state" in the c library.
--
-- All usages in C just look like:
--
-- @
-- base64_state state; // allocates on the stack, uninitialised
--
-- do_stuff(&state);
-- @
--
-- Thus any 'Ptr' 'Base64State' passed to any of the `init` functions
-- should be something like
--
-- @
-- state <- mallocBytes (sizeOf (undefined :: Base64State))
-- @
--
-- and not manually instantiated.
--
-- The datatype and corresponding 'Storable' instance exist for
-- introspection/debugging.
data Base64State = Base64State
  { eof :: CInt,
    bytes :: CInt,
    flags :: CInt,
    carry :: CUChar
  }
  deriving (Eq, Ord, Show)

instance Storable Base64State where
  sizeOf _ = 4 * sizeOfCInt
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    Base64State
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr sizeOfCInt
      <*> peekByteOff ptr (2 * sizeOfCInt)
      <*> peekByteOff ptr (3 * sizeOfCInt)
  poke ptr b = do
    pokeByteOff ptr 0 (eof b)
    pokeByteOff ptr sizeOfCInt (bytes b)
    pokeByteOff ptr (2 * sizeOfCInt) (flags b)
    pokeByteOff ptr (3 * sizeOfCInt) (carry b)

sizeOfCInt :: Int
sizeOfCInt = sizeOf (undefined :: CInt)

------------
-- ENCODE --
------------

-- | Wrapper function to encode a plain string of given length.
--
--   - Output is written to @out@ without trailing zero.
--   - Output length in bytes is written to @outlen@.
--   - The buffer in @out@ has been allocated by the caller and is at least 4/3 the size of the input.
foreign import capi "libbase64.h base64_encode"
  base64_encode :: ()
    => CString   -- ^ \*src
    -> CSize     -- ^ srclen
    -> CString   -- ^ \*out
    -> Ptr CSize -- ^ \*outlen
    -> CInt      -- ^ flags
    -> IO ()

-- | Call this before calling 'base64_stream_encode' to init the state.
foreign import capi "libbase64.h base64_stream_encode_init"
  base64_stream_encode_init :: ()
    => Ptr Base64State -- ^ \*state
    -> CInt            -- ^ flags
    -> IO ()

-- | Encodes the block of data of given length at @src@, into the buffer at @out@.
--
--   Caller is responsible for allocating a large enough @out@-buffer; it must be at least 4/3 the size
--   of the @in@-buffer, but take some margin.
--
--   Places the number of new bytes written into @outlen@ (which is set to zero when the function starts).
--
--   Does not zero-terminate or finalize the output.
foreign import capi "libbase64.h base64_stream_encode"
  base64_stream_encode :: ()
    => Ptr Base64State -- ^ \*state
    -> CString         -- ^ \*src
    -> CSize           -- ^ srclen
    -> CString         -- ^ \*out
    -> Ptr CSize       -- ^ \*outlen
    -> IO ()

-- | Finalizes the output begun by previous calls to 'base64_stream_encode'.
--
--   Adds the required end-of-stream markers if appropriate.
--
--   @outlen@ is modified and will contain the number of new bytes written at out (which will quite often be zero).
foreign import capi "libbase64.h base64_stream_encode_final"
  base64_stream_encode_final :: ()
    => Ptr Base64State -- ^ \*state
    -> CString         -- ^ \*out
    -> Ptr CSize       -- ^ \*outlen
    -> IO ()

------------
-- DECODE --
------------

-- | Wrapper function to decode a plain string of given length.
--
--   Output is written to @out@ without trailing zero.
--   Output length in bytes is written to @outlen@.
--   The buffer in @out@ has been allocated by the caller and is at least 3/4 the size of the input.
--
--   Returns 1 for success, and 0 when a decode error has occured due to invalid input.
--   Returns -1 if the chosen codec is not included in the current build.
foreign import capi "libbase64.h base64_decode"
  base64_decode :: ()
    => CString   -- ^ \*src
    -> CSize     -- ^ srclen
    -> CString   -- ^ \*out
    -> Ptr CSize -- ^ \*outlen
    -> CInt      -- ^ flags
    -> IO CInt

-- | Call this before calling 'base64_stream_decode' to init the state.
foreign import capi "libbase64.h base64_stream_decode_init"
  base64_stream_decode_init :: ()
    => Ptr Base64State -- ^ \*state
    -> CInt            -- ^ flags
    -> IO ()

-- | Decodes the block of data of given length at @src@, into the buffer at @out@.
--
--   Caller is responsible for allocating a large enough @out@-buffer; it must be at least 3/4 the size of the in-buffer, but take some margin.
--
--   Places the number of new bytes written into @outlen@ (which is set to zero when the function starts).
--
--   Does not zero-terminate the output.
--
--   Returns 1 if all is well, and 0 if a decoding error was found, such as an invalid character.
--   Returns -1 if the chosen codec is not included in the current build.
foreign import capi "libbase64.h base64_stream_decode"
  base64_stream_decode :: ()
    => Ptr Base64State -- ^ \*state
    -> CString         -- ^ \*src
    -> CSize           -- ^ srclen
    -> CString         -- ^ \*out
    -> Ptr CSize       -- ^ \*outlen
    -> IO CInt

-- | Force AVX2.
_BASE64_FORCE_AVX2 :: CInt
_BASE64_FORCE_AVX2 = 1 `shiftL` 0

-- | Force NEON32.
_BASE64_FORCE_NEON32 :: CInt
_BASE64_FORCE_NEON32 = 1 `shiftL` 1

-- | Force NEON64.
_BASE64_FORCE_NEON64 :: CInt
_BASE64_FORCE_NEON64 = 1 `shiftL` 2

-- | Force PLAIN.
_BASE64_FORCE_PLAIN :: CInt
_BASE64_FORCE_PLAIN = 1 `shiftL` 3

-- | Force SSSE3.
_BASE64_FORCE_SSSE3 :: CInt
_BASE64_FORCE_SSSE3 = 1 `shiftL` 4

-- | Force SSE41.
_BASE64_FORCE_SSE41 :: CInt
_BASE64_FORCE_SSE41 = 1 `shiftL` 5

-- | Force SSE41.
_BASE64_FORCE_SSE42 :: CInt
_BASE64_FORCE_SSE42 = 1 `shiftL` 6

-- | Force AVX.
_BASE64_FORCE_AVX :: CInt
_BASE64_FORCE_AVX = 1 `shiftL` 7
