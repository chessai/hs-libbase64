# TODO: upstream to nixpkgs
{
  stdenv,
  fetchFromGitHub,
  cmake,
  simdExtensions ? null,
  debugTrace ? false,
}:

let
  trace = msg: x: if debugTrace then builtins.trace msg x else x;

  neonGCCWarning = {
    warnOn = stdenv.cc.isGNU;
    warnMsg = "WARNING: NEON is not well-supported by GCC. On certain platforms, there are bugs; and in general, the NEON-relevant assembly emitted is poor. Consider using Clang/LLVM instead.";
  };

  supports = with stdenv.hostPlatform; [
    {
      ext = "AVX2";
      isSupported = avx2Support;
      mandatory = false;
      flagRhs = "-mavx2";
    }
    {
      ext = "AVX";
      isSupported = avxSupport;
      mandatory = false;
      flagRhs = "-mavx";
    }
    {
      ext = "SSSE3";
      isSupported = ssse3Support;
      mandatory = false;
      flagRhs = "-mssse3";
    }
    {
      ext = "SSE41";
      isSupported = sse4_1Support;
      mandatory = false;
      flagRhs = "-msse4.1";
    }
    {
      ext = "SSE42";
      isSupported = sse4_2Support;
      mandatory = false;
      flagRhs = "-msse4.1";
    }
    {
      ext = "NEON32";
      isSupported = isAarch32;
      mandatory = false;
      flagRhs = "-mfpu=neon"; # TODO: support -march/different arm versions
      warnings = [
        neonGCCWarning
      ];
    }
    {
      ext = "NEON64";
      isSupported = isAarch64;
      mandatory = isAarch64;
      flagRhs = " "; # non-empty stub, because it only exists and is mandatory on Aarch64
      warnings = [
        neonGCCWarning
      ];
    }
    {
      ext = "OPENMP";
      isSupported = stdenv.cc.isGNU;
      mandatory = false;
      flagRhs = "1";
    }
  ];

/*
  find = p: xs:
    builtins.foldr (_: a: if p a then a else null) null xs;

  validateCustomFlag = ext:
    let
      validExts = builtins.map (s: s.ext) supports;
      flag =
        let s = find (s: s.ext == ext) supports;
        in if builtins.isNull s
           then builtins.throw
                  ("libbase64: invalid SIMD extension: \"${ext}\".\nValid SIMD extensions are: ${builtins.toString validExts}")
           else s;
    in if flag.isSupported then flag else builtins.throw "libbase64: ${ext} is not supported on your platform.";

  #validateMandatory = exts:
*/

  supportToFlag = { ext, isSupported, flagRhs, ... }@support:
    let suffix = if ext == "OPENMP" then "" else "_CFLAGS";
        flag = ext + suffix + "=" + flagRhs;
        warnings = builtins.map (w: w.warnMsg)
          (builtins.filter (w: w.warnOn) (support.warnings or []));
        withWarnings = x: if warnings != [] then trace (builtins.concatStringsSep "\n" warnings) x else x;
        enableDebug = flag: x: trace "enabling: ${flag}" x;
        disableDebug = flag: x: trace "not enabling: ${flag}" x;
    in if isSupported
       then withWarnings (enableDebug ext flag)
       else disableDebug ext null;

  makeFlags = builtins.concatStringsSep " "
    (builtins.filter (flag: flag != null) (builtins.map supportToFlag supports));
in
stdenv.mkDerivation rec {
  pname = "libbase64";
  version = "0.5.0";

  src = fetchFromGitHub {
    owner = "aklomp";
    repo = "base64";
    rev = "v${version}";
    sha256 = "sha256-2HNI9ycT9f+NLwLElEuR61qmTguOsI+kNxv01ipxSqQ=";
  };

  inherit makeFlags;

  nativeBuildInputs = [ cmake ];
}
