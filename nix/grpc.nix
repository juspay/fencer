{ darwin, stdenv, lib, fetchgit, autoconf, automake, libtool, which, zlib
, openssl, fixDarwinDylibNames
}:

stdenv.mkDerivation rec {
  name    = "grpc-${version}";
  version = "1.2.0-${lib.strings.substring 0 7 rev}";
  rev     = "e2cfe9df79c4eda4e376222df064c4c65e616352";
  src = fetchgit {
    inherit rev;
    url    = "https://github.com/grpc/grpc.git";
    sha256 = "19ldbjlnbc287hkaylsigm8w9fai2bjdbfxk6315kl75cq54iprr";
  };

  patches = [
    # https://github.com/grpc/grpc/pull/13714/files
    ./grpc-cast.patch
    # Don't enable IPv6 support for 0.0.0.0, patch by @neongreen to work
    # around https://github.com/grpc/grpc/issues/10532
    ./grpc-ipv4.patch
    # Disable compiler warnings to make the library compile
    ./grpc-warnings.patch
  ];

  # `grpc`'s `Makefile` does some magic to detect the correct `ld` and `strip`
  # to use along with their flags, too.  If Nix supplies `$LD` and `$STRIP` then
  # this auto-detection fails and the build fails, which is why we unset the
  # environment variables here and let the `Makefile` set them.
  preBuild = ''
    unset LD
    unset STRIP
  '';

  preInstall = "export prefix";

  buildInputs = [
    autoconf
    automake
    libtool
    which
    zlib
    openssl
    fixDarwinDylibNames
  ];

  # Some versions of `ar` (such as the one provided by OS X) require an explicit
  # `-r` flag, whereas other versions assume `-r` is the default if no mode is
  # specified.  For example, OS X requires the `-r` flag, so as a precaution we
  # always specify the flag.
  AROPTS = "-r";
}
