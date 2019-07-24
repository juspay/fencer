{ mkDerivation, stdenv
, base, grpc-haskell
, configureFlags ? [], enableSharedExecutables ? true, enableSharedLibraries ? true
}:
mkDerivation {
  pname = "ratelimit";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  inherit enableSharedExecutables enableSharedLibraries configureFlags;
  executableHaskellDepends = [
    base grpc-haskell
  ];
  description = "Port of lyft/ratelimit";
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
}
