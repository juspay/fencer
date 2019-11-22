# Taken from https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/libraries/protobuf/generic-v3.nix

{ stdenv
, fetchFromGitHub
, autoreconfHook, zlib, gmock, buildPackages
, version ? "3.7.1"
, sha256 ? "00zkijvj80bmhlj8290x4bc416mng2dlbzwc4bkvfmbal1fx93m2"
, ...
}:

let
mkProtobufDerivation = buildProtobuf: stdenv: stdenv.mkDerivation {
  pname = "protobuf";
  inherit version;

  # make sure you test also -A pythonPackages.protobuf
  src = fetchFromGitHub {
    owner = "protocolbuffers";
    repo = "protobuf";
    rev = "v${version}";
    inherit sha256;
  };

  postPatch = ''
    rm -rf gmock
    cp -r ${gmock.src}/googlemock gmock
    cp -r ${gmock.src}/googletest googletest
    chmod -R a+w gmock
    chmod -R a+w googletest
    ln -s ../googletest gmock/gtest
  '' + stdenv.lib.optionalString stdenv.isDarwin ''
    substituteInPlace src/google/protobuf/testing/googletest.cc \
      --replace 'tmpnam(b)' '"'$TMPDIR'/foo"'
  '';

  # Modified by @neongreen: we want to make sure the result does not drag
  # the C++ compiler into the closure
  disallowedReferences = [ stdenv.cc ];

  nativeBuildInputs = [ autoreconfHook buildPackages.which buildPackages.stdenv.cc buildProtobuf ];

  buildInputs = [ zlib ];
  configureFlags = if buildProtobuf == null then [] else [ "--with-protoc=${buildProtobuf}/bin/protoc" ];

  # Modified by @neongreen: '-g' adds debug symbols to binaries, and those
  # contain paths to the C++ compiler, which get interpreted by Nix as a
  # runtime dependency. See <https://github.com/NixOS/nixpkgs/issues/73919>
  postConfigure = ''
    sed -i -e 's/ -g / /g' Makefile src/Makefile gmock/make/Makefile googletest/make/Makefile
  '';

  enableParallelBuilding = true;

  doCheck = true;

  dontDisableStatic = true;

  meta = {
    description = "Google's data interchange format";
    longDescription =
      ''Protocol Buffers are a way of encoding structured data in an efficient
        yet extensible format. Google uses Protocol Buffers for almost all of
        its internal RPC protocols and file formats.
      '';
    license = stdenv.lib.licenses.bsd3;
    platforms = stdenv.lib.platforms.unix;
    homepage = https://developers.google.com/protocol-buffers/;
  };

  passthru.version = version;
};
in mkProtobufDerivation(if (stdenv.buildPlatform != stdenv.hostPlatform)
                        then (mkProtobufDerivation null buildPackages.stdenv)
                        else null) stdenv
