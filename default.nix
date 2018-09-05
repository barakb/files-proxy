{ mkDerivation, aeson, async, base, bytestring, conduit
, conduit-extra, containers, cryptonite, directory, http-api-data
, http-client, http-conduit, http-media, http-types, mtl
, optparse-applicative, resourcet, safe-exceptions, servant
, servant-checked-exceptions, servant-server, stdenv, text
, transformers, wai, wai-logger, warp
}:
mkDerivation {
  pname = "FilesProxy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring conduit conduit-extra containers
    cryptonite directory http-api-data http-client http-conduit
    http-media http-types mtl optparse-applicative resourcet
    safe-exceptions servant servant-checked-exceptions servant-server
    text transformers wai wai-logger warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
