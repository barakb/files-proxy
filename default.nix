{ mkDerivation, aeson, base, bytestring, containers, directory
, http-api-data, http-client, http-media, http-types, mtl, servant
, servant-server, stdenv, text, transformers, wai, wai-logger, warp
}:
mkDerivation {
  pname = "FilesProxy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers directory http-api-data
    http-client http-media http-types mtl servant servant-server text
    transformers wai wai-logger warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
