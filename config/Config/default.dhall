let Config = ./Config.dhall

let TypeMap = ../TypeMap/package.dhall

in  { headers = [ "stdint.h" ], typeMap = TypeMap.default } : Config
