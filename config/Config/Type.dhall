let CHeader = ../CHeader/package.dhall
let TypeMap = ../TypeMap/package.dhall

in  { headers : List CHeader.Type, typeMap : TypeMap.Type }
