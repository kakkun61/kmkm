let TypeMap = ./TypeMap.dhall

in    { int = "int"
      , uint = "unsigned int"
      , byte = "uint8_t"
      , frac2 = "double"
      , frac = "float"
      }
    : TypeMap
