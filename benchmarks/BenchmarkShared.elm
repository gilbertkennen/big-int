module BenchmarkShared exposing (bigInt, bigIntString, negativeBigInt)

import BigInt as BI


bigIntString : String
bigIntString =
    "98765432100123456789987654321001234567899876543210012345678909"


bigInt : BI.BigInt
bigInt =
    BI.fromString bigIntString
        |> Maybe.withDefault (BI.fromInt 0)


negativeBigInt : BI.BigInt
negativeBigInt =
    BI.negate bigInt
