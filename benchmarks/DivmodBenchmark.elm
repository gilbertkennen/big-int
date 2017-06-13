module DivmodBenchmark exposing (main, benchmark)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt, negativeBigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program benchmark


benchmark : Benchmark
benchmark =
    describe "divmod"
        [ benchmark2 "one 32-digit number by 2" BI.divmod bigInt (BI.fromInt 2)
        , benchmark2 "one 32-digit number by itself" BI.divmod bigInt bigInt
        ]
