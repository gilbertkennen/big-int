module PowBenchmark exposing (main, benchmark)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt, negativeBigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program benchmark


benchmark : Benchmark
benchmark =
    describe "pow"
        [ benchmark2 "positive 32-digit to the power 21" BI.pow bigInt (BI.fromInt 21)
        , benchmark2 "negative 32-digit to the power 21" BI.pow negativeBigInt (BI.fromInt 21)
        ]
