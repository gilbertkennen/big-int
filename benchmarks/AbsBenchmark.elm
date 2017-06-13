module AbsBenchmark exposing (main, benchmark)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt, negativeBigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program benchmark


benchmark : Benchmark
benchmark =
    describe "abs"
        [ benchmark1 "positive 32-digit number" BI.abs bigInt
        , benchmark1 "negative 32-digit number" BI.abs negativeBigInt
        ]
