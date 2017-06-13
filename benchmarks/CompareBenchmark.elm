module CompareBenchmark exposing (main, benchmark)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt, negativeBigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program benchmark


benchmark : Benchmark
benchmark =
    describe "compare"
        [ benchmark2 "one 32-digit number with itself" BI.compare bigInt bigInt
        , benchmark2 "one positive and one negative 32-digit number" BI.compare bigInt negativeBigInt
        ]
