module AddBenchmark exposing (main, benchmark)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt, negativeBigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program benchmark


benchmark : Benchmark
benchmark =
    describe "add"
        [ benchmark2 "two positive 32-digit numbers" BI.add bigInt bigInt
        , benchmark2 "two negative 32-digit numbers" BI.add negativeBigInt negativeBigInt
        ]
