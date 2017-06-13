module ToStringBenchmark exposing (main, benchmark)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program benchmark


benchmark : Benchmark
benchmark =
    describe "toString"
        [ benchmark1 "32-digit number" BI.mul bigInt
        ]
