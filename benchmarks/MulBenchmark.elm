module MulBenchmark exposing (main, benchmark)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BenchmarkShared exposing (bigInt, negativeBigInt)
import BigInt as BI


main : BenchmarkProgram
main =
    program benchmark


benchmark : Benchmark
benchmark =
    describe "mul"
        [ benchmark2 "positive 32-digit number by itself" BI.mul bigInt bigInt
        , benchmark2 "negative 32-digit number by positive 32-digit number" BI.mul negativeBigInt bigInt
        ]
