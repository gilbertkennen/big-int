module FromIntBenchmark exposing (main, benchmark)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BigInt as BI


main : BenchmarkProgram
main =
    program benchmark


benchmark : Benchmark
benchmark =
    describe "fromInt"
        [ benchmark1 "MAX_SAFE_INTEGER" BI.fromInt 9007199254740991
        ]
