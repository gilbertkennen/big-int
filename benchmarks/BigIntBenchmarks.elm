module BigIntBenchmarks exposing (main)

import AbsBenchmark
import Benchmark exposing (describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)


-- Individual Benchmarks

import AbsBenchmark
import AddBenchmark
import CompareBenchmark
import DivmodBenchmark
import FromIntBenchmark
import FromStringBenchmark
import MulBenchmark
import ToStringBenchmark


main : BenchmarkProgram
main =
    program <|
        describe "BigInt"
            [ AbsBenchmark.benchmark
            , AddBenchmark.benchmark
            , CompareBenchmark.benchmark
            , DivmodBenchmark.benchmark
            , FromIntBenchmark.benchmark
            , FromStringBenchmark.benchmark
            , MulBenchmark.benchmark
            , ToStringBenchmark.benchmark
            ]
