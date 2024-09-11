module Benchmarks exposing (benchmarks)

import Benchmark
import Benchmark.Alternative
import Bit exposing (Bit)
import Bits
import Bitwise


benchmarks : Benchmark.Benchmark
benchmarks =
    Benchmark.describe "elm-syntax-format"
        []
