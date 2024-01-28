import subprocess
import os
import timeit
import json

if __name__ == "__main__":

    start_time = timeit.default_timer()

    find_zokka_cmd = ["cabal", "list-bin", "zokka"]

    benchmarks_location = "./runtime-benchmarks/core/"
    
    zokka_exec_location = \
        subprocess.run(find_zokka_cmd, capture_output=True).stdout.strip()

    print(f"=========\nRunning Benchmarks\n=========\n")

    benchmark_compilation_output = \
        subprocess.run(
            [zokka_exec_location, "make", "V8/Benchmark.elm", "--optimize", "--output", "elm.js"],
            capture_output=True, 
            cwd=benchmarks_location,
        ).stdout.strip()

    benchmark_output = \
        subprocess.run(
            ["node", "runner.js"],
            capture_output=True, 
            cwd=benchmarks_location,
        ).stdout.strip()

    print(json.loads(benchmark_output))

    total_benchmark_duration = timeit.default_timer() - start_time

    print(f"=========\nTotal benchmark duration: {total_benchmark_duration} seconds\n=========\n")
