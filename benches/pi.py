import random
import sys
import time


def esti_pi(n):
    inside = 0

    for _ in range(n):
        x = random.random()
        y = random.random()
        d = x*x + y*y
        if d < 1.0:
            inside = inside + 1

    return 4.0 * inside / n

MILLIS = 1000
MICROS = MILLIS * 1000
NANOS = MICROS * 1000

def benchmark():
    n = int(sys.argv[1])

    for line in sys.stdin:
        iters = int(line.strip())
        # Setup

        start = time.perf_counter()
        for _ in range(iters):
            esti_pi(n)
        end = time.perf_counter()

        # Teardown

        delta = end - start
        nanos = int(delta * NANOS)
        # See: https://github.com/bheisler/criterion.rs/blob/63ab55d026b3e9933584a42e8990627c84af5fa2/tests/external_process.py#L31
        print("%d" % nanos)
        sys.stdout.flush()

benchmark()
