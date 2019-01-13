const { performance } = require("perf_hooks");
const readline = require("readline");

function pi(n) {
  let inside = 0;
  for (let i = 0; i < n; i++) {
    let x = Math.random();
    let y = Math.random();
    let d = x * x + y * y;
    if (d < 1) {
      inside++;
    }
  }

  return 4 * inside / n;
}

const NANOS = 1000;

const rl = readline.createInterface({
  input: process.stdin
});

rl.on("line", function(line) {
  const start = performance.now();

  const iter = parseInt(line, 10);
  for (let i = 0; i < iter; i++) {
    pi(1000);
  }

  const end = performance.now();

  console.log(Math.round((end - start) * NANOS));
});
