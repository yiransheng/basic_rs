// INPUT api
const INPUT = Symbol();

function input() {
  return INPUT;
}

function run(gen, x = undefined) {
  const { done, value } = gen.next(x);

  if (done) {
    return Promise.resolve(value);
  }

  switch (value) {
    case INPUT: {
      return printer.inputLine().then(number => run(gen, number));
    }
    default: {
      return run(gen, undefined);
    }
  }
}

const env = {
  run,
  input
};

var main = function*() {
  var __label__;
  var $x0;
  var $x1;
  var $x2;
  var $x3;
  var $x4;
  var $x5;
  var $x6;
  var $x7;
  $x1 = 0;
  $x2 = 0;
  $x3 = 0;
  $x4 = 0;
  $x5 = 0;
  $x6 = 0;
  $x7 = 0;
  $x3 = 0;
  $x2 = 1000;
  $x0 = $x2;
  $x1 = 1;
  $L2: while (1) {
    if ($x1 - $x0 > 0) {
      __label__ = 6;
      break $L2;
    } else {
      __label__ = 3;
    }
    $x7 = Math.random();
    $x5 = Math.random();
    $x4 = $x7 * $x7 + $x5 * $x5;
    if ($x4 > 1) {
      __label__ = 5;
    } else {
      __label__ = 4;
    }
    $L5: do {
      if (__label__ === 4) {
        $x3 = $x3 + 1;
        __label__ = 5;
        break $L5;
      }
    } while (0);
    $x1 = $x1 + 1;
    __label__ = 2;
    continue $L2;
  }
  $x6 = 4 * $x3 / $x2;
  return;
};

const { performance } = require("perf_hooks");
const readline = require("readline");

const NANOS = 1000;

const rl = readline.createInterface({
  input: process.stdin
});

rl.on("line", function(line) {
  const start = performance.now();

  const iter = parseInt(line, 10);
  for (let i = 0; i < iter; i++) {
    env.run(main());
  }

  const end = performance.now();

  console.log(Math.round((end - start) * NANOS));
});
