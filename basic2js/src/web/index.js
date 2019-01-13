
function* sub() {
  const a = yield env.input();
  const b = yield env.input();

  env.print(a + b);
}

function* main() {
   yield* sub();
}

env.run(main());
