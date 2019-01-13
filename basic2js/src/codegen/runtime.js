const INPUT = Symbol();

function input() {
  return INPUT;
}

function run(gen, x = undefined) {
  const { done, value } = gen.next(x);

  if (done) {
    return;
  }

  switch (value) {
    case INPUT: {
      getInput().then(number => {
        run(gen, number);
      });
    }
    default: {
      run(gen, undefined);
    }
  }
}
