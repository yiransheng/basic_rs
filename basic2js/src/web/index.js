var main = function* () {
  var _label;
  var $x0;
  var $x1;
  var $x2;
  $x0 = 0;
  $x1 = 0;
  $x2 = 0;
  env.printLabel("X, Y, Z");
  env.printAdvance15();
  env.printNewline();
  $x2 = yield env.input();
  $x1 = yield env.input();
  $x0 = yield env.input();
  env.printLabel("X is");
  env.printAdvance15();
  env.print($x2);
  env.printNewline();
  env.printLabel("X + y is");
  env.printAdvance15();
  env.print(( $x2+$x1 ));
  env.printNewline();
  return;
}
env.run(main());
