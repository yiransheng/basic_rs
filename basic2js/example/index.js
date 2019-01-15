// Source: ../sample_programs/batnum.bas
// Compiler Output was run through Google Closure Compiler
"use strict";
class a {
  constructor() {}
}
class e {
  constructor() {
    this.g = [];
    this.c = 0;
  }
  add(b) {
    this.g.push(b);
    this.c++;
  }
  h() {
    if (0 < this.c) return this.c--, this.g[this.c];
    throw Error("No Data");
  }
}
class f {
  constructor(b) {
    this.m = b;
    this.g = null;
    this.c = 0;
    this.i = [];
    this.h();
    window.WordWidth ? (this.s = WordWidth) : (this.s = b => b.length);
  }
  B(b) {
    this.j(b.toString());
  }
  v(b) {
    this.j(b);
  }
  h() {
    console.log(this.i.join(""));
    this.i.length = 0;
    const b = document.createElement("CODE");
    this.m.appendChild(b);
    this.c = 0;
    this.g = b;
  }
  w() {
    return this.o().catch(() => {
      this.v("Illegal Number");
      this.h();
      return this.o();
    });
  }
  o() {
    const b = document.createElement("CODE"),
      d = document.createElement("INPUT");
    b.appendChild(d);
    this.m.appendChild(b);
    this.g = d;
    d.focus();
    this.c = 0;
    return new Promise((b, p) => {
      const q = () => {
          requestAnimationFrame(() => {
            d.disabled || d.focus();
          });
        },
        u = l => {
          13 === l.keyCode &&
            (document.removeEventListener("blur", q),
            document.removeEventListener("focus", q),
            d.removeEventListener("keyup", u),
            (d.disabled = !0),
            (l = d.value.trim())
              ? ((l = parseFloat(l)),
                Number.isNaN(l) ? p(new TypeError("not a number")) : b(l))
              : b(0));
        };
      d.addEventListener("keyup", u);
      document.addEventListener("blur", q, !0);
      document.addEventListener("focus", q, !0);
    });
  }
  u(b) {
    this.j(" ".repeat(b - this.c % b));
  }
  j(b) {
    this.i.push(b);
    "INPUT" === this.g.tagName && this.h();
    this.g.textContent += b;
    this.c += this.s(b);
  }
}
const g = new f(document.getElementById("output")),
  h = Symbol();
function k(b, d = void 0) {
  const { done: B, value: p } = b.next(d);
  if (B) return Promise.resolve(p);
  switch (p) {
    case h:
      return g.w().then(d => k(b, d));
    default:
      return k(b, void 0);
  }
}
const m = {},
  n = new e(),
  r = {
    print: b => {
      g.B(b);
    },
    a: () => {
      g.h();
    },
    b: b => {
      g.v(b);
    },
    f: () => {
      g.u(3);
    },
    l: () => {
      g.u(15);
    },
    D: (b, d) => b * Math.sign(d),
    C: b => {
      n.add(b);
    },
    read: () => n.h(),
    G: b => {
      m[b] = new a();
    },
    F: b => m[b],
    A: k,
    input: function() {
      return h;
    }
  };
window.c = r;
var t = 0,
  v = 0,
  w = 0,
  x = 0,
  y = 0,
  z = 0,
  A = 0;
function* C() {
  var b = t;
  var d = 1 === w ? 2 : 1;
  a: do
    if (1 === d) {
      --b;
      break a;
    }
  while (0);
  d = 1 === w ? 10 : 3;
  a: do {
    if (3 === d) {
      if (t > z) break a;
      A = 1;
      r.b("COMPUTER TAKES");
      r.f();
      r.print(t);
      r.f();
      r.b("AND LOSES.");
      r.a();
      return;
    }
    if (10 === d) {
      if (t > x) break a;
      A = 1;
      r.b("COMPUTER TAKES");
      r.f();
      r.print(t);
      r.f();
      r.b("AND WINS.");
      r.a();
      return;
    }
  } while (0);
  y = b - v * Math.trunc(b / v);
  d = y < z ? 6 : 7;
  a: do
    if (6 === d) {
      y = z;
      break a;
    }
  while (0);
  d = y > x ? 8 : 9;
  a: do
    if (8 === d) {
      y = x;
      break a;
    }
  while (0);
  t -= y;
  r.b("COMPUTER TAKES");
  r.f();
  r.print(y);
  r.f();
  r.b("AND LEAVES");
  r.f();
  r.print(t);
  r.a();
  A = 0;
}
function* D() {
  r.a();
  r.b("YOUR MOVE ");
  r.f();
  a: for (;;) {
    r.a();
    y = yield r.input();
    if (0 === y) {
      var b = 2;
      break a;
    }
    b = y !== Math.trunc(y) ? 7 : 4;
    b: do
      if (4 === b)
        if (((b = y < z ? 5 : 11), 11 === b)) {
          if (y > x) break b;
          t -= y;
          if (0 === t) {
            b = 8;
            break a;
          }
          if (!(0 > t)) {
            b = 15;
            break a;
          }
          t += y;
          break b;
        } else if (5 === b) {
          if (y === t) {
            b = 8;
            break a;
          }
          break b;
        }
    while (0);
    r.b("ILLEGAL MOVE, REENTER IT ");
    r.f();
  }
  8 === b
    ? ((b = 1 === w ? 10 : 9),
      9 === b
        ? (r.b("TOUGH LUCK, YOU LOSE."), r.a(), (A = 1))
        : 10 === b && (r.b("CONGRATULATIONS, YOU WIN."), r.a(), (A = 1)))
    : 2 === b
      ? (r.b("I TOLD YOU NOT TO USE ZERO! COMPUTER WINS BY FORFEIT."),
        r.a(),
        (A = 1))
      : 15 === b && (A = 0);
}
r.A(
  (function*() {
    r.b("BATNUM");
    r.a();
    r.b("CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY");
    r.a();
    r.a();
    r.a();
    r.a();
    r.b("THIS PROGRAM IS A 'BATTLE OF NUMBERS' GAME, WHERE THE");
    r.a();
    r.b("COMPUTER IS YOUR OPPONENT.");
    r.a();
    r.a();
    r.b("THE GAME STARTS WITH AN ASSUMED PILE OF OBJECTS. YOU");
    r.a();
    r.b("AND YOUR OPPONENT ALTERNATELY REMOVE OBJECTS FROM THE PILE.");
    r.a();
    r.b("WINNING IS DEFINED IN ADVANCE AS TAKING THE LAST OBJECT OR");
    r.a();
    r.b("NOT. YOU CAN ALSO SPECIFY SOME OTHER BEGINNING CONDITIONS.");
    r.a();
    r.b("DON'T USE ZERO, HOWEVER, IN PLAYING THE GAME.");
    r.a();
    r.b("ENTER A NEGATIVE NUMBER FOR NEW PILE SIZE TO STOP PLAYING.");
    r.a();
    r.a();
    a: for (;;) {
      r.b("ENTER PILE SIZE");
      r.l();
      r.a();
      t = yield r.input();
      var b = 1 > t ? 2 : 3;
      if (3 === b) {
        b = t !== Math.trunc(t) ? 17 : 4;
        b: do
          if (4 === b) {
            if (1 > t) break b;
            c: for (;;) {
              r.b("ENTER WIN OPTION - 1 TO TAKE LAST, 2 TO AVOID LAST: ");
              r.l();
              r.a();
              w = yield r.input();
              if (1 === w) break c;
              if (2 === w) break c;
            }
            c: for (;;) {
              r.b("ENTER MIN AND MAX ");
              r.f();
              r.a();
              z = yield r.input();
              x = yield r.input();
              if (z > x) continue c;
              if (1 > z) continue c;
              if (z !== Math.trunc(z)) continue c;
              if (x === Math.trunc(x)) break c;
            }
            c: for (;;) {
              r.b("ENTER START OPTION - 1 COMPUTER FIRST, 2 YOU FIRST ");
              r.l();
              r.a();
              b = yield r.input();
              r.a();
              r.a();
              if (1 === b) break c;
              if (2 === b) break c;
            }
            v = z + x;
            b = 2 === b ? 15 : 14;
            for (;;)
              if (15 === b) {
                yield* D();
                if (1 === A) break b;
                b = 14;
              } else if (14 === b)
                if ((yield* C(), 1 === A)) break b;
                else b = 15;
          }
        while (0);
        b = 1;
        for (;;) {
          if (0 < b - 10) continue a;
          r.a();
          b += 1;
        }
      }
    }
  })()
);
