class Cell {
  constructor() {
    this._value = null;
  }
  get value() {
    return this._value;
  }
  set value(x) {
    this._value = x;
  }
}

class SparseArray {
  constructor() {
    this._map = new Map();
  }
  index1d(i) {
    return this._cell(this._hash1d(i));
  }
  index2d(i) {
    return this._cell(this._hash2d(i));
  }
  _cell(hash) {
    let cell;
    if (this._map.has(hash)) {
      cell = this._map.get(hash);
    } else {
      cell = new Cell();
      this._map.set(hash, value);
    }

    return cell;
  }

  _hash1d(i) {
    // assert(i >= 0);
    return this._hash2d(i, 0);
  }
  _hash2d(i, j) {
    // assert(i >= 0 && j >= 0);
    j = j + 1;
    return (i + j) * (i + j + 1) / 2 + j;
  }
}

class Data {
  constructor() {
    this._values = [];
    this._count = 0;
  }
  reset() {
    this._count = this._values.length;
  }
  add(v) {
    this._values.push(v);
    this._count++;
  }
  pop() {
    if (this._count > 0) {
      this._count--;
      return this._values(this._count);
    } else {
      throw Error("No Data");
    }
  }
}

class Printer {
  constructor(el) {
    this._container = el;
    this._element = null;
    this._line = -1;
    this._col = 0;
    this._currentLine = [];

    this.newline();

    if (window.WordWidth) {
      this._widthOf = WordWidth;
    } else {
      this._widthOf = s => s.length;
    }
  }

  printNumber(n) {
    const str = n.toString();
    this._write(str);
  }
  printStr(str) {
    this._write(str);
  }

  newline() {
    console.log(this._currentLine.join(""));
    this._currentLine.length = 0;
    const line = document.createElement("CODE");
    this._container.appendChild(line);

    this._col = 0;
    this._line += 1;

    this._element = line;
  }

  inputLine() {
    const line = document.createElement("CODE");
    const input = document.createElement("INPUT");

    line.appendChild(input);

    this._container.appendChild(line);
    this._element = input;

    input.focus();

    this._col = 0;
    this._line += 1;

    return new Promise((resolve, reject) => {
      const onBlur = () => {
        requestAnimationFrame(() => {
          if (!input.disabled) {
            input.focus();
          }
        });
      };
      const onKey = e => {
        if (e.keyCode === 13) {
          onEnter();
        }
      };
      const onEnter = () => {
        document.removeEventListener("blur", onBlur);
        document.removeEventListener("focus", onBlur);
        input.removeEventListener("keyup", onKey);
        input.disabled = true;
        // this.newline();

        const v = input.value.trim();
        if (!v) {
          resolve(0);
        } else {
          const n = parseFloat(v);
          if (Number.isNaN(n)) {
            reject(new TypeError("not a number"));
          } else {
            resolve(n);
          }
        }
      };
      input.addEventListener("keyup", onKey);
      document.addEventListener("blur", onBlur, true);
      document.addEventListener("focus", onBlur, true);
    });
  }

  advanceMultiple(k) {
    const rem = this._col % k;
    const n = k - rem;
    this._write(" ".repeat(n));
  }

  _write(str) {
    this._currentLine.push(str);
    if (this._element.tagName === "INPUT") {
      this.newline();
    }
    this._element.textContent += str;
    this._col += this._widthOf(str);
  }
}

const printer = new Printer(document.getElementById("output"));

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

const arrays = {};
const data = new Data();

const env = {
  print: number => {
    printer.printNumber(number);
  },
  printNewline: () => {
    printer.newline();
  },
  printLabel: (str) => {
    printer.printStr(str);
  },
  printAdvance3: () => {
    printer.advanceMultiple(3);
  },
  printAdvance15: () => {
    printer.advanceMultiple(15);
  },
  copySign: (a, b) => {
    return a * Math.sign(b);
  },
  addData: x => {
    data.add(x);
  },
  read: () => {
    return data.pop();
  },
  newArray: name => {
    arrays[name] = new SparseArray();
  },
  getArray: name => {
    return arrays[name];
  },
  run,
  input
};

window.env = env;
