class Printer {
  constructor(el) {
    this._container = el;
    this._element = null;
    this._line = -1;
    this._col = 0;
    this._decoder = new TextDecoder("utf-8");
    this._buffer = null;

    this.newline();
  }

  setBuffer(wasmInstance) {
    this._buffer = wasmInstance.exports.data.buffer;
  }

  printNumber(n) {
    const str = n.toString();
    this._write(str);
  }
  printStr(offset, len) {
    if (!this._buffer) {
      return;
    }
    const array = new Uint8Array(this._buffer, offset, len);
    const str = this._decoder.decode(array);
    this._write(str);
  }

  newline() {
    const line = document.createElement("CODE");
    this._container.appendChild(line);

    this._col = 0;
    this._line += 1;

    this._element = line;
  }

  advanceMultiple(k) {
    const rem = this._col % k;
    const n = k - rem;
    this._write(" ".repeat(n));
    this._col += n;
  }

  _write(str) {
    this._element.textContent += str;
    this._col += str.length;
  }
}

const printer = new Printer(document.getElementById("output"));

const importObject = {
  env: {
    print: number => {
      printer.printNumber(number);
    },
    printNewline: () => {
      printer.newline();
    },
    printLabel: (offset, len) => {
      printer.printStr(offset, len);
    },
    printAdvance3: () => {
      printer.advanceMultiple(3);
    },
    printAdvance15: () => {
      printer.advanceMultiple(15);
    },
    rand: () => {
      return Math.random();
    },
    pow: (a, b) => {
      return Math.pow(a, b);
    }
  }
};

fetch("main.wasm")
  .then(response => response.arrayBuffer())
  .then(bytes => WebAssembly.instantiate(bytes, importObject))
  .then(results => {
    instance = results.instance;
    window.wasm = instance;
    printer.setBuffer(instance);

    instance.exports.main();
  })
  .catch(console.error.bind(console));
