const START_STATE = 0;

function invariant(cond, msg) {
  if (!cond) {
    throw Error(msg);
  }
}

class DFA {
  constructor(words) {
    this._transitions = [];
    this._finalStates = [...words];

    this._states = [START_STATE];

    this._stateCounter = words.length + this._states.length;

    for (const word of words) {
      invariant(word.length, "Empty word");
      this._addWord(word, START_STATE, word);
    }
  }

  _nextStateID() {
    return this._stateCounter++;
  }

  _addState() {
    const id = this._nextStateID();
    this._states.push(id);
    return id;
  }

  _addWord(word, startState, finalState) {
    if (!word.length) {
      return;
    }

    const a = word.charAt(0);
    const rest = word.slice(1);

    let transition = this._transitions.find(([s, t, ns]) => {
      return s === startState && t === a;
    });

    if (!transition) {
      const nextState = rest.length ? this._addState() : finalState;
      transition = [startState, a, nextState];
      this._transitions.push(transition);
    }

    this._addWord(rest, transition[2], finalState);
  }
}

class RsBlock {
  constructor(opener, children) {
    this._opener = opener;
    this._children = children;
  }
  toString(indent) {
    const childLines = this._children.map(c => c.toString(indent + 1));

    return (
      `${spaces(indent)}${this._opener} {\n` +
      childLines.join("\n") +
      `\n${spaces(indent)}}`
    );
  }
}

class RsDfaFn {
  constructor(rsEnum, dfa) {
    this._rsEnum = rsEnum;
    this._dfa = dfa;
  }
  stateType() {
    return `State<${this._rsEnum}>`;
  }
  fnSig() {
    return `fn _dfa(state: ${this.stateType()}, t: char) -> ${this.stateType()}`;
  }
  toString(indent = 0) {
    const fns = this._dfa._states.map(
      s => new RsStateFn(s, this._dfa, this._rsEnum)
    );

    const matchBlock = new RsBlock("match state", [
      new FnMatched(),
      ...fns.map(fn => new FnMatchArm(fn)),
      new FnMatchFail()
    ]);

    const fnBlock = new RsBlock(this.fnSig(), [matchBlock]);

    return (
      fnBlock.toString(indent) +
      "\n\n" +
      fns.map(x => x.toString(indent)).join("\n\n")
    );
  }
}

class FnMatched {
  toString(indent = 0) {
    return `${spaces(indent)}State::Matched(_) => State::Fail,`;
  }
}

class FnMatchFail {
  toString(indent = 0) {
    return `${spaces(indent)}_ => State::Fail,`;
  }
}

class FnMatchArm {
  constructor(fn) {
    this._fn = fn;
  }
  toString(indent = 0) {
    return `${spaces(indent)}${toRsState(
      this._fn._state,
      this._fn._rsEnum
    )} => ${this._fn.fnName()}(t),`;
  }
}

class RsStateFn {
  constructor(state, dfa, rsEnum) {
    this._state = state;
    this._dfa = dfa;
    this._rsEnum = rsEnum;
  }
  fnRetType() {
    return `State<${this._rsEnum}>`;
  }
  fnName() {
    return `_state_${this._state}`;
  }
  matchArams() {
    const arms = this._dfa._transitions
      .filter(([s, t, ns]) => {
        return s === this._state;
      })
      .map(([s, t, ns]) => {
        return new MatchArm(this._rsEnum, ns, `'${t.toUpperCase()}' | '${t.toLowerCase()}'`);
      });

    arms.push(new MatchArm(this._rsEnum, this._state, "a if a.is_whitespace() && a != '\\n'"));
    arms.push(new MatchArm(this._rsEnum, null, "_"));

    return arms;
  }
  toString(indent = 0) {
    const matchBlock = new RsBlock("match t", this.matchArams());
    const fnBlock = new RsBlock(
      `fn ${this.fnName()} (t: char) -> ${this.fnRetType()}`,
      [matchBlock]
    );

    const attrLine = `${spaces(indent)}#[inline(always)]`;

    return attrLine + "\n" + fnBlock.toString(indent);
  }
}

class MatchArm {
  constructor(rsEnum, toState, symbol) {
    this._rsEnum = rsEnum;
    this._toState = toState;
    this._t = symbol;
  }
  toString(indent = 0) {
    return `${spaces(indent)}${this._t} => ${toRsState(
      this._toState,
      this._rsEnum
    )},`;
  }
}

function spaces(indent = 0) {
  return " ".repeat(indent * 4);
}

function capitalize(word) {
  const a = word.charAt(0);
  const rest = word.slice(1);

  return `${a.toUpperCase()}${rest.toLowerCase()}`;
}

function toRsState(state, rsEnum) {
  if (state === null) {
    return "State::Fail";
  }

  switch (typeof state) {
    case "number":
      return `State::Intermediate(${state})`;
    case "string":
      return `State::Matched(${rsEnum}::${capitalize(state)})`;
    default:
      throw Error("Invalid state");
  }
}

class EnumVariant {
  constructor(rsEnum, variant) {
    this._rsEnum = rsEnum;
    this._variant = variant;
  }
  toString(indent = 0) {
    return `${spaces(indent)}${capitalize(this._variant)},`;
  }
}

function codegen(rsEnum, words) {
  const dfa = new DFA(words);
  const rsDfa = new RsDfaFn(rsEnum, dfa);

  return rsDfa.toString();
}

function typegen(rsEnum, words) {
  const enumBlock = new RsBlock(
    `pub enum ${rsEnum}`,
    words.map(w => new EnumVariant(rsEnum, w)),
  );

  return '#[derive(Debug, Copy, Clone, Eq, PartialEq)]\n' + enumBlock.toString(0);
}

module.exports = {
  codegen,
  typegen
};

