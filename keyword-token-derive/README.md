# Compile Time DFA Token Matching

This is a procedure macro to auto-derive a parser for `enum` tokens without any data, following BASIC's insignificant spaces rule. Generated code matches an `enum` variant based on a compile-time DFA.

The trait definition, similar to `FromStr`:

```rust
trait KeywordToken: Copy {
    fn parse_from_str(s: &str) -> Option<Self>
    where
        Self: Sized;
}
```

For example, with this simple `enum`:

```rust
#[derive(KeywordToken, Copy, Clone, Eq, PartialEq)]
enum TestIt {
    Foo,
    Bar,
    Baz,
}
```

Generated `parse_from_str` function matches the following, parsing is case-insensitive, and ignores non-newline white spaces.

```rust
assert_eq!(TestIt::parse_from_str("BAR"), Some(TestIt::Bar));
assert_eq!(TestIt::parse_from_str("  f o    O"), Some(TestIt::Foo));
```

Here is the generated code. It is essentially a compile-time `Trie`. Finite state machine's internal state is represented using `Result<T, Option<usize>>` instead of a better named `enum` type, to avoid introducing new types in proc macro code. Transition functions are expressed as closures, and hopefully they do get optimized away.

```rust
trait KeywordToken: Copy {
    fn parse_from_str(s: &str) -> Option<Self>
    where
        Self: Sized;
}

#[derive(Copy, Clone)]
enum TestIt {
    Foo,
    Bar,
    Baz,
}

impl KeywordToken for TestIt {
    fn parse_from_str(s: &str) -> Option<Self> {
        let mut state: Result<Self, Option<usize>> = Err(Some(0));
        let mut chars = s.chars();
        let _state_5 = |c: char| -> Result<TestIt, Option<usize>> {
            match c {
                '\u{61}' => Err(Some(6usize)),
                '\u{41}' => Err(Some(6usize)),
                a if a.is_whitespace() && a != '\n' => Err(Some(5usize)),
                _ => Err(None),
            }
        };
        let _state_0 = |c: char| -> Result<TestIt, Option<usize>> {
            match c {
                '\u{66}' => Err(Some(1usize)),
                '\u{46}' => Err(Some(1usize)),
                '\u{62}' => Err(Some(5usize)),
                '\u{42}' => Err(Some(5usize)),
                a if a.is_whitespace() && a != '\n' => Err(Some(0usize)),
                _ => Err(None),
            }
        };
        let _state_6 = |c: char| -> Result<TestIt, Option<usize>> {
            match c {
                '\u{7a}' => Ok(TestIt::Baz),
                '\u{5a}' => Ok(TestIt::Baz),
                '\u{72}' => Ok(TestIt::Bar),
                '\u{52}' => Ok(TestIt::Bar),
                a if a.is_whitespace() && a != '\n' => Err(Some(6usize)),
                _ => Err(None),
            }
        };
        let _state_2 = |c: char| -> Result<TestIt, Option<usize>> {
            match c {
                '\u{6f}' => Ok(TestIt::Foo),
                '\u{4f}' => Ok(TestIt::Foo),
                a if a.is_whitespace() && a != '\n' => Err(Some(2usize)),
                _ => Err(None),
            }
        };
        let _state_1 = |c: char| -> Result<TestIt, Option<usize>> {
            match c {
                '\u{6f}' => Err(Some(2usize)),
                '\u{4f}' => Err(Some(2usize)),
                a if a.is_whitespace() && a != '\n' => Err(Some(1usize)),
                _ => Err(None),
            }
        };
        loop {
            if let Ok(x) = state {
                return Some(x);
            }
            let c = chars.next()?;
            state = match state {
                Err(None) => return None,
                Err(Some(5usize)) => _state_5(c),
                Err(Some(0usize)) => _state_0(c),
                Err(Some(6usize)) => _state_6(c),
                Err(Some(2usize)) => _state_2(c),
                Err(Some(1usize)) => _state_1(c),
                _ => unreachable!(),
            }
        }
    }
}

```

