use std::error;
use std::fmt;
use std::mem;

use either::Either;

use crate::ast::keyword::Keyword;
use crate::ast::*;
use crate::scanner::{Error as ScanError, Scanner, SourceLoc, SourceMapped};

pub type Error = SourceMapped<ErrorInner>;

#[derive(Debug)]
pub enum ErrorInner {
    ScanError(ScanError),
    UnexpectedToken(Token),
    BadListOrTableName(Variable),
    BadSubscript(String),
    BadArgument(String),
    BadLineNo,
}
impl fmt::Display for ErrorInner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let desc = error::Error::description(self);
        match self {
            ErrorInner::ScanError(e) => e.fmt(f),
            ErrorInner::UnexpectedToken(t) => write!(f, "{}: {}", desc, t.ty()),
            ErrorInner::BadListOrTableName(var) => write!(f, "{}: {}", desc, var),
            ErrorInner::BadArgument(arg) => write!(f, "{}: {}", desc, arg),
            ErrorInner::BadSubscript(sub) => write!(f, "{}: {}", desc, sub),
            ErrorInner::BadLineNo => write!(f, "{}", desc),
        }
    }
}

impl error::Error for ErrorInner {
    fn description(&self) -> &str {
        match self {
            ErrorInner::ScanError(err) => err.description(),
            ErrorInner::UnexpectedToken(_) => "Unexpected Token",
            ErrorInner::BadListOrTableName(_) => "Invalid list/table name",
            ErrorInner::BadSubscript(_) => "Invalid list/table subscripts",
            ErrorInner::BadArgument(_) => "Invalid function argument",
            ErrorInner::BadLineNo => "Expected line number",
        }
    }
}

impl From<ScanError> for ErrorInner {
    fn from(e: ScanError) -> Self {
        ErrorInner::ScanError(e)
    }
}

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    previous: Token,
    current: Token,
    current_loc: SourceLoc,
}

macro_rules! consume_token {
    ($self:ident, $($pattern:tt)+) => {
        match &$self.current {
            $($pattern)+ => {
                $self.advance()?;
            }
            _ => return $self.unexpected_token(),
        }
    };
}

macro_rules! parse_statement {
    ($self:ident, $kw:ident, $b:block) => {{
        consume_token!($self, Token::Keyword(Keyword::$kw));

        let r = $b;

        consume_token!($self, Token::Eol | Token::Eof);

        r
    }};
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>) -> Self {
        let mut parser = Parser {
            scanner,
            previous: Token::Eof,
            current: Token::Eof,
            current_loc: SourceLoc { line: 0, col: 0 },
        };
        parser.advance().unwrap();

        parser
    }

    pub fn parse(&mut self) -> Result<Program, Error> {
        let statements = self.many(Self::statement)?;

        Ok(Program { statements })
    }

    fn statement(&mut self) -> Result<Statement, Error> {
        // skip empty lines
        match &self.current {
            Token::Eol => {
                self.advance()?;
            }
            _ => {}
        }

        let line_no = self.line_no()?;
        let keyword = &match self.current {
            Token::Keyword(kw) => kw,
            _ => return self.unexpected_token(),
        };

        let stmt = match keyword {
            Keyword::End => parse_statement!(self, End, { Stmt::End }),
            Keyword::Stop => parse_statement!(self, Stop, { Stmt::Stop }),
            Keyword::Return => parse_statement!(self, Return, { Stmt::Return }),
            Keyword::Rem => parse_statement!(self, Rem, { Stmt::Rem }),
            Keyword::Let => Stmt::Let(self.let_statement()?),
            Keyword::Read => Stmt::Read(self.read_statement()?),
            Keyword::Data => Stmt::Data(self.data_statement()?),
            Keyword::Print => Stmt::Print(self.print_statement()?),
            Keyword::Goto => Stmt::Goto(self.goto_statement()?),
            Keyword::Gosub => Stmt::Gosub(self.gosub_statement()?),
            Keyword::If => Stmt::If(self.if_statement()?),
            Keyword::For => Stmt::For(self.for_statement()?),
            Keyword::Next => Stmt::Next(self.next_statement()?),
            Keyword::Def => Stmt::Def(self.def_statement()?),
            Keyword::Dim => Stmt::Dim(self.dim_statement()?),
            Keyword::To | Keyword::Step | Keyword::Then => return self.unexpected_token(),
        };

        // skip empty lines
        match &self.current {
            Token::Eol => {
                self.advance()?;
            }
            _ => {}
        }

        Ok(Statement {
            statement: stmt,
            line_no,
        })
    }

    fn let_statement(&mut self) -> Result<LetStmt, Error> {
        let (var, expr) = parse_statement!(self, Let, {
            let var = self.variable()?;
            consume_token!(self, Token::Equal);

            let expr = self.expression()?;

            (var, expr)
        });

        Ok(LetStmt { var, expr })
    }

    fn read_statement(&mut self) -> Result<ReadStmt, Error> {
        let vars = parse_statement!(self, Read, { self.list_of(Self::variable)? });

        Ok(ReadStmt { vars })
    }

    fn data_statement(&mut self) -> Result<DataStmt, Error> {
        let vals = parse_statement!(self, Data, { self.list_of(Self::number_signed)? });

        Ok(DataStmt { vals })
    }

    fn print_statement(&mut self) -> Result<PrintStmt, Error> {
        consume_token!(self, Token::Keyword(Keyword::Print));

        let parts = match self.current {
            Token::Eof | Token::Eol => vec![],
            _ => self.many(Self::printable)?,
        };

        consume_token!(self, Token::Eol | Token::Eof);
        Ok(PrintStmt { parts })
    }

    fn goto_statement(&mut self) -> Result<GotoStmt, Error> {
        let n = parse_statement!(self, Goto, { self.line_no()? });

        Ok(GotoStmt { goto: n })
    }

    fn gosub_statement(&mut self) -> Result<GosubStmt, Error> {
        let n = parse_statement!(self, Gosub, { self.line_no()? });

        Ok(GosubStmt { goto: n })
    }

    fn if_statement(&mut self) -> Result<IfStmt, Error> {
        parse_statement!(self, If, {
            let lhs = self.expression()?;
            let op = self.relop()?;
            let rhs = self.expression()?;
            consume_token!(self, Token::Keyword(Keyword::Then));
            let then = self.line_no()?;

            Ok(IfStmt { op, lhs, rhs, then })
        })
    }

    fn for_statement(&mut self) -> Result<ForStmt, Error> {
        parse_statement!(self, For, {
            let var = self.variable()?;
            let var = match var {
                LValue::Variable(var) => var,
                _ => return self.unexpected_token(),
            };
            consume_token!(self, Token::Equal);
            let from = self.expression()?;
            consume_token!(self, Token::Keyword(Keyword::To));
            let to = self.expression()?;

            let step = match &self.current {
                Token::Keyword(Keyword::Step) => {
                    self.advance()?;
                    Some(self.expression()?)
                }
                _ => None,
            };
            Ok(ForStmt {
                var,
                from,
                to,
                step,
            })
        })
    }

    fn next_statement(&mut self) -> Result<NextStmt, Error> {
        let var = parse_statement!(self, Next, { self.variable()? });
        let var = match var {
            LValue::Variable(var) => var,
            _ => return self.unexpected_token(),
        };

        Ok(NextStmt { var })
    }

    fn def_statement(&mut self) -> Result<DefStmt, Error> {
        let (func, expr, var) = parse_statement!(self, Def, {
            let func = match &self.current {
                Token::Function(func) => *func,
                _ => return self.unexpected_token(),
            };
            self.advance()?;

            consume_token!(self, Token::OpenParen);
            let var = self.variable()?;
            consume_token!(self, Token::CloseParen);
            consume_token!(self, Token::Equal);
            let expr = self.expression()?;

            (func, expr, var)
        });

        match var {
            LValue::Variable(var) => Ok(DefStmt { func, var, expr }),
            _ => self.error_current(ErrorInner::BadArgument(var.to_string())),
        }
    }

    fn dim_statement(&mut self) -> Result<DimStmt, Error> {
        let mut lvals = parse_statement!(self, Dim, { self.list_of(Self::variable)? });

        let n = lvals.len();
        let mut dim_var: Option<Variable> = None;
        let dims = lvals
            .drain(..)
            .filter_map(|v| match v {
                LValue::List(list) => Some(Either::Left(list)),
                LValue::Table(table) => Some(Either::Right(table)),
                LValue::Variable(var) => {
                    dim_var = Some(var);
                    None
                }
            })
            .collect::<Vec<_>>();

        if let Some(var) = dim_var {
            self.error_current(ErrorInner::BadSubscript(var.to_string()))
        } else {
            Ok(DimStmt { dims })
        }
    }

    fn printable(&mut self) -> Result<Printable, Error> {
        match self.current.take() {
            Token::Label(label) => {
                self.advance()?;
                Ok(Printable::Label(label))
            }
            Token::Comma => {
                self.advance()?;
                Ok(Printable::Advance15)
            }
            Token::SemiColon => {
                self.advance()?;
                Ok(Printable::Advance3)
            }
            _ => {
                let expr = self.expression()?;
                Ok(Printable::Expr(expr))
            }
        }
    }

    fn expression(&mut self) -> Result<Expression, Error> {
        self.term()
    }

    fn term(&mut self) -> Result<Expression, Error> {
        let mut lhs = self.factor()?;
        loop {
            match &self.current {
                Token::Plus => {
                    self.advance()?;
                    let rhs = self.factor()?;
                    lhs = Expression::Add(Box::new(lhs), Box::new(rhs));
                }
                Token::Minus => {
                    self.advance()?;
                    let rhs = self.factor()?;
                    lhs = Expression::Sub(Box::new(lhs), Box::new(rhs));
                }
                _ => break,
            }
        }

        Ok(lhs)
    }
    fn factor(&mut self) -> Result<Expression, Error> {
        let mut lhs = self.power()?;
        loop {
            match &self.current {
                Token::Star => {
                    self.advance()?;
                    let rhs = self.power()?;
                    lhs = Expression::Mul(Box::new(lhs), Box::new(rhs));
                }
                Token::Slash => {
                    self.advance()?;
                    let rhs = self.power()?;
                    lhs = Expression::Div(Box::new(lhs), Box::new(rhs));
                }
                _ => break,
            }
        }

        Ok(lhs)
    }
    fn power(&mut self) -> Result<Expression, Error> {
        let mut lhs = self.unary()?;
        loop {
            match &self.current {
                Token::CaretUp => {
                    self.advance()?;
                    let rhs = self.unary()?;
                    lhs = Expression::Pow(Box::new(lhs), Box::new(rhs));
                }
                _ => break,
            }
        }

        Ok(lhs)
    }
    fn unary(&mut self) -> Result<Expression, Error> {
        match &self.current {
            Token::Minus => {
                self.advance()?;
                let expr = self.unary()?;
                let expr = Box::new(expr);
                Ok(Expression::Neg(expr))
            }
            _ => self.call(),
        }
    }
    fn call(&mut self) -> Result<Expression, Error> {
        match &self.current {
            Token::Function(func) => {
                let func = *func;
                self.advance()?;
                consume_token!(self, Token::OpenParen);
                let expr = self.expression()?;
                consume_token!(self, Token::CloseParen);

                Ok(Expression::Call(func, Box::new(expr)))
            }
            _ => self.primary(),
        }
    }
    fn primary(&mut self) -> Result<Expression, Error> {
        match &self.current {
            Token::OpenParen => {
                self.advance()?;
                let expr = self.expression()?;

                consume_token!(self, Token::CloseParen);

                Ok(expr)
            }
            Token::Natural(_) | Token::Real(_) => {
                let n = self.number()?;
                Ok(Expression::Lit(n))
            }
            Token::Varname(_) => {
                let var = self.variable()?;
                let var = Box::new(var);

                Ok(Expression::Var(var))
            }
            _ => self.unexpected_token(),
        }
    }

    fn number(&mut self) -> Result<f64, Error> {
        let n = match &self.current {
            Token::Natural(n) => *n as f64,
            Token::Real(n) => *n,
            _ => return self.unexpected_token(),
        };

        self.advance()?;

        Ok(n)
    }

    // parses negative number as well (only for data statement)
    fn number_signed(&mut self) -> Result<f64, Error> {
        match &self.current {
            Token::Minus => {
                self.advance()?;
                // allows -12, but not --12
                let n = self.number()?;
                Ok(-n)
            }
            Token::Natural(_) | Token::Real(_) => self.number(),
            _ => return self.unexpected_token(),
        }
    }

    fn variable(&mut self) -> Result<LValue, Error> {
        let var = match &self.current {
            Token::Varname(var) => *var,
            _ => return self.unexpected_token(),
        };
        self.advance()?;

        let value = match &self.current {
            Token::OpenParen => {
                if !var.can_name_list_or_table() {
                    return self.error_current(ErrorInner::BadListOrTableName(var));
                }
                self.advance()?;
                let mut subscripts = self.list_of(Self::expression)?;
                consume_token!(self, Token::CloseParen);

                match subscripts.len() {
                    1 | 2 => {}
                    _ => {
                        return self.error_current(ErrorInner::BadSubscript(format!(
                            "Wrong number of subscripts for {}",
                            var
                        )))
                    }
                }

                let s1 = subscripts.pop();
                let s0 = subscripts.pop();

                match (s0, s1) {
                    (Some(s0), Some(s1)) => {
                        let table = Table {
                            var,
                            subscript: (s0, s1),
                        };
                        LValue::Table(table)
                    }
                    (None, Some(s0)) => {
                        let list = List { var, subscript: s0 };
                        LValue::List(list)
                    }
                    _ => unreachable!(),
                }
            }
            _ => LValue::Variable(var),
        };

        Ok(value)
    }

    fn relop(&mut self) -> Result<Relop, Error> {
        let relop = match &self.current {
            Token::Greater => Relop::Greater,
            Token::GreaterEqual => Relop::GreaterEqual,
            Token::Less => Relop::Less,
            Token::LessEqual => Relop::LessEqual,
            Token::Equal => Relop::Equal,
            Token::NotEqual => Relop::NotEqual,
            _ => return self.unexpected_token(),
        };

        self.advance()?;

        Ok(relop)
    }

    fn line_no(&mut self) -> Result<LineNo, Error> {
        let n = match &self.current {
            Token::Natural(n) => *n,
            _ => return self.error_current(ErrorInner::BadLineNo),
        };

        self.advance()?;

        Ok(n as usize)
    }

    fn list_of<T, F>(&mut self, f: F) -> Result<Vec<T>, Error>
    where
        F: Fn(&mut Self) -> Result<T, Error>,
    {
        let first = f(self)?;
        let mut results = vec![first];

        loop {
            match &self.current {
                Token::Comma => {
                    self.advance()?;
                    let item = f(self)?;
                    results.push(item);
                }
                _ => break,
            }
        }

        Ok(results)
    }

    fn many<T, F>(&mut self, f: F) -> Result<Vec<T>, Error>
    where
        F: Fn(&mut Self) -> Result<T, Error>,
    {
        let first = f(self)?;
        let mut results = vec![first];

        'outter: loop {
            match &self.current {
                Token::Eof | Token::Eol => {
                    break;
                }
                _ => {
                    let item = f(self)?;
                    results.push(item);
                }
            }
        }

        Ok(results)
    }

    fn advance(&mut self) -> Result<(), Error> {
        mem::swap(&mut self.previous, &mut self.current);

        match self.scanner.scan() {
            Ok(token) => {
                self.current = token.value;
                self.current_loc = token.loc;
                Ok(())
            }
            Err(err) => Err(err.map(ScanError::into)),
        }
    }

    fn error_current<T>(&self, err: ErrorInner) -> Result<T, Error> {
        Err(SourceMapped {
            value: err,
            loc: self.current_loc,
        })
    }

    fn unexpected_token<T>(&mut self) -> Result<T, Error> {
        let t = mem::replace(&mut self.current, Token::Eof);
        self.error_current(ErrorInner::UnexpectedToken(t))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::*;

    #[test]
    fn test_parse_simple() {
        let program = indoc!(
            "
            10 REM POWER TABLE
            11 DATA 8, 4
            15 RE ADN0,P0
            20 PRINT \"N\",
            25 FOR P = 2 to P0
            30   PRINT \"N ^\" P,

            35 NEXT P
            40 PR INT \"SUM\"
            45 LET S = 0
            50 FOR N = 2 TO N0 STEP 1
            55   PRINT N,
            60   FOR P = 2 TO P0
            65     LET S = S + N ^ P
            70     PRINT N ^ P,
            75   NEXT P
            80   PRINT S
            81 DIM A(3, 2), B(9)
            82 READ A(1, 1), P9, B(3)
            83 LET Z = A(1, 2)^9 - B(3)
            85 NEXT N
            90 DEF FNA(X) = X + Y
            99 END"
        );

        let parsed = indoc!(
            "
            10 REM <comment omitted>
            11 DATA 8, 4
            15 READ N0, P0
            20 PRINT \"N\" ,
            25 FOR P = 2 TO P0
            30 PRINT \"N ^\" P ,
            35 NEXT P
            40 PRINT \"SUM\"
            45 LET S = 0
            50 FOR N = 2 TO N0 STEP 1
            55 PRINT N ,
            60 FOR P = 2 TO P0
            65 LET S = S + N^P
            70 PRINT N^P ,
            75 NEXT P
            80 PRINT S
            81 DIM A(3, 2), B(9)
            82 READ A(1, 1), P9, B(3)
            83 LET Z = A(1, 2)^9 - B(3)
            85 NEXT N
            90 DEF FNA(X) = X + Y
            99 END"
        );

        let scanner = Scanner::new(program);
        let mut parser = Parser::new(scanner);

        let ast = parser.parse();
        let ast = ast.unwrap();

        assert_eq!(parsed, &ast.to_string());
    }
}
