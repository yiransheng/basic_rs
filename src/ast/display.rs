use std::fmt;

use super::*;

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Func::*;

        match self {
            Sin => write!(f, "SIN"),
            Cos => write!(f, "COS"),
            Tan => write!(f, "TAN"),
            Atn => write!(f, "ATN"),
            Exp => write!(f, "EXP"),
            Abs => write!(f, "ABS"),
            Log => write!(f, "LOG"),
            Sqr => write!(f, "SQR"),
            Rnd => write!(f, "RND"),
            Int => write!(f, "INT"),
            Fna => write!(f, "FNA"),
            Fnb => write!(f, "FNB"),
            Fnc => write!(f, "FNC"),
            Fnd => write!(f, "FND"),
            Fne => write!(f, "FNE"),
            Fnf => write!(f, "FNF"),
            Fng => write!(f, "FNG"),
            Fnh => write!(f, "FNH"),
            Fni => write!(f, "FNI"),
            Fnj => write!(f, "FNJ"),
            Fnk => write!(f, "FNK"),
            Fnl => write!(f, "FNL"),
            Fnm => write!(f, "FNM"),
            Fnn => write!(f, "FNN"),
            Fno => write!(f, "FNO"),
            Fnp => write!(f, "FNP"),
            Fnq => write!(f, "FNQ"),
            Fnr => write!(f, "FNR"),
            Fns => write!(f, "FNS"),
            Fnt => write!(f, "FNT"),
            Fnu => write!(f, "FNU"),
            Fnv => write!(f, "FNV"),
            Fnw => write!(f, "FNW"),
            Fnx => write!(f, "FNX"),
            Fny => write!(f, "FNY"),
            Fnz => write!(f, "FNZ"),
        }
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.var.fmt(f)?;
        write!(f, "(")?;
        self.subscript.fmt(f)?;
        write!(f, ")")
    }
}

impl fmt::Display for Table {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.var.fmt(f)?;
        write!(f, "(")?;
        self.subscript.0.fmt(f)?;
        write!(f, ", ")?;
        self.subscript.1.fmt(f)?;
        write!(f, ")")
    }
}

impl fmt::Display for Relop {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Relop::*;
        match self {
            Less => write!(f, "<"),
            LessEqual => write!(f, "<="),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            Equal => write!(f, "="),
            NotEqual => write!(f, "<>"),
        }
    }
}

impl fmt::Display for LValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::LValue::*;

        match self {
            Variable(ref v) => v.fmt(f),
            List(ref v) => v.fmt(f),
            Table(ref v) => v.fmt(f),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Expression::*;

        match self {
            Lit(n) => n.fmt(f),
            Var(ref v) => v.fmt(f),
            Call(ref func, ref expr) => {
                func.fmt(f)?;
                write!(f, "(")?;
                expr.fmt(f)?;
                write!(f, ")")
            }
            Neg(ref a) => {
                write!(f, "-")?;
                a.fmt(f)
            }
            Add(ref a, ref b) => {
                a.fmt(f)?;
                write!(f, " + ")?;
                b.fmt(f)
            }
            Sub(ref a, ref b) => {
                a.fmt(f)?;
                write!(f, " - ")?;
                b.fmt(f)
            }
            Mul(ref a, ref b) => {
                a.fmt(f)?;
                write!(f, " * ")?;
                b.fmt(f)
            }
            Div(ref a, ref b) => {
                a.fmt(f)?;
                write!(f, " / ")?;
                b.fmt(f)
            }
            Pow(ref a, ref b) => {
                a.fmt(f)?;
                write!(f, "^")?;
                b.fmt(f)
            }
        }
    }
}

impl fmt::Display for Printable {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Printable::*;

        match self {
            Label(ref s) => {
                write!(f, "\"")?;
                s.fmt(f)?;
                write!(f, "\"")
            }
            Expr(ref v) => v.fmt(f),
            Advance3 => write!(f, ";"),
            Advance5 => write!(f, ","),
        }
    }
}

const COMMA_SEP: &str = ", ";
const SPACE_SEP: &str = " ";
const NEWLINE_SEP: &str = "\n";

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        intersperse(&self.statements, &NEWLINE_SEP, f)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} {}", self.line_no, self.statement)
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Stmt::*;

        match self {
            Let(ref v) => v.fmt(f),
            Read(ref v) => v.fmt(f),
            Data(ref v) => v.fmt(f),
            Print(ref v) => v.fmt(f),
            Goto(ref v) => v.fmt(f),
            Gosub(ref v) => v.fmt(f),
            If(ref v) => v.fmt(f),
            For(ref v) => v.fmt(f),
            Next(ref v) => v.fmt(f),
            Def(ref v) => v.fmt(f),
            Dim(ref v) => v.fmt(f),
            Rem => write!(f, "REM <comment omitted>"),
            End => write!(f, "END"),
            Stop => write!(f, "STOP"),
            Return => write!(f, "RETURN"),
        }
    }
}

impl fmt::Display for LetStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "LET ")?;
        self.var.fmt(f)?;
        write!(f, " = ")?;
        self.expr.fmt(f)
    }
}

impl fmt::Display for ReadStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "READ ")?;
        intersperse(&self.vars, &COMMA_SEP, f)
    }
}

impl fmt::Display for DataStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "DATA ")?;
        intersperse(&self.vals, &COMMA_SEP, f)
    }
}

impl fmt::Display for PrintStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "PRINT ")?;
        intersperse(&self.parts, &SPACE_SEP, f)
    }
}

impl fmt::Display for GotoStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "GOTO ")?;
        self.goto.fmt(f)
    }
}

impl fmt::Display for GosubStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "GOSUB ")?;
        self.goto.fmt(f)
    }
}

impl fmt::Display for IfStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "IF {} {} {} THEN {}",
            self.lhs, self.op, self.rhs, self.then
        )
    }
}

impl fmt::Display for ForStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "FOR {} = {} TO {}", self.var, self.from, self.to)?;

        if let Some(ref step) = self.step {
            write!(f, " STEP {}", step)
        } else {
            Ok(())
        }
    }
}

impl fmt::Display for NextStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "NEXT ")?;
        self.var.fmt(f)
    }
}

impl fmt::Display for DefStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "DEF {}({}) = {}", self.func, self.var, self.expr)
    }
}

impl fmt::Display for DimStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "DIM ")?;
        intersperse(&self.dims, &COMMA_SEP, f)?;

        Ok(())
    }
}

fn intersperse<T: fmt::Display, U: fmt::Display>(
    xs: &[T],
    sep: &U,
    f: &mut fmt::Formatter,
) -> Result<(), fmt::Error> {
    let mut first = true;
    for x in xs {
        if first {
            first = false;
        } else {
            sep.fmt(f)?;
        }
        x.fmt(f)?;
    }

    Ok(())
}
