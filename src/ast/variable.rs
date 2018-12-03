#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Variable([u8; 2]);

#[derive(Debug, Copy, Clone)]
pub enum NameError {
    NotLetter,
    NotDigit,
    LowerCase,
}

impl Variable {
    pub fn from_byte(b: u8) -> Result<Self, NameError> {
        match b {
            b'A'...b'Z' => Ok(Variable([b, 0])),
            b'a'...b'z' => Err(NameError::LowerCase),
            _ => Err(NameError::NotLetter),
        }
    }
    pub fn from_bytes(a: u8, d: u8) -> Result<Self, NameError> {
        match d {
            b'0'...b'9' => {}
            _ => return Err(NameError::NotDigit),
        }
        match a {
            b'A'...b'Z' => Ok(Variable([a, d])),
            _ => Err(NameError::NotLetter),
        }
    }
    pub fn can_name_list_or_table(&self) -> bool {
        self.0[1] == 0
    }

    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        let b0 = self.0[0];
        let b1 = self.0[1];

        match b0 as char {
            c if c.is_alphabetic() => {
                write!(f, "{}", c)?;
            }
            _ => return Ok(()),
        }
        match b1 as char {
            c if c.is_digit(10) => write!(f, "{}", c),
            _ => Ok(()),
        }
    }
}

impl ::std::fmt::Display for Variable {
    #[inline]
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        self.fmt(f)
    }
}
impl ::std::fmt::Debug for Variable {
    #[inline]
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        self.fmt(f)
    }
}

impl Variable {
    fn hash_usize(&self) -> usize {
        let b0 = self.0[0]; // b'A'-b'Z'
        let b1 = self.0[1]; // 0 or b'0' - b'9'

        let b0 = (b0 % 26) as usize;

        if b1 == 0 {
            b0
        } else {
            let b1 = ((b1 % 10 + 1) as usize) << 5;
            b1 | b0
        }
    }
}
