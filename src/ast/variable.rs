use std::error;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::str;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Variable([u8; 2]);

#[derive(Debug, Copy, Clone)]
pub enum NameError {
    NotLetter,
    NotDigit,
    LowerCase,
    TooLong,
    TooShort,
}

impl fmt::Display for NameError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(std::error::Error::description(self))
    }
}

impl error::Error for NameError {
    fn description(&self) -> &str {
        match self {
            NameError::NotLetter => "Variable name must start with an alphabet.",
            NameError::NotDigit => "Second character in variable name must be a digit (0-9).",
            NameError::LowerCase => "Variable name must use upper case alphabets.",
            NameError::TooLong => "Variable name too long",
            NameError::TooShort => "Variable name too short",
        }
    }
}

fn u16_to_bytes(b: u16) -> [u8; 2] {
    let b1 = (b & 0xff) as u8;
    let b2 = ((b >> 8) & 0xff) as u8;
    [b1, b2]
}

impl Variable {
    pub fn from_u16(b: u16) -> Result<Self, NameError> {
        let [b1, b2] = u16_to_bytes(b);
        if b2 == 0 {
            Self::from_byte(b1 as u8)
        } else {
            Self::from_bytes(b1 as u8, b2 as u8)
        }
    }
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

    pub fn from_bytes_unchecked(bytes: [u8; 2]) -> Self {
        Variable(bytes)
    }
    pub fn from_u16_unchecked(b: u16) -> Self {
        Variable(u16_to_bytes(b))
    }

    // Requires List of Table Name to be A-Z (no follow up digit)
    pub fn can_name_list_or_table(&self) -> bool {
        self.0[1] == 0
    }

    pub fn raw(self) -> [u8; 2] {
        self.0
    }

    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        let b0 = self.0[0];
        let b1 = self.0[1];

        match (b0, b1) {
            (b'A'...b'Z', 0) => write!(f, "{}", b0 as char),
            (b'A'...b'Z', b'0'...b'9') => write!(f, "{}{}", b0 as char, b1 as char),
            _ => write!(f, "<anonymous variable {}{}>", b1, b0),
        }
    }
}

impl Into<[u8; 2]> for Variable {
    fn into(self) -> [u8; 2] {
        self.0
    }
}
impl str::FromStr for Variable {
    type Err = NameError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.len() {
            0 => Err(NameError::TooShort),
            1 => Variable::from_byte(s.bytes().next().unwrap()),
            2 => {
                let mut bytes = s.bytes();
                let b1 = bytes.next().unwrap();
                let b2 = bytes.next().unwrap();
                Variable::from_bytes(b1, b2)
            }
            _ => Err(NameError::TooLong),
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
