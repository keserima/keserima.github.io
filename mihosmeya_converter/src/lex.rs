#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    A,
    I,
    E,
    O,
    Space,
    Comma,
    Period,
    H,
    P,
    B,
    T,
    D,
    R,
    K,
    G,
    F,
    S,
    M,
    N,
    Y,
}

impl From<Token> for char {
    fn from(s: Token) -> char {
        match s {
            Token::P => 'P',
            Token::B => 'B',
            Token::H => 'H',
            Token::T => 'T',
            Token::Y => 'Y',
            Token::M => 'M',
            Token::N => 'N',
            Token::F => 'F',
            Token::G => 'G',
            Token::R => 'R',
            Token::D => 'D',
            Token::K => 'K',
            Token::S => 'S',
            Token::A => 'A',
            Token::E => 'E',
            Token::I => 'I',
            Token::O => 'O',
            Token::Space => ' ',
            Token::Comma => ',',
            Token::Period => '.',
        }
    }
}

pub fn lex(a: &str) -> Result<Vec<Token>, crate::Error> {
    let mut ans = Vec::new();
    for c in a.chars() {
        match c {
            'P' => ans.push(Token::P),
            'B' => ans.push(Token::B),
            'H' => ans.push(Token::H),
            'T' => ans.push(Token::T),
            'Y' => ans.push(Token::Y),
            'M' => ans.push(Token::M),
            'N' => ans.push(Token::N),
            'F' => ans.push(Token::F),
            'G' => ans.push(Token::G),
            'R' => ans.push(Token::R),
            'D' => ans.push(Token::D),
            'K' => ans.push(Token::K),
            'S' => ans.push(Token::S),
            'A' => ans.push(Token::A),
            'E' => ans.push(Token::E),
            'I' => ans.push(Token::I),
            'O' => ans.push(Token::O),
            ' ' => ans.push(Token::Space),
            ',' => ans.push(Token::Comma),
            '.' => ans.push(Token::Period),
            _ => {
                return Err(crate::Error(format!(
                    "unexpected character {} encountered",
                    c
                )))
            }
        }
    }

    Ok(ans)
}
