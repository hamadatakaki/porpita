pub type Position = (usize, usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    Character(char),
    LParen,
    RParen,
    Union,
    Star,
    EOF
}

#[derive(Clone, Debug)]
pub struct Token {
    ty: TokenType,
    pos: Position
}

impl Token {
    fn new(ty: TokenType, pos: Position) -> Self {
        Self { ty, pos }
    }

    fn eof(pos: Position) -> Self {
        Self {
            ty: TokenType::EOF,
            pos
        }
    }

    pub fn token_type(&self) -> TokenType {
        self.ty
    }

    pub fn position(&self) -> Position {
        self.pos
    }
}

pub struct Lexer {
    source: Vec<char>,
    tokens: Vec<Token>,
    look: usize
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let source = source.chars().collect::<Vec<char>>();
        Self {
            source,
            tokens: Vec::new(),
            look: 0
        }
    }

    pub fn source(&self) -> String {
        self.source.iter().collect::<String>()
    }

    pub fn tokens(&self) -> Vec<Token> {
        self.tokens.clone()
    }

    fn look_at(&self) -> char {
        self.source[self.look]
    }

    fn at_end(&self) -> bool {
        self.look == self.source.len()
    }

    fn forward(&mut self) {
        self.look += 1;
    }

    fn rec_tokenize(&mut self) {
        if !self.at_end() {
            let looked = self.look_at();
            let pos = (self.look, 1);
            let ty = match looked {
                '(' => TokenType::LParen,
                ')' => TokenType::RParen,
                '|' => TokenType::Union,
                '*' => TokenType::Star,
                c => TokenType::Character(c)
            };
            let token = Token::new(ty, pos);
            self.tokens.push(token);
            self.forward();
            self.rec_tokenize();
        }
    }

    pub fn tokenize(&mut self) {
        self.rec_tokenize();
        let pos = (self.look, 1);
        let eof = Token::eof(pos);
        self.tokens.push(eof);
    }
}

#[test]
fn debug_tokenize() {
    use super::lexer::Lexer;
    let mut lexer = Lexer::new("a(b|c)*a");
    lexer.tokenize();
    println!("{:?}", lexer.tokens());
}
