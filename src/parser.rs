use super::lexer::{Position, Token, TokenType};

#[derive(Clone, Debug)]
pub enum ASTNode {
    Epsilon,
    Character(char),
    Concat(Box<AST>, Box<AST>),
    Union(Box<AST>, Box<AST>),
    Star(Box<AST>),
}

#[derive(Clone, Debug)]
pub struct AST {
    node: ASTNode,
    pos: Position,
}

impl AST {
    fn new(node: ASTNode, pos: Position) -> Self {
        Self { node, pos }
    }

    fn epsilon(at: usize) -> Self {
        let node = ASTNode::Epsilon;
        let pos = (at, 0);
        Self::new(node, pos)
    }

    fn character(c: char, at: usize) -> Self {
        let node = ASTNode::Character(c);
        let pos = (at, 1);
        Self::new(node, pos)
    }

    fn concat(r1: Self, r2: Self) -> Self {
        let (at, _) = r1.position();
        let (to, l) = r2.position();
        let pos = (at, to - at + l);
        let node = ASTNode::Concat(Box::new(r1), Box::new(r2));
        Self::new(node, pos)
    }

    fn union(r1: Self, r2: Self) -> Self {
        let (at, _) = r1.position();
        let (to, l) = r2.position();
        let pos = (at, to - at + l);
        let node = ASTNode::Union(Box::new(r1), Box::new(r2));
        Self::new(node, pos)
    }

    fn star(r: Self) -> Self {
        let pos = (r.pos.0, r.pos.1 + 1);
        let node = ASTNode::Star(Box::new(r));
        Self::new(node, pos)
    }

    pub fn ast_node(&self) -> ASTNode {
        self.node.clone()
    }

    fn position(&self) -> Position {
        self.pos
    }

    fn to_display(&self, indent: usize) {
        for _ in 0..indent {
            print!(" ");
        }
        match self.ast_node() {
            ASTNode::Character(c) => println!("<char: {}>", c),
            ASTNode::Epsilon => println!("<epsilon>"),
            ASTNode::Concat(r1, r2) => {
                println!("<concat>");
                r1.to_display(indent + 1);
                r2.to_display(indent + 1);
            }
            ASTNode::Union(r1, r2) => {
                println!("<union>");
                r1.to_display(indent + 1);
                r2.to_display(indent + 1);
            }
            ASTNode::Star(r) => {
                println!("<star>");
                r.to_display(indent + 1);
            }
        }
    }
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.to_display(0);
        write!(f, "")
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    look: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, look: 0 }
    }

    fn forward(&mut self) {
        self.look += 1;
    }

    fn look_at(&self) -> Token {
        self.tokens[self.look].clone()
    }

    fn at_end(&self) -> bool {
        self.look == self.tokens.len()
    }

    fn check(&self, ty: TokenType) -> bool {
        self.look_at().token_type() == ty
    }

    pub fn parse(&mut self) -> AST {
        self.expr()
    }

    // expr -> union EOF
    // union -> seq Union union | seq
    // seq -> term seq
    // term -> factor Star | factor Plus | factor Question | factor
    // factor -> LParen union RParen | Escape Character | Character

    fn expr(&mut self) -> AST {
        let union = self.union();
        assert!(self.check(TokenType::EOF));
        self.forward();
        assert!(self.at_end());
        union
    }

    fn union(&mut self) -> AST {
        let seq = self.seq();
        if self.check(TokenType::Union) {
            self.forward();
            let u = self.union();
            AST::union(seq, u)
        } else {
            seq
        }
    }

    fn seq(&mut self) -> AST {
        let term = self.term();
        let looking = self.look_at();
        match looking.token_type() {
            TokenType::LParen | TokenType::Character(_) => {
                let seq = self.seq();
                AST::concat(term, seq)
            }
            TokenType::RParen | TokenType::Union | TokenType::EOF => term,
            _ => panic!(),
        }
    }

    fn term(&mut self) -> AST {
        let factor = self.factor();
        let looking = self.look_at();
        match looking.token_type() {
            TokenType::Star => {
                self.forward();
                AST::star(factor)
            }
            _ => factor,
        }
    }

    fn factor(&mut self) -> AST {
        let looking = self.look_at();
        match looking.token_type() {
            TokenType::LParen => {
                self.forward();
                let mut union = self.union();
                assert!(self.check(TokenType::RParen));
                self.forward();
                let (at, len) = union.position();
                union.pos = (at - 1, len + 2);
                union
            }
            TokenType::Character(c) => {
                let (at, _) = looking.position();
                self.forward();
                AST::character(c, at)
            }
            _ => panic!(),
        }
    }
}

#[test]
fn debug_parse() {
    use super::lexer::Lexer;
    let mut lexer = Lexer::new("aa*a");
    lexer.tokenize();
    let tokens = lexer.tokens();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("{}", ast);
}
