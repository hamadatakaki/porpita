use std::collections::HashSet;

use super::parser::{ASTNode, AST};

type Transition = (usize, Option<char>, usize);

#[derive(Clone, Debug)]
struct GeneralizedEpsilonNFA {
    transitions: HashSet<Transition>,
    start: usize,
    finish: usize,
}

impl GeneralizedEpsilonNFA {
    fn new(transitions: HashSet<Transition>, start: usize, finish: usize) -> Self {
        Self {
            transitions,
            start,
            finish,
        }
    }

    fn epsilon(from: usize) -> Self {
        let transitions = [(from, None, from + 1)]
            .iter()
            .cloned()
            .collect::<HashSet<Transition>>();
        Self::new(transitions, from, from + 1)
    }

    fn character(c: char, from: usize) -> Self {
        let transitions = [(from, Some(c), from + 1)]
            .iter()
            .cloned()
            .collect::<HashSet<Transition>>();
        Self::new(transitions, from, from + 1)
    }

    fn transition_rule(&self) -> HashSet<Transition> {
        self.transitions.clone()
    }

    fn start(&self) -> usize {
        self.start
    }
    fn finish(&self) -> usize {
        self.finish
    }
}

#[derive(Clone, Debug)]
struct NFABuilder {
    state: usize,
}

impl NFABuilder {
    fn new() -> Self {
        Self { state: 0 }
    }

    fn state(&self) -> usize {
        self.state
    }

    fn count_n(&mut self, n: usize) {
        self.state += n;
    }

    fn from_ast(&mut self, ast: AST) -> GeneralizedEpsilonNFA {
        match ast.ast_node() {
            ASTNode::Epsilon => {
                let nfa = GeneralizedEpsilonNFA::epsilon(self.state());
                self.count_n(2);
                nfa
            }
            ASTNode::Character(c) => {
                let nfa = GeneralizedEpsilonNFA::character(c, self.state());
                self.count_n(2);
                nfa
            }
            ASTNode::Concat(ast1, ast2) => {
                let nfa1 = self.from_ast(*ast1);
                let nfa2 = self.from_ast(*ast2);
                let mut transitions = nfa1
                    .transition_rule()
                    .union(&nfa2.transition_rule())
                    .cloned()
                    .collect::<HashSet<Transition>>();
                transitions.insert((nfa1.finish(), None, nfa2.start()));
                GeneralizedEpsilonNFA::new(transitions, nfa1.start(), nfa2.finish())
            }
            ASTNode::Union(ast1, ast2) => {
                let nfa1 = self.from_ast(*ast1);
                let nfa2 = self.from_ast(*ast2);
                let mut transitions = nfa1
                    .transition_rule()
                    .union(&nfa2.transition_rule())
                    .cloned()
                    .collect::<HashSet<Transition>>();
                let s = self.state();
                self.count_n(2);
                transitions.insert((s, None, nfa1.start()));
                transitions.insert((s, None, nfa2.start()));
                transitions.insert((nfa1.finish(), None, s + 1));
                transitions.insert((nfa2.finish(), None, s + 1));
                GeneralizedEpsilonNFA::new(transitions, s, s + 1)
            }
            ASTNode::Star(ast) => {
                let nfa = self.from_ast(*ast);
                let mut transitions = nfa.transition_rule();
                transitions.insert((nfa.finish(), None, nfa.start()));
                GeneralizedEpsilonNFA::new(transitions, nfa.start(), nfa.start())
            }
        }
    }
}

#[test]
fn debug_nfa() {
    use super::lexer::Lexer;
    use super::parser::Parser;

    let mut lexer = Lexer::new("a(b|c)*a");
    lexer.tokenize();
    let tokens = lexer.tokens();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    let mut builder = NFABuilder::new();
    let nfa = builder.from_ast(ast);
    println!("{:?}", nfa);
}
