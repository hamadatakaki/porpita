use std::collections::HashSet;

use super::parser::{ASTNode, AST};

macro_rules! union {
    ($vec_mut: expr, $vec: expr) => {{
        for item in $vec {
            if !$vec_mut.contains(&item) {
                $vec_mut.push(item);
            }
        }
    }};
}

type Transition = (usize, Option<char>, usize);
type TemporaryTransition = (HashSet<usize>, char, HashSet<usize>);

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

    fn sigma(&self) -> HashSet<char> {
        self.transition_rule()
            .iter()
            .filter(|(_, x, _)| x.is_some())
            .map(|(_, c, _)| (*c).unwrap())
            .collect::<HashSet<char>>()
    }

    fn unit_trans(&self, state: usize, c: char) -> HashSet<usize> {
        self.transition_rule()
            .iter()
            .filter(|(s, x, _)| (*x == Some(c)) && (*s == state))
            .map(|(_, _, s)| *s)
            .collect::<HashSet<usize>>()
    }

    fn unit_trans_as_dfa(&self, state: HashSet<usize>, c: char) -> HashSet<usize> {
        let hs = state.iter().fold(HashSet::new(), |acc, s| {
            acc.union(&self.unit_trans(*s, c)).cloned().collect()
        });
        let mut ret = hs.clone();
        for state in hs {
            ret = ret
                .union(&self.skip_epsilon(state, &mut HashSet::new()))
                .cloned()
                .collect();
        }
        ret
    }

    fn epsilon_trans(&self, state: usize) -> HashSet<usize> {
        self.transition_rule()
            .iter()
            .filter(|(s, x, _)| (*x == None) && (*s == state))
            .map(|(_, _, s)| *s)
            .collect::<HashSet<usize>>()
    }

    fn skip_epsilon(&self, state: usize, investigated: &mut HashSet<usize>) -> HashSet<usize> {
        if investigated.contains(&state) {
            HashSet::new()
        } else {
            investigated.insert(state);
            let mut skipped = HashSet::new();
            let once_skipped = self.epsilon_trans(state);
            for &x in once_skipped.iter() {
                let next_skipped = self.skip_epsilon(x, investigated);
                skipped = skipped.union(&next_skipped).cloned().collect();
            }
            skipped.union(&once_skipped).cloned().collect()
        }
    }

    fn rec_calc_delta(
        &self,
        qs: HashSet<usize>,
        investigated: &mut Vec<HashSet<usize>>,
    ) -> Vec<TemporaryTransition> {
        // println!("investigated: {:?}", investigated);  // DEBUG
        let sigma = self.sigma();
        if qs.is_empty() {
            Vec::new()
        } else {
            let sub_delta = sigma
                .iter()
                .map(|c| (qs.clone(), *c, self.unit_trans_as_dfa(qs.clone(), *c)))
                .filter(|(_, _, x)| !x.is_empty())
                .collect::<Vec<TemporaryTransition>>();
            // DEBUG
            // println!("sub_delta:");
            // for x in sub_delta.clone() {
            //     println!("    {:?}", x);
            // }
            let finish = sub_delta
                .iter()
                .map(|(_, _, y)| y.clone())
                .collect::<Vec<HashSet<usize>>>();
            investigated.push(qs.clone());
            let mut differed: Vec<HashSet<usize>> = Vec::new();
            for x in finish {
                if !investigated.contains(&x) {
                    differed.push(x)
                }
            }
            let applied_delta = differed
                .iter()
                .map(|hs| self.rec_calc_delta(hs.clone(), investigated))
                .collect::<Vec<Vec<TemporaryTransition>>>();
            let mut v = sub_delta;
            for x in applied_delta {
                union!(v, x);
            }
            v
        }
    }

    fn calc_gnfa(
        &self,
        qs: HashSet<usize>,
    ) -> (
        HashSet<usize>,
        Vec<TemporaryTransition>,
        Vec<HashSet<usize>>,
    ) {
        let mut investigated: Vec<HashSet<usize>> = Vec::new();
        let delta = self.rec_calc_delta(qs.clone(), &mut investigated);
        let mut f = Vec::new();
        for x in investigated.clone() {
            if x.contains(&self.finish()) {
                f.push(x);
            }
        }
        (qs, delta, f)
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

    let mut lexer = Lexer::new("(a|b)*ab");
    lexer.tokenize();
    let tokens = lexer.tokens();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    let mut builder = NFABuilder::new();
    let nfa = builder.from_ast(ast);
    println!("{:?}", nfa);
    let mut s0 = nfa.skip_epsilon(nfa.start(), &mut HashSet::new());
    s0.insert(nfa.start());
    let (s0, delta, f) = nfa.calc_gnfa(s0);
    println!("{:?}, {:?}, {:?}", s0, delta, f);
}
