use std::fmt::Debug;
use std::marker::PhantomData;
use thiserror::Error;

pub trait InputClass: Debug + Copy + Sized {
    fn classify(c: char) -> Self;
}

pub trait Token: Sized {
    fn emit(s: String) -> Self;
    fn append(s: String, last: Option<&mut Self>) -> Option<Self>;
}

pub trait StateTransitionTable<IC>: Debug + Copy
where
    IC: InputClass,
{
    fn transition(self, class: Option<IC>) -> (Self, LexerAction);
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LexerAction {
    NoAction,
    Advance,
    EmitAndAdvance,
    EmitAndReset,
    AppendAndAdvance,
    AppendAndReset,
    Stop,
}

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("")]
    NoWordIndex,
}

pub struct Lexer<IC, LS, T>
where
    IC: InputClass,
    LS: StateTransitionTable<IC>,
    T: Token,
{
    initial_state: LS,
    phantom: PhantomData<(IC, T)>,
}

impl<IC, LS, T> Lexer<IC, LS, T>
where
    IC: InputClass,
    LS: StateTransitionTable<IC>,
    T: Token,
{
    pub fn new(initial_state: LS) -> Self {
        Self {
            initial_state,
            phantom: PhantomData,
        }
    }

    pub fn lex(&self, input: &str) -> Result<Vec<T>, LexerError> {
        use LexerAction::*;
        use LexerError::*;

        let input: Vec<char> = input.chars().collect();

        let mut current_index = 0;
        let mut word_index = Some(0);
        let mut current_state = self.initial_state;

        let mut output = Vec::new();
        while current_index <= input.len() {
            let class = if current_index < input.len() {
                Some(IC::classify(input[current_index]))
            } else {
                None
            };
            let (next_state, action) = current_state.transition(class);

            if action == Stop {
                break;
            }

            // Emit words
            match action {
                EmitAndAdvance | EmitAndReset | AppendAndAdvance | AppendAndReset => {
                    let word_index = word_index.ok_or(NoWordIndex)?;
                    let text: String = input[word_index..current_index].iter().collect();
                    if action == AppendAndAdvance || action == AppendAndReset {
                        if let Some(t) = T::append(text, output.last_mut()) {
                            output.push(t);
                        }
                    } else {
                        output.push(T::emit(text));
                    }
                }
                _ => (),
            }

            // Update word index
            match action {
                Advance | EmitAndAdvance | AppendAndAdvance => word_index = Some(current_index),
                EmitAndReset | AppendAndReset => word_index = None,
                _ => (),
            }

            current_state = next_state;
            current_index += 1;
        }

        Ok(output)
    }
}

#[cfg(test)]
mod hex_test {
    use super::*;

    #[derive(Debug, Copy, Clone)]
    enum HexInputClass {
        Zero,
        X,
        Digit,
        Other,
    }

    impl InputClass for HexInputClass {
        fn classify(c: char) -> Self {
            use HexInputClass::*;
            match c {
                '0' => Zero,
                'x' | 'X' => X,
                'a'..='f' | 'A'..='F' | '1'..='9' => Digit,
                _ => Other,
            }
        }
    }

    #[derive(Debug, Copy, Clone)]
    enum HexLexerState {
        WaitingForZero,
        ExpectingX,
        ExpectingFirstDigit,
        ExpectingNonDigitOrEnd,
    }

    impl StateTransitionTable<HexInputClass> for HexLexerState {
        fn transition(self, class: Option<HexInputClass>) -> (Self, LexerAction) {
            use HexInputClass::*;
            use HexLexerState::*;
            use LexerAction::*;

            match self {
                WaitingForZero => match class {
                    Some(Zero) => (ExpectingX, Advance),
                    _ => (WaitingForZero, NoAction),
                },

                ExpectingX => match class {
                    Some(X) => (ExpectingFirstDigit, NoAction),
                    _ => (WaitingForZero, NoAction),
                },

                ExpectingFirstDigit => match class {
                    Some(Zero | Digit) => (ExpectingNonDigitOrEnd, NoAction),
                    _ => (WaitingForZero, NoAction),
                },

                ExpectingNonDigitOrEnd => match class {
                    Some(Zero | Digit) => (ExpectingNonDigitOrEnd, NoAction),
                    None | Some(X | Other) => (WaitingForZero, EmitAndReset),
                },
            }
        }
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
    struct HexToken(usize);

    impl Token for HexToken {
        fn emit(s: String) -> Self {
            HexToken(usize::from_str_radix(s.trim_start_matches("0x"), 16).unwrap())
        }

        fn append(_s: String, _last: Option<&mut Self>) -> Option<Self> {
            unreachable!()
        }
    }

    fn get_lexer() -> Lexer<HexInputClass, HexLexerState, HexToken> {
        Lexer::new(HexLexerState::WaitingForZero)
    }

    #[test]
    fn single_bare() {
        let lexer = get_lexer();
        let input = "0x1234";
        let mut numbers = lexer.lex(input).unwrap().into_iter().fuse();
        assert_eq!(numbers.next(), Some(HexToken(4660)));
        assert_eq!(numbers.next(), None);
    }

    #[test]
    fn single_embedded() {
        let lexer = get_lexer();
        let input = "qqqqq0x1234qqqqq";
        let mut numbers = lexer.lex(input).unwrap().into_iter().fuse();
        assert_eq!(numbers.next(), Some(HexToken(4660)));
        assert_eq!(numbers.next(), None);
    }

    #[test]
    fn multiple_embedded() {
        let lexer = get_lexer();
        let input = "qqq0x30x30x40x0xxxx";
        let mut numbers = lexer.lex(input).unwrap().into_iter().fuse();
        assert_eq!(numbers.next(), Some(HexToken(48)));
        assert_eq!(numbers.next(), Some(HexToken(64)));
        assert_eq!(numbers.next(), None);
    }
}
