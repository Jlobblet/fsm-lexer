#![deny(invalid_doc_attributes, missing_docs, unused_doc_comments)]
#![forbid(unsafe_code)]
#![deny(missing_copy_implementations, missing_debug_implementations)]
//! # `fsm-lexer`
//!
//! A finite state (Mealy) machine lexer.
//!
//! # How to Use
//!
//! To create a lexer using this crate, you should:
//!
//! 1. Implement the trait [`InputClass`] for a data type of your choosing.
//!    Typically this would be an `enum` of the different states you wish to
//!    process, but it could be anything.
//!
//!    As part of implementing this trait, implement the [`InputClass::classify`]
//!    function, which takes a single [`char`] and maps it to a class.
//!
//! 2. Implement the trait [`StateTransitionTable`] on a data type describing
//!    the current machine state. Again, this would typically be an `enum` of
//!    the possible states, but it could be anything.
//!
//!    The [`StateTransitionTable::transition`] function describes how a given
//!    current state and input class pair should map to a new state and what
//!    action the lexer should take at this step.
//!
//! 3. Implement the trait [`Token`] for a data type containing the potential
//!    output types of your lexer. As with the others, it would typically be an
//!    `enum` but could be anything. The output of [`Lexer::lex`] on success
//!    will contain a [`Vec<Token>`] for a given [`Token`] implementor.
//!
//!    The [`Token::emit`] function creates a new token from the given [`String`]
//!    and state of the lexer, whereas [`Token::append`] should attempt to
//!    combine this token with the prior one, if applicable.

use std::fmt::Debug;
use std::marker::PhantomData;
use thiserror::Error;

/// A trait for taking input and classifying it in a user-defined way.
///
/// # Examples
///
/// ```rust
/// # use crate::fsm_lexer::InputClass;
/// # use std::fmt::Debug;
/// #[derive(Debug, Copy, Clone)]
/// enum DigitClass {
///     Digit,
///     Other,
/// }
///
/// impl InputClass for DigitClass {
///     fn classify(c: char) -> Self {
///         use DigitClass::*;
///         match c {
///             '0'..='9' => Digit,
///             _ => Other,
///         }
///     }
/// }
/// ```
pub trait InputClass: Debug + Copy + Sized {
    /// Classify a character.
    ///
    /// The output of this function is used alongside the current lexer state in
    /// the [`StateTransitionTable`] to determine what [`LexerAction`] to take,
    /// and what the next state will be.
    fn classify(c: char) -> Self;
}

/// A trait for generating tokens from a [`String`].
/// The two generation functions (`emit` and `append`) handle two different
/// cases.
pub trait Token<LS: Debug + Copy>: Sized {
    /// Create a new token from a [`String`] and the current state.
    fn emit(s: String, state: LS) -> Self;
    /// Update the previous token (`last`) if applicable and return `None`.
    /// If not, create a new token.
    ///
    /// While it is not expected that `last` will be modified and a new token
    /// will be returned in the same call, this use case is permitted.
    fn append(s: String, state: LS, last: Option<&mut Self>) -> Option<Self>;
}

/// A trait to be implemented by the lexer state describing how the state should
/// transition based on the current state and the current input class, and what
/// action the lexer should take as a result.
pub trait StateTransitionTable<IC: InputClass>: Debug + Copy {
    /// Given the current state (`self`) and an input class (if applicable),
    /// return the new lexer state and a [`LexerAction`] to be taken.
    ///
    /// At the end of the input string, `class` is `None`.
    fn transition(self, class: Option<IC>) -> (Self, LexerAction);
}

/// An enum containing actions that the lexer can take after parsing a character.
// Users do not have to use all of the possible lexer actions.
#[allow(unused, dead_code)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum LexerAction {
    /// Perform no action.
    NoAction,
    /// Update the word index to the current index.
    Advance,
    /// Emit a word, and then update the word index to the current index.
    EmitAndAdvance,
    /// Emit a word, and then reset the word index to `None`.
    EmitAndReset,
    /// Append to the last word if applicable, and then update the word index to
    /// the current index.
    AppendAndAdvance,
    /// Append to the last word if applicable, and then reset the word index to
    /// `None`.
    AppendAndReset,
    /// Stop the lexer.
    Stop,
}

/// A type that enumerates possible errors caused while lexing.
#[derive(Debug, Error, Copy, Clone)]
pub enum LexerError {
    /// There was no word index set when attempting to emit a word.
    #[error("There was no word index set when attempting to emit a word")]
    NoWordIndex,
}

/// Store initial state for a lexer so that it can be reused easily.
#[derive(Debug)]
pub struct Lexer<IC: InputClass, LS: StateTransitionTable<IC>, T: Token<LS>> {
    /// The starting state of the lexer.
    initial_state: LS,
    /// The starting word index of the lexer.
    ///
    /// Recommended to be either `Some(0)` or `None`.
    initial_word_index: Option<usize>,
    phantom: PhantomData<(IC, T)>,
}

impl<IC: InputClass, LS: StateTransitionTable<IC>, T: Token<LS>> Lexer<IC, LS, T> {
    /// Create a new lexer with the specified initial state.
    /// The initial word index will be `Some(0)`.
    pub fn new(initial_state: LS) -> Self {
        Self {
            initial_state,
            initial_word_index: Some(0),
            phantom: PhantomData,
        }
    }

    /// Create a new lexer with the specified initial state and word index.
    pub fn with_initial_word_index(initial_state: LS, initial_word_index: Option<usize>) -> Self {
        Self {
            initial_state,
            initial_word_index,
            phantom: PhantomData,
        }
    }

    /// Process a [`&str`] according to the input classifier `IC`, state
    /// transition table `LS`, and the tokeniser `T`.
    pub fn lex(&self, input: &str) -> Result<Vec<T>, LexerError> {
        use LexerAction::*;
        use LexerError::*;

        let input: Vec<char> = input.chars().collect();

        let mut current_index = 0;
        let mut word_index = self.initial_word_index;
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
                        if let Some(t) = T::append(text, current_state, output.last_mut()) {
                            output.push(t);
                        }
                    } else {
                        output.push(T::emit(text, current_state));
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

    impl Token<HexLexerState> for HexToken {
        fn emit(s: String, _state: HexLexerState) -> Self {
            HexToken(usize::from_str_radix(s.trim_start_matches("0x"), 16).unwrap())
        }

        fn append(_s: String, _state: HexLexerState, _last: Option<&mut Self>) -> Option<Self> {
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
