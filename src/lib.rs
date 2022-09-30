#![deny(invalid_doc_attributes, missing_docs, unused_doc_comments)]
#![forbid(unsafe_code)]
#![deny(missing_copy_implementations, missing_debug_implementations)]
#![feature(test)]
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
//!
//! # Motivations
//!
//! This crate uses traits to provide functions to the lexer. This is so that
//! the functions can be resolved at compile time via monomorphisation rather
//! than being passed as boxed trait objects (for the [`Fn`] trait) at runtime.
//!
//! An alternate implementation could use the same principles but via boxed
//! functions. I preferred the trait approach, which is why I used it.

use std::fmt::Debug;
use std::marker::PhantomData;
use std::str::CharIndices;
use thiserror::Error;

/// A trait for taking input and classifying it in a user-defined way.
///
/// # Examples
///
/// ```rust
/// # use crate::fsm_lexer::InputClassifier;
/// # use std::fmt::Debug;
/// #[derive(Debug, Copy, Clone)]
/// enum DigitClass {
///     Digit,
///     Other,
/// }
///
/// impl InputClassifier for DigitClass {
///     type InputClass = Self;
///
///     fn classify(c: char) -> Self::InputClass {
///         use DigitClass::*;
///         match c {
///             '0'..='9' => Digit,
///             _ => Other,
///         }
///     }
/// }
/// ```
pub trait InputClassifier: Debug {
    /// The type produced by the input classifier.
    /// In general, this is often `Self` or `Option<Self>`.
    type InputClass: Debug + Copy + Sized;

    /// Classify a character.
    ///
    /// The output of this function is used alongside the current lexer state in
    /// the [`StateTransitionTable`] to determine what [`LexerAction`] to take,
    /// and what the next state will be.
    fn classify(c: char) -> Self::InputClass;
}

/// A trait for generating tokens from a [`String`].
/// The two generation functions (`emit` and `append`) handle two different
/// cases.
pub trait Tokeniser<LS: Debug>: Sized {
    /// The token type produced by the tokeniser.
    /// This may be `Self`, or some other type such as `String` or `usize`.
    type Token: Debug + Clone;

    /// Create a new token from a [`String`] and the current state.
    fn emit(s: &str, state: LS) -> Self::Token;
    /// Update the previous token (`last`) if applicable and return `None`.
    /// If not, create a new token.
    ///
    /// While it is not expected that `last` will be modified and a new token
    /// will be returned in the same call, this use case is permitted.
    fn append(s: &str, state: LS, last: Option<&mut Self::Token>) -> Option<Self::Token>;
}

/// A trait to be implemented by the lexer state describing how the state should
/// transition based on the current state and the current input class, and what
/// action the lexer should take as a result.
pub trait StateTransitionTable<IC: Debug>: Debug {
    /// The type representing the current lexer state, usually an `enum`.
    type LexerState: Debug + Copy;

    /// Given the current state (`self`) and an input class (if applicable),
    /// return the new lexer state and a [`LexerAction`] to be taken.
    ///
    /// At the end of the input string, `class` is `None`.
    fn transition(state: Self::LexerState, class: Option<IC>) -> (Self::LexerState, LexerAction);
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
pub struct Lexer<IC, STT, T>
where
    IC: InputClassifier,
    STT: StateTransitionTable<IC::InputClass>,
    T: Tokeniser<STT::LexerState>,
{
    /// The starting state of the lexer.
    initial_state: STT::LexerState,
    /// The starting word index of the lexer.
    ///
    /// Recommended to be either `Some(0)` or `None`.
    initial_word_index: Option<usize>,
    phantom: PhantomData<(IC, T)>,
}

impl<IC, STT, T> Lexer<IC, STT, T>
where
    IC: InputClassifier,
    STT: StateTransitionTable<IC::InputClass>,
    T: Tokeniser<STT::LexerState>,
{
    /// Create a new lexer with the specified initial state.
    /// The initial word index will be `Some(0)`.
    pub fn new(initial_state: STT::LexerState) -> Self {
        Self {
            initial_state,
            initial_word_index: Some(0),
            phantom: PhantomData,
        }
    }

    /// Create a new lexer with the specified initial state and word index.
    pub fn with_initial_word_index(
        initial_state: STT::LexerState,
        initial_word_index: Option<usize>,
    ) -> Self {
        Self {
            initial_state,
            initial_word_index,
            phantom: PhantomData,
        }
    }

    /// Process a [`&str`] according to the input classifier `IC`, state
    /// transition table `STT`, and the tokeniser `T`.
    pub fn lex(&self, input: &str) -> Result<Vec<T::Token>, LexerError> {
        use LexerAction::*;
        use LexerError::*;

        let mut cis = input.char_indices();

        let mut word_index = self.initial_word_index;
        let mut current_state = self.initial_state;

        let mut output = Vec::new();

        loop {
            let (current_index, class) = match cis.next() {
                Some((i, c)) => (i, Some(IC::classify(c))),
                None => (input.len(), None),
            };
            let (next_state, action) = STT::transition(current_state, class);

            if action == Stop {
                break;
            }

            // Emit words
            match action {
                EmitAndAdvance | EmitAndReset | AppendAndAdvance | AppendAndReset => {
                    let word_index = word_index.ok_or(NoWordIndex)?;
                    let text = &input[word_index..current_index];
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

            // Stop if we have reached the end of the string
            // We check at the end of the loop because need to process end of
            // string for any final actions in the STT.
            if class.is_none() {
                break;
            }

            current_state = next_state;
        }

        Ok(output)
    }
}

#[derive(Debug)]
/// DOCS: TODO
pub struct LexerIterator<'a, IC, STT, T>
where
    IC: InputClassifier,
    STT: StateTransitionTable<IC::InputClass>,
    T: Tokeniser<STT::LexerState>,
{
    input: &'a str,
    source: CharIndices<'a>,
    word_index: Option<usize>,
    current_state: STT::LexerState,
    finished: bool,
    next_word: Option<T::Token>,
    phantom: PhantomData<IC>,
}

impl<'a, IC, STT, T> Iterator for LexerIterator<'a, IC, STT, T>
where
    IC: InputClassifier,
    STT: StateTransitionTable<IC::InputClass>,
    T: Tokeniser<STT::LexerState>,
{
    type Item = T::Token;

    fn next(&mut self) -> Option<Self::Item> {
        use LexerAction::*;
        use LexerError::*;

        let mut ret = self.next_word.take();

        if self.finished {
            return ret;
        }

        loop {
            let (current_index, class) = match self.source.next() {
                None => (self.input.len(), None),
                Some((i, c)) => (i, Some(IC::classify(c))),
            };

            let (next_state, action) = STT::transition(self.current_state, class);

            if action == Stop {
                self.finished = true;
                break;
            }

            // Emit words
            match action {
                EmitAndAdvance | EmitAndReset | AppendAndAdvance | AppendAndReset => {
                    let word_index = self.word_index.expect("No word index when trying to emit a word");
                    let text = &self.input[word_index..current_index];
                    if action == AppendAndAdvance || action == AppendAndReset {
                        // try to append to current return word
                        let ow = T::append(text, self.current_state, ret.as_mut());
                        if ret.is_none() {
                            ret = ow;
                        } else {
                            self.next_word = ow;
                        }
                    } else {
                        let w = Some(T::emit(text, self.current_state));
                        if ret.is_none() {
                            ret = w;
                        } else {
                            self.next_word = w;
                        }
                    }
                }
                _ => (),
            }

            // Update word index
            match action {
                Advance | EmitAndAdvance | AppendAndAdvance => self.word_index = Some(current_index),
                EmitAndReset | AppendAndReset => self.word_index = None,
                _ => (),
            }

            self.current_state = next_state;

            // Stop if we have reached the end of the string
            // We check at the end of the loop because need to process end of
            // string for any final actions in the STT.
            if class.is_none() || self.next_word.is_some() {
                break;
            }
        }

        ret
    }
}

impl<'a, IC, STT, T> std::iter::FusedIterator for LexerIterator<'a, IC, STT, T>
where
    IC: InputClassifier,
    STT: StateTransitionTable<IC::InputClass>,
    T: Tokeniser<STT::LexerState>,
{}

/// todo
pub fn lex<IC, STT, T>(input: &str, initial_state: STT::LexerState) -> LexerIterator<'_, IC, STT, T>
where
    IC: InputClassifier,
    STT: StateTransitionTable<IC::InputClass>,
    T: Tokeniser<STT::LexerState>,
{
    LexerIterator {
        input,
        source: input.char_indices(),
        word_index: Some(0),
        current_state: initial_state,
        finished: false,
        next_word: None,
        phantom: PhantomData
    }
}

#[cfg(test)]
mod hex_test {
    extern crate test;
    use test::Bencher;
    use crate::hex_test::HexLexerState::WaitingForZero;
    use super::*;

    #[derive(Debug, Copy, Clone)]
    enum HexInputClass {
        Zero,
        X,
        Digit,
        Other,
    }

    impl InputClassifier for HexInputClass {
        type InputClass = Self;

        #[inline]
        fn classify(c: char) -> Self::InputClass {
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
        type LexerState = Self;

        fn transition(
            state: Self::LexerState,
            class: Option<HexInputClass>,
        ) -> (Self::LexerState, LexerAction) {
            use HexInputClass::*;
            use HexLexerState::*;
            use LexerAction::*;

            match state {
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

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    struct HexTokeniser;

    impl Tokeniser<HexLexerState> for HexTokeniser {
        type Token = u128;

        fn emit(s: &str, _state: HexLexerState) -> Self::Token {
            match Self::Token::from_str_radix(s.trim_start_matches("0x").trim_start_matches("0X"), 16) {
                Ok(u) => u,
                Err(e) => panic!("Error trying to parse {s} ({}): {e}", s.trim_start_matches("0x")),
            }
        }

        fn append(
            _s: &str,
            _state: HexLexerState,
            _last: Option<&mut Self::Token>,
        ) -> Option<Self::Token> {
            unreachable!()
        }
    }

    fn get_lexer() -> Lexer<HexInputClass, HexLexerState, HexTokeniser> {
        Lexer::new(HexLexerState::WaitingForZero)
    }

    #[test]
    fn single_bare() {
        let lexer = get_lexer();
        let input = "0x1234";
        let mut numbers = lexer.lex(input).unwrap().into_iter().fuse();
        assert_eq!(numbers.next(), Some(4660));
        assert_eq!(numbers.next(), None);
    }

    #[test]
    fn single_embedded() {
        let lexer = get_lexer();
        let input = "qqqqq0x1234qqqqq";
        let mut numbers = lexer.lex(input).unwrap().into_iter().fuse();
        assert_eq!(numbers.next(), Some(4660));
        assert_eq!(numbers.next(), None);
    }

    #[test]
    fn multiple_embedded() {
        let lexer = get_lexer();
        let input = "qqq0x30x30x40x0xxxx";
        let mut numbers = lex::<HexInputClass, HexLexerState, HexTokeniser>(input, WaitingForZero);
        assert_eq!(numbers.next(), Some(48));
        assert_eq!(numbers.next(), Some(64));
        assert_eq!(numbers.next(), None);
    }

    #[bench]
    fn bench_1m(b: &mut Bencher) {
        let lexer = get_lexer();
        let input = include_str!("1m.txt");
        b.iter(|| lex::<HexInputClass, HexLexerState, HexTokeniser>(input, WaitingForZero).for_each(|_| ()));
    }

    #[bench]
    fn bench_10k(b: &mut Bencher) {
        let lexer = get_lexer();
        let input = include_str!("10k.txt");
        b.iter(|| lexer.lex(input).ok());
    }
}
