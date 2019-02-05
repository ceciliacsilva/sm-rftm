//! This is an adaptation of *SM* crate (https://docs.rs/sm/0.7.0/sm/) for embedded
//! application using *RTFM* (https://github.com/japaric/cortex-m-rtfm). The main
//! concepts are similar to the original crate, _states_, connected via _transitions, triggered
//! by _events_, but these transitions are "guarded" by a Boolean expression.
//!
//! The state machine is checked at compile-time, using Rust's type systems and ownership
//! model, no run-time checks occurs when using the library.
//!
//! To acomodate guards functions the new syntax has undergone some modifications, which can
//! be seen in the crate examples.
//!
//! Like the original, the library exposes the `sm!` macro, which allows declarative build of
//! state machines.

#![no_std]
#![forbid(
    future_incompatible,
    macro_use_extern_crate,
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    nonstandard_style,
    rust_2018_compatibility,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    unsafe_code,
    variant_size_differences,
)]
#![warn(
    non_snake_case,
    rust_2018_idioms,
    unused_import_braces,
    unused_lifetimes,
    unused_qualifications,
    unused_results,
    unused,
)]
#![feature(tool_lints)]
#![deny(clippy::all)]

use core::fmt;

#[cfg(feature = "macro")]
extern crate sm_macro;
#[cfg(feature = "macro")]
pub use sm_macro::sm;

pub trait State: fmt::Debug + Eq + Clone {}

pub trait InitialState: State {}

pub trait Event: fmt::Debug + Eq + Clone {}

pub trait Machine: fmt::Debug + Eq {
    type State: State;
    type Event: Event;

    fn state(&self) -> Self::State;

    fn trigger(&self) -> Option<Self::Event>;
}

pub trait Initializer<S: InitialState> {
    type Machine: Machine<State = S, Event = NoneEvent>;

    fn new(state: S) -> Self::Machine;
}

/// In the original crate, a `transition` to a new state implies the `consumption` of the old one.
/// transition(self, event: E) -> Self::Machine
/// But as the RTFM's resources must live for the entire duration of the program, the `transitions`
/// receive only a reference of the previous state.
pub trait Transition<E: Event>: fmt::Debug {
    type Machine: Machine;

    ///old self, now &self.
    fn transition(&self, event: E) -> Self::Machine;
}

pub trait AsEnum: fmt::Debug {
    type Enum;

    fn as_enum(self) -> Self::Enum;
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NoneEvent;
impl Event for NoneEvent {}
