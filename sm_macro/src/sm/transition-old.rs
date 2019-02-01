use alloc::vec::Vec;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Result};
use syn::token::Comma;
use syn::{braced, Token};

use crate::sm::event::Event;
use crate::sm::state::State;

#[derive(Debug, PartialEq)]
pub(crate) struct Transitions(pub Vec<Transition>);

impl Parse for Transitions {
    /// example transitions tokens:
    ///
    /// ```text
    /// Push { ... }
    /// Coin { ... }
    /// ```
    ///
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let mut transitions: Vec<Transition> = Vec::new();
        while !input.is_empty() {
            // `Coin { Locked, Unlocked => Unlocked }`
            //  ^^^^
            let event = Event::parse(input)?;

            // `Coin { Locked, Unlocked => Unlocked }`
            //         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            let block_transition;
            braced!(block_transition in input);

            while !block_transition.is_empty() {
                let mut from_states: Vec<State> = Vec::new();

                // `Coin { Locked, Unlocked => Unlocked }`
                //                          ^^
                while !block_transition.peek(Token![=>]) {
                    // `Coin { Locked, Unlocked => Unlocked }`
                    //               ^
                    if block_transition.peek(Token![,]) {
                        let _: Comma = block_transition.parse()?;
                        continue;
                    }

                    // `Coin { Locked, Unlocked => Unlocked }`
                    //         ^^^^^^  ^^^^^^^^
                    from_states.push(State::parse(&block_transition)?);
                }

                // `Coin { Locked, Unlocked => Unlocked }`
                //                          ^^
                let _: Token![=>] = block_transition.parse()?;

                // `Coin { Locked, Unlocked => Unlocked }`
                //                             ^^^^^^^^
                let to = State::parse(&block_transition)?;

                for from in from_states {
                    let event = event.clone();
                    let to = to.clone();

                    transitions.push(Transition { event, from, to })
                }
            }
        }

        Ok(Transitions(transitions))
    }
}

impl ToTokens for Transitions {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for transition in &self.0 {
            transition.to_tokens(tokens);
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Transition {
    pub event: Event,
    pub from: State,
    pub to: State,
}

impl ToTokens for Transition {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let event = &self.event.name;
        let from = &self.from.name;
        let to = &self.to.name;

        tokens.extend(quote! {
            impl<E: Event> Transition<#event> for Machine<#from, E> {
                type Machine = Machine<#to, #event>;

                fn transition(self, event: #event) -> Self::Machine {
                    Machine(#to, Some(event))
                }
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::{format, vec};
    use proc_macro2::TokenStream;
    use syn;
    use syn::parse_quote;

    #[test]
    fn test_transition_to_tokens() {
        let transition = Transition {
            event: Event {
                name: parse_quote! { Push },
            },
            from: State {
                name: parse_quote! { Locked },
            },
            to: State {
                name: parse_quote! { Unlocked },
            },
        };

        let left = quote! {
            impl<E: Event> Transition<Push> for Machine<Locked, E> {
                type Machine = Machine<Unlocked, Push>;

                fn transition(self, event: Push) -> Self::Machine {
                    Machine(Unlocked, Some(event))
                }
            }
        };

        let mut right = TokenStream::new();
        transition.to_tokens(&mut right);

        assert_eq!(format!("{}", left), format!("{}", right))
    }

    #[test]
    fn test_transitions_parse() {
        let left: Transitions = syn::parse2(quote! {
            Push { Unlocked => Locked }
            Coin { Locked => Unlocked }
        }).unwrap();

        let right = Transitions(vec![
            Transition {
                event: Event {
                    name: parse_quote! { Push },
                },
                from: State {
                    name: parse_quote! { Unlocked },
                },
                to: State {
                    name: parse_quote! { Locked },
                },
            },
            Transition {
                event: Event {
                    name: parse_quote! { Coin },
                },
                from: State {
                    name: parse_quote! { Locked },
                },
                to: State {
                    name: parse_quote! { Unlocked },
                },
            },
        ]);

        assert_eq!(left, right);
    }

    #[test]
    fn test_transitions_to_tokens() {
        let transitions = Transitions(vec![
            Transition {
                event: Event {
                    name: parse_quote! { Push },
                },
                from: State {
                    name: parse_quote! { Unlocked },
                },
                to: State {
                    name: parse_quote! { Locked },
                },
            },
            Transition {
                event: Event {
                    name: parse_quote! { Coin },
                },
                from: State {
                    name: parse_quote! { Locked },
                },
                to: State {
                    name: parse_quote! { Unlocked },
                },
            },
        ]);

        let left = quote! {
            impl<E: Event> Transition<Push> for Machine<Unlocked, E> {
                type Machine = Machine<Locked, Push>;

                fn transition(self, event: Push) -> Self::Machine {
                    Machine(Locked, Some(event))
                }
            }

            impl<E: Event> Transition<Coin> for Machine<Locked, E> {
                type Machine = Machine<Unlocked, Coin>;

                fn transition(self, event: Coin) -> Self::Machine {
                    Machine(Unlocked, Some(event))
                }
            }
        };

        let mut right = TokenStream::new();
        transitions.to_tokens(&mut right);

        assert_eq!(format!("{}", left), format!("{}", right))
    }
}
