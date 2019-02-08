use alloc::{format, vec::Vec};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Result};
use syn::{braced, parse_quote, Ident};

use crate::sm::event::{Event, Events};
use crate::sm::initial_state::InitialStates;
use crate::sm::state::{State, States};
use crate::sm::transition::Transitions;
use crate::sm::resources::{Action, Guard, Guards, Actions};

#[derive(Debug, PartialEq)]
pub struct Machines(pub Vec<Machine>);

impl Parse for Machines {
    /// example machines tokens:
    ///
    /// ```text
    /// TurnStile { ... }
    /// Lock { ... }
    /// MyStateMachine { ... }
    /// ```
    ///
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let mut machines: Vec<Machine> = Vec::new();

        while !input.is_empty() {
            // `TurnStile { ... }`
            //  ^^^^^^^^^^^^^^^^^
            let machine = Machine::parse(input)?;
            machines.push(machine);
        }

        Ok(Machines(machines))
    }
}

impl ToTokens for Machines {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(quote! {
            use sm::{AsEnum, Initializer, Machine as M, Transition};
        });

        for machine in &self.0 {
            machine.to_tokens(tokens);
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Machine {
    pub name: Ident,
    pub initial_states: InitialStates,
    pub transitions: Transitions,
    pub guard_resources: Guards,
    pub action_resources: Actions,
}

impl Machine {
    fn states(&self) -> States {
        let mut states: Vec<State> = Vec::new();

        for t in &self.transitions.0 {
            if !states.iter().any(|s| s.name == t.from.name) {
                states.push(t.from.clone());
            }

            if !states.iter().any(|s| s.name == t.to.name) {
                states.push(t.to.clone());
            }
        }

        for i in &self.initial_states.0 {
            if !states.iter().any(|s| s.name == i.name) {
                states.push(State {
                    name: i.name.clone(),
                });
            }
        }

        States(states)
    }

    fn non_terminal_states(&self) -> States {
        let mut states: Vec<State> = Vec::new();

        for t in &self.transitions.0 {
            if !states.iter().any(|s| s.name == t.from.name) {
                states.push(t.from.clone());
            }
        }

        States(states)
    }

    fn terminal_states(&self) -> States {
        let States(states) = self.states();
        let States(non_terminal_states) = self.non_terminal_states();
        let mut terminal_states: Vec<State> = Vec::new();

        for s in states {
            if !non_terminal_states.contains(&s) {
                terminal_states.push(s);
            }
        }

        States(terminal_states)
    }

    fn events(&self) -> Events {
        let mut events: Vec<Event> = Vec::new();

        for t in &self.transitions.0 {
            if !events.iter().any(|s| s.name == t.event.name) {
                events.push(t.event.clone());
            }
        }

        Events(events)
    }
}

// TODO GuardResourses and ActionResources
impl Parse for Machine {
    /// example machine tokens:
    ///
    /// ```text
    /// TurnStile {
    ///     GuardsResources { ... }
    ///     ActionResources { ... }
    ///     InitialStates { ... }
    ///
    ///     Push { ... }
    ///     Coin { ... }
    /// }
    /// ```
    ///
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        // `TurnStile { ... }`
        //  ^^^^^^^^^
        let name: Ident = input.parse()?;

        // `TurnStile { ... }`
        //              ^^^
        let block_machine;
        braced!(block_machine in input);

        // GuardResources { ... }
        // ^^^^^^^^^^^^^^^^^^^^^
        let guard_resources = Guards::parse(&block_machine)?;

        // ActionResources { ... }
        // ^^^^^^^^^^^^^^^^^^^^^
        let action_resources = Actions::parse(&block_machine)?;


        // `InitialStates { ... }`
        //  ^^^^^^^^^^^^^^^^^^^^^
        let initial_states = InitialStates::parse(&block_machine)?;

        // `Push { ... }`
        //  ^^^^^^^^^^^^
        let transitions = Transitions::parse(&block_machine)?;

        Ok(Machine {
            name,
            initial_states,
            transitions,
            guard_resources,
            action_resources,
        })
    }
}

impl ToTokens for Machine {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        let initial_states = &self.initial_states;
        let states = &self.states();
        let events = &self.events();
        let machine_enum = MachineEnum { machine: &self };
        let machine_eval = MachineEval { machine: &self };
        let transitions = &self.transitions;
        let guard_resources = &self.guard_resources;
        let action_resources = &self.action_resources;

        let Events(inside_event) = events;
        let mut use_events = Vec::new();

        for e in inside_event {
            let event_name = e.name.clone();
            use_events.push(quote!(use crate::#name::#event_name;));
        }

        tokens.extend(quote! {
            #[allow(non_snake_case)]
            mod #name {
                use sm::{AsEnum, Event, InitialState, Initializer, Machine as M, NoneEvent, State, Transition};

                #[derive(Debug, Eq, PartialEq)]
                pub struct Machine<S: State, E: Event>(S, Option<E>);

                impl<S: State, E: Event> M for Machine<S, E> {
                    type State = S;
                    type Event = E;

                    fn state(&self) -> Self::State {
                        self.0.clone()
                    }

                    fn trigger(&self) -> Option<Self::Event> {
                        self.1.clone()
                    }
                }

                impl<S: InitialState> Initializer<S> for Machine<S, NoneEvent> {
                    type Machine = Machine<S, NoneEvent>;

                    fn new(state: S) -> Self::Machine {
                        Machine(state, Option::None)
                    }
                }

                #states
                #initial_states
                #events
                #machine_enum
                #transitions

                pub trait ValidEvent {
                    fn is_enabled(#guard_resources) -> bool;
                    fn action(#action_resources);
                }
                pub trait MachineEvaluation {
                    fn eval_machine(&mut self, #guard_resources #action_resources) -> Result<(), ()>;
                }

                fn bool_to_u8(b: bool) -> u8 {
                    if b { 1 } else { 0 }
                }

                #machine_eval
            }
        });
    }
}

#[derive(Debug)]
#[allow(single_use_lifetimes)]
pub struct MachineEval<'a> {
    pub machine: &'a Machine,
}

#[allow(single_use_lifetimes)]
impl<'a> MachineEval<'a> {
    fn filter_transitions_from(&self, state: &State) -> Vec<Event> {
        let mut result: Vec<Event> = Vec::new();
        for t in &self.machine.transitions.0 {
            let name = t.event.name.clone();
            let from = t.from.name.clone();

            if from == state.name.clone() {
                result.push(Event{ name });
            }
        }

        result
    }

    fn filter_variants(&self, state: &State) -> Vec<(Ident, Ident)> {
        let mut variants = Vec::new();

        for s in &self.machine.initial_states.0 {
            let name = s.name.clone();
            if name == state.name.clone() {
                let variant = Ident::new(&format!("Initial{}", name), Span::call_site());
                let none = Ident::new(&format!("NoneEvent"), Span::call_site());
                variants.push((variant, none));
            }
        }

        for t in &self.machine.transitions.0 {
            let to = t.to.name.clone();
            let event = t.event.name.clone();
            let variant = Ident::new(&format!("{}By{}", to, event), Span::call_site());
            // if variants.contains(&(&variant, &event)) {
                // continue;
            // }

            if to == state.name.clone() {
                variants.push((variant, event));
            }
        }

        variants
    }
}

#[allow(single_use_lifetimes)]
impl<'a> ToTokens for MachineEval<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let _name = &self.machine.name;
        let guard_resources = &self.machine.guard_resources;
        let action_resources = &self.machine.action_resources;

        let mut m_variants = Vec::new();
        let States(non_terminal_states) = &self.machine.non_terminal_states();
        let States(terminal_states) = &self.machine.terminal_states();

        for s in terminal_states {
            let variants_none = &self.filter_variants(s);
            let name_state = &s.name;

            for (v, e) in variants_none {
                m_variants.push(quote!(
                    Variant::#v(m) => { Ok(Machine(#name_state, Some(#e)).as_enum()) },
                ));
            }
        }

        // non terminal states
        for s in non_terminal_states {
            let variants_events = &self.filter_variants(s);
            let name_state = &s.name;

            for (v, e) in variants_events {
                let mut m_guards = Vec::new();
                let transitions = &self.filter_transitions_from(s);
                let mut determinism_condition = Vec::new();

                for (i, t) in transitions.iter().enumerate() {
                    let name = t.name.clone();
                    let names_vars_guard = guard_resources.names();
                    let names_vars_action = action_resources.names();
                    let result = quote!(#name::is_enabled(#(#names_vars_guard),*));

                    if i == 0 {
                        determinism_condition.push(quote!(
                            bool_to_u8(#result)
                        ));
                    } else {
                        determinism_condition.push(quote!(
                           + bool_to_u8(#result)
                        ));
                    }

                    if i == 0 {
                        m_guards.push(quote!(
                            if #result {
                                #name::action(#(#names_vars_action),*);
                                Ok(m.transition(#name).as_enum())
                            }
                        ));
                    } else {
                        m_guards.push(quote!(
                            else if #result {
                                #name::action(#(#names_vars_action),*);
                                Ok(m.transition(#name).as_enum())
                            }
                        ));
                    }
                }

                m_guards.push(quote!(
                    else {
                        Ok(Machine(#name_state, Some(#e)).as_enum())
                    }
                ));

                m_variants.push(quote!(
                    Variant::#v(m) => {
                        let cnt = #(#determinism_condition)*;

                        if cnt < 2 {
                            #(#m_guards)*
                        } else {
                            Err(Machine(#name_state, Some(#e)).as_enum())
                        }
                    },
                ));
            }
        }

        tokens.extend(quote!{
            impl MachineEvaluation for Variant {
                fn eval_machine(&mut self, #guard_resources #action_resources) -> Result<(), ()> {
                    let result_sm =
                        match self {
                            #(#m_variants)*
                        };

                    match result_sm {
                        Ok(new_sm) => {
                            *self = new_sm;
                            Ok(())
                        },
                        Err(same_sm) => {
                            *self = same_sm;
                            Err(())
                        }
                    }
                }
            }
        });

    }
}

#[derive(Debug)]
#[allow(single_use_lifetimes)]
struct MachineEnum<'a> {
    machine: &'a Machine,
}

#[allow(single_use_lifetimes)]
impl<'a> ToTokens for MachineEnum<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut variants = Vec::new();
        let mut states = Vec::new();
        let mut events = Vec::new();

        for s in &self.machine.initial_states.0 {
            let name = s.name.clone();
            let none = parse_quote! { NoneEvent };
            let variant = Ident::new(&format!("Initial{}", name), Span::call_site());

            variants.push(variant);
            states.push(name);
            events.push(none);
        }

        for t in &self.machine.transitions.0 {
            let state = t.to.name.clone();
            let event = t.event.name.clone();
            let variant = Ident::new(&format!("{}By{}", state, event), Span::call_site());

            if variants.contains(&variant) {
                continue;
            }

            variants.push(variant);
            states.push(state);
            events.push(event);
        }

        let variants = &variants;
        let states = &states;
        let events = &events;

        tokens.extend(quote!{
            #[derive(Debug)]
            pub enum Variant {
                #(#variants(Machine<#states, #events>)),*
            }

            #(
                impl AsEnum for Machine<#states, #events> {
                    type Enum = Variant;

                    fn as_enum(self) -> Self::Enum {
                        Variant::#variants(self)
                    }
                }
            )*
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::{format, vec};
    use crate::sm::initial_state::InitialState;
    use crate::sm::transition::Transition;
    use proc_macro2::TokenStream;
    use syn;
    use syn::parse_quote;

    #[test]
    fn test_machine_parse() {
        let left: Machine = syn::parse2(quote! {
           TurnStile {
               GuardResources {
                   {a: u8}
                   {b: u16} }
               ActionResouces {
                   {led: Led}
                   {max: u16} }
               InitialStates { Locked, Unlocked }

               Coin { Locked => Unlocked }
               Push { Unlocked => Locked }
           }
        }).unwrap();

        let right = Machine {
            name: parse_quote! { TurnStile },
            guard_resources: Guards(vec![
                Guard {
                    name: parse_quote! { a },
                    ty: parse_quote! { u8 },
                },
                Guard {
                    name: parse_quote! { b },
                    ty: parse_quote! { u16 },
                }
            ]),
            action_resources: Actions(vec![
                Action {
                    name: parse_quote! { led },
                    ty: parse_quote! { Led },
                },
                Action {
                    name: parse_quote! { max },
                    ty: parse_quote! { u16 },
                }
            ]),
            initial_states: InitialStates(vec![
                InitialState {
                    name: parse_quote! { Locked },
                },
                InitialState {
                    name: parse_quote! { Unlocked },
                },
            ]),
            transitions: Transitions(vec![
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
            ]),
        };

        assert_eq!(left, right);
    }

    #[test]
    fn test_machine_to_tokens() {
        let machine = Machine {
            name: parse_quote! { TurnStile },
            guard_resources: Guards( vec![
                Guard {
                    name: parse_quote! { a },
                    ty: parse_quote! { u8 },
                },
                Guard {
                    name: parse_quote! { b },
                    ty: parse_quote! { u16 },
                }
            ]),
            action_resources: Actions(vec![
                Action {
                    name: parse_quote! { led },
                    ty: parse_quote! { Led },
                },
                Action {
                    name: parse_quote! { max },
                    ty: parse_quote! { u16 },
                }
            ]),
            initial_states: InitialStates(vec![
                InitialState {
                    name: parse_quote! { Unlocked },
                },
                InitialState {
                    name: parse_quote! { Locked },
                },
            ]),
            transitions: Transitions(vec![Transition {
                event: Event {
                    name: parse_quote! { Push },
                },
                from: State {
                    name: parse_quote! { Unlocked },
                },
                to: State {
                    name: parse_quote! { Locked },
                },
            }]),
        };

        let left = quote! {
            #[allow(non_snake_case)]
            mod TurnStile {
                use sm::{AsEnum, Event, InitialState, Initializer, Machine as M, NoneEvent, State, Transition};

                #[derive(Debug, Eq, PartialEq)]
                pub struct Machine<S: State, E: Event>(S, Option<E>);

                impl<S: State, E: Event> M for Machine<S, E> {
                    type State = S;
                    type Event = E;

                    fn state(&self) -> Self::State {
                        self.0.clone()
                    }

                    fn trigger(&self) -> Option<Self::Event> {
                        self.1.clone()
                    }
                }

                impl<S: InitialState> Initializer<S> for Machine<S, NoneEvent> {
                    type Machine = Machine<S, NoneEvent>;

                    fn new(state: S) -> Self::Machine {
                        Machine(state, Option::None)
                    }
                }

                #[derive(Clone, Copy, Debug, Eq)]
                pub struct Unlocked;
                impl State for Unlocked {}

                impl PartialEq<Unlocked> for Unlocked {
                    fn eq(&self, _: & Unlocked) -> bool {
                        true
                    }
                }

                impl PartialEq<Locked> for Unlocked {
                    fn eq(&self, _: & Locked) -> bool {
                        false
                    }
                }

                #[derive(Clone, Copy, Debug, Eq)]
                pub struct Locked;
                impl State for Locked {}

                impl PartialEq<Unlocked> for Locked {
                    fn eq(&self, _: &Unlocked) -> bool {
                        false
                    }
                }

                impl PartialEq<Locked> for Locked {
                    fn eq(&self, _: &Locked) -> bool {
                        true
                    }
                }

                impl InitialState for Unlocked {}
                impl InitialState for Locked {}

                #[derive(Clone, Copy, Debug, Eq)]
                pub struct Push;
                impl Event for Push {}

                impl PartialEq<Push> for Push {
                    fn eq(&self, _: &Push) -> bool {
                        true
                    }
                }

                #[derive(Debug)]
                pub enum Variant {
                    InitialUnlocked(Machine<Unlocked, NoneEvent>),
                    InitialLocked(Machine<Locked, NoneEvent>),
                    LockedByPush(Machine<Locked, Push>)
                }

                impl AsEnum for Machine<Unlocked, NoneEvent> {
                    type Enum = Variant;

                    fn as_enum(self) -> Self::Enum {
                        Variant::InitialUnlocked(self)
                    }
                }

                impl AsEnum for Machine<Locked, NoneEvent> {
                    type Enum = Variant;

                    fn as_enum(self) -> Self::Enum {
                        Variant::InitialLocked(self)
                    }
                }

                impl AsEnum for Machine<Locked, Push> {
                    type Enum = Variant;

                    fn as_enum(self) -> Self::Enum {
                        Variant::LockedByPush(self)
                    }
                }

                impl<E: Event> Transition<Push> for Machine<Unlocked, E> {
                    type Machine = Machine<Locked, Push>;

                    fn transition(&self, event: Push) -> Self::Machine {
                        Machine(Locked, Some(event))
                    }
                }

                pub trait ValidEvent {
                    fn is_enabled ( a : crate::u8 , b: crate::u16, ) -> bool ;
                    fn action (led: crate::Led, max: crate::u16 , ) ;
                }

                pub trait MachineEvaluation {
                    fn eval_machine(&mut self, a: crate::u8, b: crate::u16, led: crate::Led, max: crate::u16,) -> Result<(), ()>;
                }
                fn bool_to_u8 (b: bool) -> u8 {
                    if b { 1 } else { 0 }
                }

                impl MachineEvaluation for Variant {
                    fn eval_machine(&mut self, a: crate::u8, b: crate::u16, led: crate::Led, max: crate::u16,) -> Result<(), ()> {
                        let result_sm =
                            match self {
                                Variant::InitialLocked(m) => {
                                    Ok(Machine(Locked, Some(NoneEvent)).as_enum())
                                },
                                Variant::LockedByPush(m) => {
                                    Ok(Machine(Locked, Some(Push)).as_enum())
                                },
                                Variant::InitialUnlocked(m) => {
                                    let cnt = bool_to_u8(Push::is_enabled(a, b));

                                    if cnt < 2 {
                                        if Push::is_enabled(a,b) {
                                            Push::action(led, max);
                                            Ok(m.transition(Push).as_enum())
                                        } else {
                                            Ok(Machine(Unlocked, Some(NoneEvent)).as_enum())
                                        }
                                    } else {
                                        Err(Machine(Unlocked, Some(NoneEvent)).as_enum())
                                    }
                                },
                            };

                        match result_sm {
                            Ok(new_sm) => {
                                *self = new_sm;
                                Ok(())
                            },
                            Err(same_sm) => {
                                *self = same_sm;
                                Err(())
                            }
                        }
                    }
                }
            }
        };

        let mut right = TokenStream::new();
        machine.to_tokens(&mut right);

        assert_eq!(format!("{}", left), format!("{}", right))
    }
}
