use alloc::vec::Vec;
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::parse::{Parse, ParseStream, Result};
use syn::{Ident, braced, Token};

#[derive(Debug, Clone, PartialEq)]
pub struct Guard {
    pub name: Ident,
    pub ty: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Guards(pub Vec<Guard>);

impl Guards {
    pub fn names(&self) -> Vec<Ident> {
        let mut names: Vec<Ident> = Vec::new();
        let Guards(guards) = &self;

        for g in guards {
            names.push(g.name.clone());
        }

        names
    }
}

impl Parse for Guards {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let mut resources: Vec<Guard> = Vec::new();

        let _block_name: Ident = input.parse()?;

        // TODO if not error

        // `GuardResources { ... }`
        //                   ^^^
        let block_resources;
        braced!(block_resources in input);

        while !block_resources.is_empty() {
            let resource;
            braced!(resource in block_resources);

            let name: Ident = resource.parse()?;
            let _: Token![:] = resource.parse()?;
            let ty: Ident = resource.parse()?;

            resources.push(Guard { name: name.clone(), ty: ty.clone() })
        }

        Ok(Guards(resources))
    }
}

impl ToTokens for Guards {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for resource in &self.0 {
            let name = resource.name.clone();
            let ty = resource.ty.clone();

            tokens.extend(quote!{
                #name: crate::#ty,
            });
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Action {
    pub name: Ident,
    pub ty: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Actions(pub Vec<Action>);

impl Actions {
    pub fn names(&self) -> Vec<Ident> {
        let mut names: Vec<Ident> = Vec::new();
        let Actions(actions) = &self;

        for a in actions {
            names.push(a.name.clone());
        }

        names
    }
}

impl Parse for Actions {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let mut resources: Vec<Action> = Vec::new();
        let _block_name: Ident = input.parse()?;

        // TODO if not, error

        // `ActionResources { ... }`
        //                    ^^^
        let block_resources;
        braced!(block_resources in input);

        while !block_resources.is_empty() {
            let resource;
            braced!(resource in block_resources);

            let name: Ident = resource.parse()?;
            let _: Token![:] = resource.parse()?;
            let ty: Ident = resource.parse()?;

            resources.push(Action { name: name.clone(), ty: ty.clone() });
        }

        Ok(Actions(resources))
    }
}

impl ToTokens for Actions {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for action in &self.0 {
            let name = action.name.clone();
            let ty = action.ty.clone();

            tokens.extend(quote!{
                #name: crate::#ty,
            });
        }
    }
}
