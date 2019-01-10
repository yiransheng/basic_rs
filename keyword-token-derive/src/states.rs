use multimap::MultiMap;
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use sequence_trie::SequenceTrie;
use syn::DeriveInput;

#[derive(Debug, Clone)]
enum State<T> {
    Final(T),
    Intermediate(usize),
}

#[derive(Debug, Clone)]
pub struct Transition<T> {
    from: usize,
    to: State<T>,
    character: char,
}

pub fn impl_keyword_token<'a>(item: &'a DeriveInput) -> impl quote::ToTokens {
    let trie = make_trie(enum_variants(item));
    let trans = transitions(&trie);

    let name = &item.ident;

    let states = trans.iter_all().map(|(from_id, trans)| {
        let (closure_ident, _) = transition_function(*from_id, name, trans);

        quote::quote!(Err(Some(#from_id)) => #closure_ident(c))
    });

    let closures = trans.iter_all().map(|(from_id, trans)| {
        let (_, code) = transition_function(*from_id, name, trans);

        code
    });

    quote::quote! {
        impl KeywordToken for #name {
            fn parse_from_str(s: &str) -> Option<(usize, Self)> {
                let mut state: Result<Self, Option<usize>> = Err(Some(0));
                let mut chars = s.chars();

                #(#closures)*

                let mut consumed: usize = 0;

                loop {
                    if let Ok(x) = state {
                        return Some((consumed, x));
                    }

                    let c = chars.next()?;
                    consumed += 1;

                    state = match state {
                        Err(None) => return None,
                        #( #states , )*
                        _ => unreachable!(),
                    }
                }
            }
        }
    }
}

fn enum_variants<'a>(
    item: &'a DeriveInput,
) -> impl Iterator<Item = &'a Ident> + 'a {
    match &item.data {
        syn::Data::Enum(x) => x.variants.iter().map(|v| &v.ident),
        _ => panic!("Not an enum"),
    }
}

fn make_trie<'a, I: IntoIterator<Item = &'a Ident>>(
    variants: I,
) -> SequenceTrie<char, State<Ident>> {
    let mut trie = SequenceTrie::new();
    trie.insert(&[], State::Intermediate(0));

    let mut id: usize = 1;

    for variant in variants {
        let variant = &variant.to_string();

        for chars in prefixes(variant) {
            trie.insert_owned(chars, State::Intermediate(id));
            id += 1;
        }
        trie.insert_owned(
            variant.chars(),
            State::Final(Ident::new(&variant, Span::call_site())),
        );
    }

    trie
}

fn prefixes<'a>(
    s: &'a str,
) -> impl Iterator<Item = impl Iterator<Item = char> + 'a> + 'a {
    let n_char = s.chars().count();
    let k = n_char.saturating_sub(1);

    (1..=k).map(move |i| s.chars().take(i))
}

fn transitions(
    trie: &SequenceTrie<char, State<Ident>>,
) -> MultiMap<usize, Transition<Ident>> {
    let mut trans = vec![];
    let mut map = MultiMap::new();

    _transitions(trie, &mut trans);

    for t in trans.drain(..) {
        map.insert(t.from, t);
    }

    map
}

fn _transitions(
    trie: &SequenceTrie<char, State<Ident>>,
    results: &mut Vec<Transition<Ident>>,
) {
    if let Some(State::Intermediate(n)) = trie.value() {
        let from = *n;
        for (c, trie) in trie.children_with_keys() {
            let to = match trie.value() {
                Some(s) => s.clone(),
                None => unreachable!(),
            };
            let t = Transition {
                from,
                to,
                character: *c,
            };
            results.push(t);
            _transitions(trie, results);
        }
    }
}

impl Transition<Ident> {
    pub fn as_match_arms(&self, ty: &Ident) -> impl quote::ToTokens {
        let c_lower = &self.character.to_ascii_lowercase();
        let c_upper = &self.character.to_ascii_uppercase();

        let to_id = match &self.to {
            State::Final(s) => {
                let s = s.clone();
                quote::quote!(Ok(#ty::#s))
            }
            State::Intermediate(i) => quote::quote!(Err(Some(#i))),
        };

        quote::quote! {
            #c_lower => #to_id,
            #c_upper => #to_id,
        }
    }
}

fn transition_function(
    from_id: usize,
    ty: &Ident,
    trans: &Vec<Transition<Ident>>,
) -> (Ident, impl quote::ToTokens) {
    let arms = trans.iter().map(|t| t.as_match_arms(ty));
    let closure_ident =
        Ident::new(&format!("_state_{}", from_id), Span::call_site());
    (
        closure_ident.clone(),
        quote::quote! {
            let #closure_ident = |c: char| -> Result<#ty, Option<usize>> {
                match c {
                    #( #arms )*
                    a if a.is_whitespace() && a != '\n' => Err(Some(#from_id)),
                    _ => Err(None),
                }
            };
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keys() {
        let t = Transition {
            from: 0,
            to: Err(2),
            character: 'd',
        };
        println!("{}", t.as_closure("KW"));

        assert!(false);
    }

}
