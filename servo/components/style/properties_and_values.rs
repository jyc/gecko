/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! Support for the [Properties & Values API][spec].
//!
//! [spec]: https://drafts.css-houdini.org/css-properties-values-api-1/

use crate::custom_properties::{self, CssEnvironment, EmptySubstitutionMap, ExtraData, Name, ResolutionMap, substitute_references_in_value, SubstitutionMap, TokenStream, VariableValue};
use crate::media_queries::Device;
use crate::properties::{CSSWideKeyword, CustomDeclaration, CustomDeclarationValue};
use crate::properties::longhands::transform;
use crate::selector_map::PrecomputedHashSet;
use crate::stylesheets::{Origin, PerOrigin};
use crate::Atom;
use cssparser::{BasicParseError, BasicParseErrorKind, Parser, ParserInput, Token};
use parser::{Parse, ParserContext};
use servo_arc::Arc;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Write};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::vec::Vec;
use style_traits::values::{CssWriter, OneOrMoreSeparated, Space, ToCss};
use style_traits::{self, ParsingMode, StyleParseErrorKind};
use values::computed::{self, ToComputedValue};
use values::{self, specified};

/// A registration for a custom property.
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub struct Registration {
    /// The custom property name, sans leading '--'.
    pub name: Name,

    /// The syntax of the custom property.
    pub syntax: Syntax,

    /// Whether the custom property is inherited down the DOM tree.
    pub inherits: bool,

    /// The initial value of the custom property.
    ///
    /// Ideally we'd merge this with `syntax` so that illegal states would be
    /// unrepresentable. But while we could do that by turning the fields of the
    /// SpecifiedValue variants into Option<T>'s, we would need a more
    /// expressive type system to do this with disjunctions.
    pub initial_value: Option<(ComputedValue, TokenStream)>,
}

/// A versioned set of registered custom properties, stored on the document.
/// The [[registeredPropertySet]] of the spec. We keep track of the version to
/// know if we need to recompute which declarations are valid.
#[derive(Default)]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub struct RegisteredPropertySet {
    /// The set of registered custom properties. Names are sans leading '--'.
    registrations: HashMap<Name, Registration>,

    /// The current version. Must be incremented whenever `registrations` is
    /// modified.
    generation: u32,
}

impl RegisteredPropertySet {
    /// Attempt to register a custom property.
    ///
    /// If a custom property has already been registered with that name, return
    /// Err(()), otherwise return Ok(()) and increment the generation.
    pub fn register_property(&mut self, registration: Registration) -> Result<(), ()> {
        match self
            .registrations
            .insert(registration.name.clone(), registration)
        {
            Some(_) => Err(()),
            None => {
                self.generation += 1;
                Ok(())
            }
        }
    }

    /// Attempt to unregister a custom property.
    ///
    /// If no custom property has been registered with that name, return
    /// Err(()), otherwise return Ok(()) and increment the generation.
    pub fn unregister_property(&mut self, name: &Name) -> Result<(), ()> {
        match self.registrations.remove(name) {
            Some(_) => {
                self.generation += 1;
                Ok(())
            }
            None => Err(()),
        }
    }

    /// Return the current generation.
    ///
    /// The generation is incremented every time the set of custom property
    /// registrations changes. It's used by the Stylist to keep track of when it
    /// has to restyle.
    pub fn generation(&self) -> u32 {
        self.generation
    }

    /// Attempt to get the registration for the custom property with the given
    /// name.
    pub fn get(&self, name: &Name) -> Option<&Registration> {
        self.registrations.get(name)
    }

    /// Return the set of all uninherited custom properties.
    ///
    /// Used by style::properties::compute_early_custom_properties to insert
    /// initial values when needed.
    pub fn uninherited_properties(&self) -> HashSet<&Name> {
        self.registrations
            .iter()
            .filter(|&(_, registration)| !registration.inherits)
            .map(|(name, _)| name)
            .collect()
    }
}

/// The result of a call to register_property or unregister_property,
/// corresponding to the errors that can be thrown by CSS.(un)registerProperty.
///
/// Should be kept in sync with mozilla::PropertyRegistrationResult on the Gecko
/// side.
pub enum PropertyRegistrationResult {
    /// Indicates that the call was successful. The caller should return without
    /// error.
    Ok = 0,
    /// Indicates that the call failed, and that the caller should throw a
    /// SyntaxError.
    SyntaxError,
    /// Indicates that the call failed, and that the caller should throw an
    /// InvalidModificationError.
    InvalidModificationError,
    /// Indicates that the call failed, and that the caller should throw a
    /// NotFoundError.
    NotFoundError,
}

/// Attempt to register a custom property.
///
/// This is used by the CSS.registerProperty implementations for Servo and Gecko
/// in order to share logic. The caller should handle the returned
/// PropertyRegistrationResult by throwing the appropriate DOM error.
pub fn register_property(
    device: &Device,
    registered_property_set: &mut RegisteredPropertySet,
    parser_context: &ParserContext,
    name: &str,
    syntax: &str,
    inherits: bool,
    initial_value: Option<&str>,
) -> PropertyRegistrationResult {
    let name = match custom_properties::parse_name(name) {
        Ok(name) => name,
        Err(()) => return PropertyRegistrationResult::SyntaxError,
    };

    let syntax = match Syntax::from_string(syntax) {
        Ok(syntax) => syntax,
        Err(()) => return PropertyRegistrationResult::SyntaxError,
    };

    let initial_value = match initial_value {
        Some(ref specified) => {
            let mut input = ParserInput::new(specified);
            let mut input = Parser::new(&mut input);
            match syntax.parse(parser_context, &mut input) {
                Ok(parsed) => {
                    if parsed.is_computationally_independent() {
                        Some(parsed)
                    } else {
                        return PropertyRegistrationResult::SyntaxError;
                    }
                }
                _ => return PropertyRegistrationResult::SyntaxError,
            }
        }
        None if matches!(syntax, Syntax::Anything) => None,
        // initialValue is required if the syntax is not '*'.
        _ => return PropertyRegistrationResult::SyntaxError,
    };

    let computed_initial_value = initial_value.map(|initial_value| {
        let value =
            computed::Context::for_media_query_evaluation(
                device,
                parser_context.quirks_mode,
                |context| initial_value.to_computed_value(context),
            )
            .expect("register_property: a parsed specified initial value should never fail to compute");
        let css = value.to_token_stream();
        (value, css)
    });

    let result = registered_property_set.register_property(Registration {
        name: name.into(),
        syntax: syntax,
        inherits: inherits,
        initial_value: computed_initial_value,
    });

    match result {
        Ok(_) => PropertyRegistrationResult::Ok,
        Err(_) => PropertyRegistrationResult::InvalidModificationError,
    }
}

/// Attempt to unregister a custom property.
///
/// This is used by the CSS.registerProperty implementations for Servo and Gecko
/// in order to share logic. The caller should handle the returned
/// PropertyRegistraitonResult by throwing the appropriate DOM error.
pub fn unregister_property(
    registered_property_set: &mut RegisteredPropertySet,
    name: &str,
) -> PropertyRegistrationResult {
    let name = match custom_properties::parse_name(name) {
        Ok(name) => name,
        Err(()) => return PropertyRegistrationResult::SyntaxError,
    };

    let result = registered_property_set.unregister_property(&name.into());

    match result {
        Ok(_) => PropertyRegistrationResult::Ok,
        Err(_) => PropertyRegistrationResult::NotFoundError,
    }
}

/// A CSS <custom-ident>.
///
/// We make a newtype for Atom and implement ToCss ourselves because the
/// ToCss implementation for atom in `style_traits::values` uses `cssparsers`'s
/// `serialize_string` function, which writes a double-quoted CSS string. We're
/// only storing <custom-idents>, which should be serialized as specified.
#[derive(Clone, Debug, MallocSizeOf, PartialEq, ToShmem)]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub struct Ident(pub Atom);

trivial_to_computed_value!(Ident);

impl ToCss for Ident {
    #[cfg(feature = "servo")]
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: fmt::Write,
    {
        dest.write_str(&*self.0)
    }

    #[cfg(feature = "gecko")]
    fn to_css<W>(&self, dest: &mut CssWriter<W>) -> fmt::Result
    where
        W: fmt::Write,
    {
        dest.write_str(&self.0.to_string())
    }
}

impl Deref for Ident {
    type Target = Atom;

    fn deref(&self) -> &Atom {
        &self.0
    }
}

/// A basic custom property syntax string for a custom property that, used to
/// build up disjunctions and list terms.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub enum Type {
    /// Syntax to allow any valid <length> value.
    Length,
    /// Syntax to allow any valid <number> value.
    Number,
    /// Syntax to allow any valid <percentage> value.
    Percentage,
    /// Syntax to allow any valid <length> or <percentage> value, or any valid
    /// <calc()> expression combining <length> and <percentage> components.
    LengthPercentage,
    /// Syntax to allow any valid <color> value.
    Color,
    /// Syntax to allow any valid <image> value.
    Image,
    /// Syntax to allow any valid <url> value.
    Url,
    /// Syntax to allow any valid <integer> value.
    Integer,
    /// Syntax to allow any valid <angle> value.
    Angle,
    /// Syntax to allow any valid <time> value.
    Time,
    // FIXME: We should support <resolution> values as well.
    /// Syntax to allow any valid <transform-list> value.
    /// TODO Implement <transform-function>, then implement <transform-list> as
    /// <transform-function>+.
    TransformList,
    /// Syntax to allow any valid <custom-ident> value.
    CustomIdent,
    /// Syntax to allow a specific identifier (matching the <custom-ident>
    /// production, compared codepoint-wise).
    SpecificIdent(Ident),
}

impl Type {
    /// Attempt to parse `input` as this type.
    pub fn parse<'i, 't>(
        &self,
        context: &ParserContext,
        input: &mut Parser<'i, 't>,
    ) -> Result<SpecifiedValueItem, style_traits::ParseError<'i>> {
        macro_rules! parse {
            ($_self:expr, $context:expr, $input:expr,

             $($typ:ident => $fn:path),*) => {
                match $_self {
                    $(
                        Type::$typ => {
                            $fn(context, input).map(|x| {
                                SpecifiedValueItem::$typ(x)
                            })
                        }
                    ), *


                    // We need to actually compare SpecificIdents
                    // codepoint-wise.
                    #[cfg(feature = "servo")]
                    Type::SpecificIdent(ref ident) => {
                        // XXX(jyc) Don't know if this actually compiles.
                        $input.expect_ident_cloned()
                            .and_then(|actual| {
                                // &Atom -> Atom -> str
                                if **actual == &**expected {
                                    Ok(SpecifiedValueItem::SpecificIdent(actual.clone()))
                                } else {
                                    Err(input
                                        .current_source_location()
                                        .new_basic_unexpected_token_error(Token::Ident(actual)))
                                }
                            })
                            .map_err(|e| e.into())
                    },
                    #[cfg(feature = "gecko")]
                    Type::SpecificIdent(ref atom) => {
                        atom.with_str(|expected| {
                            $input.expect_ident_cloned()
                                .and_then(|actual| {
                                    if actual == expected {
                                        Ok(SpecifiedValueItem::SpecificIdent(atom.clone()))
                                    } else {
                                        Err(input
                                            .current_source_location()
                                            .new_basic_unexpected_token_error(Token::Ident(actual)))
                                    }
                                })
                                .map_err(|e| e.into())
                        })
                    },
                }
            };
        }

        fn parse_custom_ident<'i, 't>(
            _context: &ParserContext,
            input: &mut Parser<'i, 't>,
        ) -> Result<Ident, style_traits::ParseError<'i>> {
            input
                .expect_ident_cloned()
                .map(|x| Ident((&*x).into()))
                .map_err(|e| e.into())
        }

        parse! {
            *self, context, input,

            Length => specified::Length::parse,
            Number => specified::Number::parse,
            Percentage => specified::Percentage::parse,
            LengthPercentage => specified::LengthPercentage::parse,
            Color => specified::Color::parse,
            Image => specified::Image::parse,
            Url => specified::url::SpecifiedUrl::parse,
            Integer => specified::Integer::parse,
            Angle => specified::Angle::parse,
            Time => specified::Time::parse,
            TransformList => transform::parse,
            CustomIdent => parse_custom_ident
        }
    }
}

/// A custom property syntax string that is either some basic syntax string
/// (e.g. some <url> value) or some list term. A list term syntax string allows
/// a space-separated list of one or more repetitions of the type specified by
/// the string. Used to build up disjunctions.
/// TODO Support comma-separated lists.
/// https://drafts.css-houdini.org/css-properties-values-api/#multipliers
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub struct Term {
    /// The type of the term (e.g. <integer>).
    pub typ: Type,
    /// Whether or not the term is a list, i.e., if the syntax string was
    /// <integer>+.
    pub list: bool,
}

/// A custom property syntax string.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub enum Syntax {
    /// Syntax to allow any token stream (written '*').
    Anything,
    /// Syntax to allow some disjunction of terms (possibly list terms), which
    /// allows any value matching one of the items in the combination, matched
    /// in specified order (written 'a | b | ...').
    Disjunction(Vec<Term>),
}

fn is_css_wide_keyword(input: &str) -> bool {
    let mut input = ParserInput::new(input);
    let mut input = Parser::new(&mut input);
    CSSWideKeyword::parse(&mut input).is_ok()
}

impl Syntax {
    /// Parse a syntax string given to `CSS.registerProperty`.
    pub fn from_string(input: &str) -> Result<Syntax, ()> {
        // Syntax strings are DOMStrings, but in Servo we assume they are valid
        // UTF-8. See
        // https://doc.servo.org/script/dom/bindings/str/struct.DOMString.html
        // . This justifies iteration by |char|.

        // Can identifiers in syntax strings contain escapes? No.
        //
        // "Currently the answer is no - I've clarified the "literal ident"
        //  part to be specifically a name-start char followed by 0+ name chars.
        //  Would prefer to avoid having to do CSS parsing on the syntax string.
        //  ^_^"
        // https://github.com/w3c/css-houdini-drafts/issues/112
        //
        // A 'specific ident' is any sequence consisting of a name-start code
        // point, followed by zero
        // or more name code points, which matches the <custom-ident>
        // production
        // https://drafts.css-houdini.org/css-properties-values-api-1/#supported-syntax-strings
        //
        // ...
        // This generic data type is denoted by <custom-ident>, and represents
        // any valid CSS identifier that would not be misinterpreted as a
        // pre-defined keyword in that property’s value definition.
        // https://drafts.csswg.org/css-values-4/#identifier-value
        //
        // So! In order to make sure specific identifiers don't contain
        // escapes, we need to check for escapes, which are only introduced by
        // backslashes, which shouldn't show up anyhow.
        // https://drafts.csswg.org/css-syntax-3/#escaping
        let mut contains_backslash = false;
        for c in input.chars() {
            if c == '\\' {
                contains_backslash = true;
                break;
            }
        }
        if contains_backslash {
            return Err(());
        }

        // The parsed syntax string, which we'll build up as we scan tokens.
        let mut syntax = None;

        // The syntax string isn't really CSS, but hopefully this maximizes
        // code reuse.
        let mut parser_input = ParserInput::new(input);
        let mut parser = Parser::new(&mut parser_input);

        #[derive(Debug)]
        enum State {
            // *.
            AfterAsterisk,
            // <.
            AfterOpenAngle,
            // +.
            AfterPlus,
            // <type>.
            // ident.
            AfterType { after_whitespace: bool },
            // <type.
            AfterTypeName,
            // .
            // |.
            Start { after_bar: bool },
        }

        let mut state = State::Start { after_bar: false };

        /// Add a `Type` to the disjunction. It might turn out this is a list
        /// term, in which case we'll modify the `Term` later.
        fn push_type(syntax: &mut Option<Syntax>, t: Type) {
            if let Some(Syntax::Disjunction(ref mut ts)) = *syntax {
                ts.push(Term {
                    typ: t,
                    list: false,
                })
            } else {
                unreachable!()
            }
        }

        /// Signal that we expect to be parsing some term in a disjunction.
        fn expect_disjunction(syntax: &mut Option<Syntax>) {
            if let Some(Syntax::Disjunction(_)) = *syntax {
                // Good!
            } else {
                assert!(*syntax == None);
                *syntax = Some(Syntax::Disjunction(vec![]))
            }
        }

        /// Handle the next token in the syntax string (including whitespace).
        fn handle_token(
            syntax: &mut Option<Syntax>,
            state: State,
            token: &Token,
        ) -> Result<State, ()> {
            debug!("{:?} - {:?}", state, token);
            match (state, token) {
                (_, &Token::Comment(_)) => Err(()),

                (State::Start { .. }, &Token::WhiteSpace(_)) => {
                    // Ignore whitespace.
                    Ok(State::Start { after_bar: false })
                }
                (State::Start { after_bar: false }, &Token::Delim('*')) => {
                    // If we see a '*', that should be it (modulo whitespace).
                    if syntax != &None {
                        Err(())
                    } else {
                        *syntax = Some(Syntax::Anything);
                        Ok(State::AfterAsterisk)
                    }
                }
                (State::Start { .. }, &Token::Delim('<')) => {
                    // A '<' should mean we're in the start of a '<type>'.
                    expect_disjunction(syntax);
                    Ok(State::AfterOpenAngle)
                }
                (State::Start { .. }, &Token::Ident(ref id)) => {
                    // An identifier by itself should mean we're about to see a
                    // specific identifier. Note that for <custom-idents> we
                    // have that they "[must] not be misinterpreted as a
                    // pre-defined keyword in that property’s value
                    // definition". Here that means CSS-wide keywords!
                    expect_disjunction(syntax);
                    if is_css_wide_keyword(&id) {
                        Err(())
                    } else {
                        push_type(syntax, Type::SpecificIdent(Ident((**id).into())));
                        Ok(State::AfterType {
                            after_whitespace: false,
                        })
                    }
                }
                (State::Start { .. }, _) => Err(()),

                (State::AfterOpenAngle, &Token::Ident(ref id)) => {
                    // We should be in something like '<length>' here.
                    // https://drafts.css-houdini.org/css-properties-values-api/#supported-syntax-strings
                    push_type(
                        syntax,
                        match &**id {
                            "length" => Type::Length,
                            "number" => Type::Number,
                            "percentage" => Type::Percentage,
                            "length-percentage" => Type::LengthPercentage,
                            "color" => Type::Color,
                            "image" => Type::Image,
                            "url" => Type::Url,
                            "integer" => Type::Integer,
                            "angle" => Type::Angle,
                            "time" => Type::Time,
                            //"resolution" => Type::Resolution,
                            "transform-list" => Type::TransformList,
                            "custom-ident" => Type::CustomIdent,
                            _ => return Err(()),
                        },
                    );
                    Ok(State::AfterTypeName)
                }
                (State::AfterOpenAngle, _) => Err(()),

                (State::AfterTypeName, &Token::Delim('>')) => {
                    // This should be the end of something like '<length>'.
                    Ok(State::AfterType {
                        after_whitespace: false,
                    })
                }
                (State::AfterTypeName, _) => Err(()),

                (State::AfterType { .. }, &Token::WhiteSpace(_)) => {
                    // Ignore whitespace.
                    Ok(State::AfterType {
                        after_whitespace: true,
                    })
                }
                (
                    State::AfterType {
                        after_whitespace: false,
                    },
                    &Token::Delim('+'),
                ) => {
                    // We should be following some type.
                    // We should panic if we're not, because we should only get
                    // here from Start -> AfterOpenAngle -> AfterTypeName (in
                    // the case of something like '<length>') or
                    // Start (in the case of something like 'my-ident'), both
                    // of which should have pushed a type.
                    if let Some(Syntax::Disjunction(ref mut ts)) = *syntax {
                        let term = &mut ts.last_mut().unwrap();
                        if term.typ == Type::TransformList {
                            // <transform-list>+ is specifically disallowed.
                            return Err(());
                        }
                        term.list = true
                    } else {
                        unreachable!()
                    }
                    Ok(State::AfterPlus)
                }
                (State::AfterType { .. }, &Token::Delim('|')) => {
                    // Some other term in the disjunction should follow.
                    Ok(State::Start { after_bar: true })
                }
                (State::AfterType { .. }, _) => Err(()),

                (State::AfterAsterisk, &Token::WhiteSpace(_)) => Ok(State::AfterAsterisk),
                (State::AfterAsterisk, _) => Err(()),
                (State::AfterPlus, &Token::WhiteSpace(_)) => Ok(State::AfterPlus),
                (State::AfterPlus, &Token::Delim('|')) => {
                    // Some other term in the disjunction should follow.
                    Ok(State::Start { after_bar: true })
                }
                (State::AfterPlus, _) => Err(()),
            }
        }

        // Loop over all of the tokens in the syntax string.
        loop {
            match parser.next_including_whitespace_and_comments() {
                Err(BasicParseError {
                    kind: BasicParseErrorKind::EndOfInput,
                    ..
                }) => {
                    match state {
                        State::Start { after_bar: false }
                        | State::AfterType { .. }
                        | State::AfterAsterisk
                        | State::AfterPlus => break,

                        // We shouldn't reach EOF in the middle of something.
                        State::Start { after_bar: true }
                        | State::AfterOpenAngle
                        | State::AfterTypeName => return Err(()),
                    }
                }
                Err(_) => return Err(()),
                Ok(token) => match handle_token(&mut syntax, state, token) {
                    Ok(s) => state = s,
                    Err(()) => return Err(()),
                },
            }
        }

        syntax.ok_or(())
    }

    /// Parse some value following this syntax.
    ///
    /// It's the responsibility of the caller to appropriately delimit the
    /// input, and to make sure that the expected amount of input was consumed.
    /// This should accordingly be called with a delimited parser.
    /// This is a difference from the `parse` function provided by the `Parse`
    /// trait, along with the fact that this returns a `SpecifiedValue`
    /// rather than `Self`.
    pub fn parse<'i, 't>(
        &self,
        context: &ParserContext,
        input: &mut Parser<'i, 't>,
    ) -> Result<SpecifiedValue, style_traits::ParseError<'i>> {
        let start = input.state();

        // Check to make sure the entirety of the input isn't some CSS-wide
        // keyword.
        if let Ok(ident) = input.expect_ident_cloned() {
            if input.is_exhausted() {
                if is_css_wide_keyword(&ident) {
                    return Err(input
                        .current_source_location()
                        .new_unexpected_token_error(Token::Ident(ident)));
                }
            }
        }

        input.reset(&start);

        match *self {
            Syntax::Anything => {
                custom_properties::SpecifiedValue::parse(context, input)
                    // Don't allow variable references: they should have been
                    // expanded by now.
                    .and_then(|x| {
                        if x.has_references() {
                            Err(input
                                .current_source_location()
                                .new_unexpected_token_error(Token::Function("var".into())))
                        } else {
                            Ok(x)
                        }
                    })
                    .map(|x| SpecifiedValue::Item(SpecifiedValueItem::TokenStream((**x).clone())))
            }
            Syntax::Disjunction(ref terms) => {
                for term in terms {
                    if term.list {
                        let mut outputs = Vec::new();
                        loop {
                            // TODO(jyc) Extend parse_until_before to take
                            // space as a delimiter?
                            match term.typ.parse(context, input) {
                                Err(_) => break,
                                Ok(x) => outputs.push(x),
                            }
                            // Need at least one.
                            if input.is_exhausted() {
                                return Ok(SpecifiedValue::List(outputs));
                            }
                            if let Err(_) = input.expect_whitespace() {
                                break;
                            }
                        }
                    } else {
                        // If we fail to parse, try again!
                        if let Ok(x) = term.typ.parse(context, input) {
                            return Ok(SpecifiedValue::Item(x));
                        }
                    }
                    input.reset(&start)
                }
                Err(input
                    .current_source_location()
                    .new_custom_error(StyleParseErrorKind::UnspecifiedError))
            }
        }
    }

    /// Whether this Syntax contains any <length> or <length-percentage>
    /// component.
    ///
    /// TODO(jyc) Also returns true if it contains a <transform-list> component,
    /// although the spec doesn't say this. Update pending clarification.
    ///
    /// Used to determine which properties might depend on font-size, which
    /// allows us to resolve dependency cycles via relative units.
    /// https://drafts.css-houdini.org/css-properties-values-api/#dependency-cycles-via-relative-units
    pub fn has_length_component(&self) -> bool {
        match *self {
            Syntax::Anything => false,
            Syntax::Disjunction(ref terms) => {
                for term in terms {
                    match term.typ {
                        Type::Length | Type::LengthPercentage => return true,
                        Type::TransformList => return true,
                        _ => (),
                    }
                }
                false
            }
        }
    }
}

/// A single specified typed value.
#[derive(Clone, ToCss)]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub enum SpecifiedValueItem {
    /// A single specified <length> value.
    Length(specified::Length),
    /// A single specified <number> value.
    Number(specified::Number),
    /// A single specified <percentage> value.
    Percentage(specified::Percentage),
    /// A single specified <length-percentage> value.
    LengthPercentage(specified::LengthPercentage),
    /// A single specified <color> value.
    Color(specified::Color),
    /// A single specified <image> value.
    Image(specified::Image),
    /// A single specified <url> value.
    Url(specified::url::SpecifiedUrl),
    /// A single specified <integer> value.
    Integer(specified::Integer),
    /// A single specified <angle> value.
    Angle(specified::Angle),
    /// A single specified <time> value.
    Time(specified::Time),
    // FIXME: We should support <resolution> as well.
    /// A single specified <transform-list> value (note that this is composed of
    /// multiple <transform-functions>.
    TransformList(transform::SpecifiedValue),
    /// A single specified <custom-ident> value.
    CustomIdent(Ident),
    /// A single specified <custom-ident> value that matches the specific ident
    /// contained herein.
    SpecificIdent(Ident),
    /// A <token-stream> value.
    TokenStream(custom_properties::TokenStream),
}

impl OneOrMoreSeparated for SpecifiedValueItem {
    type S = Space;
}

/// Dependencies indicates the set of values that are necessary in order to
/// compute the associated specified value of some registered custom property.
#[derive(Default, PartialEq)]
pub struct Dependencies {
    /// Whether the computed value of 'font-size' is necessary for computation.
    pub font_size: bool,
    /// Whether the computed value of 'font-size' on the root element is
    /// necessary for computation.
    pub root_font_size: bool,

    /// Whether information about the viewport is necessary for computation.
    pub viewport: bool,
    // TODO(jyc) Looks like we don't support lh units?
    //pub line_height: bool,
    //pub root_line_height: bool,
}

impl Dependencies {
    /// Whether no values are necessary for computation.
    pub fn is_empty(&self) -> bool {
        *self == Default::default()
    }
}

impl SpecifiedValueItem {
    /// Returns a Dependencies struct indicating the set of values necessary to
    /// compute this specified value.
    pub fn dependencies(&self) -> Dependencies {
        use self::specified::{
            CalcLengthPercentage, FontRelativeLength, Length, LengthPercentage, NoCalcLength,
        };
        use self::values::generics::transform::TransformOperation;

        let mut dependencies: Dependencies = Default::default();

        fn check_no_calc_length(length: &NoCalcLength, dependencies: &mut Dependencies) {
            match *length {
                NoCalcLength::Absolute(_) => (),
                // FIXME: 0em should be computationally independent.
                NoCalcLength::FontRelative(FontRelativeLength::Em(_))
                | NoCalcLength::FontRelative(FontRelativeLength::Ex(_))
                | NoCalcLength::FontRelative(FontRelativeLength::Ch(_)) => {
                    dependencies.font_size = true
                }
                NoCalcLength::FontRelative(FontRelativeLength::Rem(_)) => {
                    dependencies.root_font_size = true
                }
                NoCalcLength::ViewportPercentage(_) => dependencies.viewport = true,
                NoCalcLength::ServoCharacterWidth(_) => dependencies.font_size = true,
            }
        }

        fn check_calc(calc: &Box<CalcLengthPercentage>, dependencies: &mut Dependencies) {
            match **calc {
                CalcLengthPercentage {
                    clamping_mode: _,
                    absolute: _,
                    ref vw,
                    ref vh,
                    ref vmin,
                    ref vmax,
                    ref em,
                    ref ex,
                    ref ch,
                    ref rem,
                    percentage: _,
                } => {
                    for part in &[vw, vh, vmin, vmax] {
                        if matches!(**part, Some(x) if x != 0.0) {
                            dependencies.viewport = true
                        }
                    }
                    for part in &[em, ex, ch] {
                        if matches!(**part, Some(x) if x != 0.0) {
                            dependencies.font_size = true
                        }
                    }
                    if matches!(*rem, Some(x) if x != 0.0) {
                        dependencies.root_font_size = true
                    }
                }
            }
        }

        fn check_length(length: &Length, dependencies: &mut Dependencies) {
            match *length {
                Length::NoCalc(ref length) => check_no_calc_length(length, dependencies),
                Length::Calc(ref calc) => check_calc(calc, dependencies),
            }
        }

        fn check_length_or_percentage(
            length_or_percentage: &LengthPercentage,
            dependencies: &mut Dependencies,
        ) {
            match *length_or_percentage {
                LengthPercentage::Length(ref length) => check_no_calc_length(length, dependencies),
                LengthPercentage::Percentage(_) => (),
                LengthPercentage::Calc(ref calc) => check_calc(calc, dependencies),
            }
        }

        fn check_transform_list(
            transform_list: &transform::SpecifiedValue,
            dependencies: &mut Dependencies,
        ) {
            for operation in transform_list.0.iter() {
                match *operation {
                    TransformOperation::Matrix(_) => (),
                    TransformOperation::Matrix3D { .. } => (),
                    TransformOperation::Skew(_, _) => (),
                    TransformOperation::SkewX(_) => (),
                    TransformOperation::SkewY(_) => (),
                    TransformOperation::Translate(ref tx, ref ty) => {
                        check_length_or_percentage(tx, dependencies);
                        check_length_or_percentage(ty, dependencies);
                    }
                    TransformOperation::TranslateX(ref tx) => {
                        check_length_or_percentage(tx, dependencies)
                    }
                    TransformOperation::TranslateY(ref ty) => {
                        check_length_or_percentage(ty, dependencies)
                    }
                    TransformOperation::TranslateZ(ref length) => {
                        check_length(length, dependencies)
                    }
                    TransformOperation::Translate3D(ref tx, ref ty, ref tz) => {
                        check_length_or_percentage(tx, dependencies);
                        check_length_or_percentage(ty, dependencies);
                        check_length(tz, dependencies);
                    }
                    TransformOperation::Scale(_, _) => (),
                    TransformOperation::ScaleX(_) => (),
                    TransformOperation::ScaleY(_) => (),
                    TransformOperation::ScaleZ(_) => (),
                    TransformOperation::Scale3D(_, _, _) => (),
                    TransformOperation::Rotate(_) => (),
                    TransformOperation::RotateX(_) => (),
                    TransformOperation::RotateY(_) => (),
                    TransformOperation::RotateZ(_) => (),
                    TransformOperation::Rotate3D(_, _, _, _) => (),
                    TransformOperation::Perspective(ref length) => {
                        check_length(length, dependencies)
                    }
                    TransformOperation::InterpolateMatrix {
                        ref from_list,
                        ref to_list,
                        ..
                    } => {
                        check_transform_list(from_list, dependencies);
                        check_transform_list(to_list, dependencies);
                    }
                    TransformOperation::AccumulateMatrix {
                        ref from_list,
                        ref to_list,
                        ..
                    } => {
                        check_transform_list(from_list, dependencies);
                        check_transform_list(to_list, dependencies);
                    }
                }
            }
        }

        use self::SpecifiedValueItem::*;

        match *self {
            Length(ref length) => check_length(length, &mut dependencies),
            Number(_) => (),
            Percentage(_) => (),
            LengthPercentage(ref length_or_percentage) => {
                check_length_or_percentage(length_or_percentage, &mut dependencies)
            }
            Color(_) => (),
            Image(_) => (),
            Url(_) => (),
            Integer(_) => (),
            Angle(_) => (),
            Time(_) => (),
            TransformList(ref transform_list) => {
                check_transform_list(transform_list, &mut dependencies)
            }
            CustomIdent(_) => (),
            SpecificIdent(_) => (),
            TokenStream(_) => (),
        }

        dependencies
    }

    /// Returns whether or not this specified value is computationally
    /// independent, that is, 'if it can be converted into a computed value
    /// using only the value of the property on the element, and "global"
    /// information that cannot be changed by CSS.'
    /// https://drafts.css-houdini.org/css-properties-values-api-1/#computationally-independent
    pub fn is_computationally_independent(&self) -> bool {
        self.dependencies().is_empty()
    }

    /// Attempts to convert this specified value to a computed value using the
    /// given context.
    pub fn to_computed_value(&self, context: &computed::Context) -> Result<ComputedValueItem, ()> {
        macro_rules! compute {
            ($_self:expr, $context:expr,

             $($typ:ident),*) => {
                match $_self {
                    $(
                        SpecifiedValueItem::$typ(ref value) => {
                            Ok(ComputedValueItem::$typ(value.to_computed_value($context)))
                        }
                    ), *

                    // Special cases.
                    // Would put in the match syntax, but we can't have things
                    // expand to match cases.

                    SpecifiedValueItem::CustomIdent(ref ident) =>
                        Ok(ComputedValueItem::CustomIdent(ident.clone())),
                    SpecifiedValueItem::SpecificIdent(ref ident) =>
                        Ok(ComputedValueItem::SpecificIdent(ident.clone())),
                    SpecifiedValueItem::TokenStream(ref stream) =>
                        Ok(ComputedValueItem::TokenStream(stream.clone())),
                }
            };
        }

        compute! {
            *self, context,

            Length, Number, Percentage, LengthPercentage, Color, Image, Integer,
            Angle, Time, TransformList, Url
        }
    }
}

/// A specified typed value.
///
/// A value can either be a single item or a list of items.
#[derive(Clone, ToCss)]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub enum SpecifiedValue {
    /// A single specified value.
    Item(SpecifiedValueItem),
    /// A list of one or more specified values.
    /// Note that we cannot have lists of <transform-lists>.
    List(Vec<SpecifiedValueItem>),
}

/// If `list` and `x` are both `Ok(..)`, append the contents of `x` to `list`.
/// To be used as an argument to fold to convert a sequence of `Result<T, E>`s
/// to a `Result<Vec<T>, E>` (pulling the `Result` outside).
fn all<T, E>(list: Result<Vec<T>, E>, x: Result<T, E>) -> Result<Vec<T>, E> {
    list.and_then(|mut list| {
        x.map(move |x| {
            list.push(x);
            list
        })
    })
}

impl SpecifiedValue {
    /// Returns whether or not this specified value is computationally
    /// independent.
    /// See the comment on SpecifiedValueItem::is_computationally_independent.
    pub fn is_computationally_independent(&self) -> bool {
        match *self {
            SpecifiedValue::Item(ref item) => item.is_computationally_independent(),
            SpecifiedValue::List(ref items) => {
                items.iter().all(|x| x.is_computationally_independent())
            }
        }
    }

    /// Attempts to convert this specified value to a computed value.
    pub fn to_computed_value(&self, context: &computed::Context) -> Result<ComputedValue, ()> {
        match *self {
            SpecifiedValue::Item(ref item) => item
                .to_computed_value(context)
                .map(|x| ComputedValue::Item(x)),
            SpecifiedValue::List(ref items) => {
                // All of the items must compute successfully.
                items
                    .iter()
                    .map(|x| x.to_computed_value(context))
                    .fold(Ok(Vec::new()), all)
                    .map(ComputedValue::List)
            }
        }
    }

    /// Returns the CSS serialization of this specified value.
    pub fn to_token_stream(&self) -> TokenStream {
        TokenStream::from_css(self)
    }
}

/// A single computed typed value.
#[derive(Clone, Debug, MallocSizeOf, PartialEq, ToCss, ToShmem)]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub enum ComputedValueItem {
    /// A single computed <length> value.
    Length(computed::Length),
    /// A single computed <number> value.
    Number(computed::Number),
    /// A single computed <percentage> value.
    Percentage(computed::Percentage),
    /// A single computed <length-percentage> value.
    LengthPercentage(computed::LengthPercentage),
    /// A single computed <color> value.
    Color(computed::Color),
    /// A single computed <image> value.
    Image(computed::Image),
    /// A single computed <url> value.
    Url(computed::url::ComputedUrl),
    /// A single computed <integer> value.
    Integer(computed::Integer),
    /// A single computed <angle> value.
    Angle(computed::Angle),
    /// A single computed <time> value.
    Time(computed::Time),
    // FIXME: We should support <resolution> values as well.
    /// A single computed <transform-list> value (note that this is composed of
    /// multiple <transform-functions>.
    TransformList(transform::computed_value::T),
    /// A single computed <custom-ident> value.
    CustomIdent(Ident),
    /// A single specified <custom-ident> value that matches the specific ident
    /// contained herein.
    SpecificIdent(Ident),
    /// A computed <token-stream> value (the same as an uncomputed
    /// <token-stream> value).
    TokenStream(custom_properties::TokenStream),
}

impl OneOrMoreSeparated for ComputedValueItem {
    type S = Space;
}

/// A computed typed value.
#[derive(Clone, Debug, MallocSizeOf, PartialEq, ToCss, ToShmem)]
#[cfg_attr(feature = "servo", derive(HeapSizeOf))]
pub enum ComputedValue {
    /// A single value.
    Item(ComputedValueItem),
    /// A list of one or more values.
    List(Vec<ComputedValueItem>),
}

impl ComputedValue {
    /// Returns the CSS serialization of this computed value.
    pub fn to_token_stream(&self) -> TokenStream {
        TokenStream::from_css(self)
    }
}

/// A map from CSS variable names to CSS variable computed values, used for
/// resolving.
///
/// This composes a custom_properties::CustomPropertiesMap, which maps CSS
/// variable names to their token stream values.
///
/// We also keep track of whether or not this contains any uninherited
/// properties, in order to just clone an Arc when possible.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct CustomPropertiesMap {
    untyped_map: custom_properties::CustomPropertiesMap,
    /// Whether this map contains typed properties that were registered as
    /// uninherited. If true, this property map may not be shareable in some
    /// situations.
    pub has_uninherited: bool,
}

impl Deref for CustomPropertiesMap {
    type Target = custom_properties::CustomPropertiesMap;

    fn deref(&self) -> &Self::Target {
        &self.untyped_map
    }
}

impl DerefMut for CustomPropertiesMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.untyped_map
    }
}

enum ComputationResult {
    Shareable,
    Computed(VariableValue),
    Error,
}

fn compute_substituted_value(
    context: Option<&computed::Context>,
    registration: Option<&Registration>,
    substituted_value: TokenStream,
    extra: &ExtraData,
) -> ComputationResult {
    match (extra, registration) {
        (ExtraData::Specified(_), None) => {
            ComputationResult::Computed(VariableValue::computed(substituted_value))
        }
        (ExtraData::Computed, _) => ComputationResult::Shareable,
        (ExtraData::Precomputed(_), _) => ComputationResult::Shareable,

        // Compute uncomputed typed custom property values.
        (ExtraData::Specified(ref url_data), Some(&Registration { ref syntax, .. })) => {
            let context = context.expect("compute_substituted_value: can't compute typed custom property value without Context");
            let parser_context = ParserContext::new(
                Origin::Author,
                url_data,
                /* rule_type */ None,
                ParsingMode::DEFAULT,
                context.quirks_mode,
                /* error_reporter */ None,
                /* use_counters */ None,
            );
            let mut input = ParserInput::new(&substituted_value.css);
            let mut input = Parser::new(&mut input);
            // TODO In the future, we might want to save the
            // ComputedValue so that Typed OM can use it. Right now
            // we only keep the serialization.
            // XXX(jyc) This should become ExtraData::Precomputed and then
            // computed token stream. We shouldn't throw away the
            // ComputedValue...
            syntax
                .parse(&parser_context, &mut input)
                .ok()
                .and_then(|parsed_value| parsed_value.to_computed_value(context).ok())
                .map(|computed_value| ComputationResult::Computed((&computed_value).into()))
                .unwrap_or(ComputationResult::Error)
        }
    }
}

/// A SubstitutionMap that fills in the initial values of of registered custom
/// properties.
pub struct PVSubstitutionMap<'a> {
    /// Custom property registrations. This is used to provide the
    /// initial values lacking entries in `custom_properties`.
    pub registered_property_set: Option<&'a RegisteredPropertySet>,

    /// Custom property values.
    pub custom_properties: &'a CustomPropertiesMap,
}

impl<'a> SubstitutionMap for PVSubstitutionMap<'a> {
    fn get(&self, name: &Name) -> Option<&TokenStream> {
        if let Some(var) = self.custom_properties.get(name) {
            debug_assert!(!var.has_references());
            return Some(&var.token_stream);
        }

        let initial_value = self
            .registered_property_set
            .and_then(|rps| rps.get(name))
            .and_then(|r| r.initial_value.as_ref());

        if let Some(&(ref _value, ref css)) = initial_value {
            return Some(css);
        }

        None
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum PVName {
    Variable(Name),
    FontSize,
}

impl Hash for PVName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // custom_properties::resolve_all uses a PrecomputedHashMap with
        // ResolutionMap::Name as the key type. A PrecomputedHashMap is a
        // HashMap using a PrecomputedHasher; a PrecomputedHasher expects
        // Hasher::write_u32 to be called once, and just returns that for
        // Hasher::finish. custom_properties::Name does this; we implement Hash
        // here to do this for PVName.
        match *self {
            PVName::Variable(ref name) => name.hash(state),
            PVName::FontSize => state.write_u32(0),
        }
    }
}

fn detect_ems<'i, 'tt>(input: &mut Parser<'i, 'tt>) -> Result<bool, cssparser::ParseError<'i, ()>> {
    while !input.is_exhausted() {
        let token = input.next()?.clone();
        // We have to descend into functions.
        match token {
            Token::Function(_) => {
                if input.parse_nested_block(detect_ems).unwrap() {
                    return Ok(true);
                }
            }
            Token::Dimension { ref unit, .. } => {
                if unit.eq_ignore_ascii_case("em") || unit.eq_ignore_ascii_case("rem") {
                    return Ok(true);
                }
            }
            _ => (),
        }
    }
    Ok(false)
}

struct PVResolutionMap<'a, 'b: 'a> {
    // Inputs

    // Might be needed for computation. None if we should just not compute,
    // a hack for declaration_block. TODO Fix this!
    context: Option<&'a computed::Context<'b>>,
    registered_property_set: Option<&'a RegisteredPropertySet>,
    environment: &'a CssEnvironment,
    font_size_references: Option<&'a [Name]>,

    // Outputs
    // font_size_dependent and font_size_cyclical are only populated for early
    // resolution maps.

    font_size_dependent: Option<PrecomputedHashSet<Name>>,
    font_size_cyclical: bool,
    custom_properties: &'a mut CustomPropertiesMap,
}

impl<'a, 'b: 'a> PVResolutionMap<'a, 'b> {
    // Create a PVResolutionMap for the resolution of early custom properties
    // (i.e. those independent of standard properties, e.g. font-size), and for
    // the identification of font-size-dependent properties and whether
    // font-size is involved in a dependency cycle.
    //
    // The computed `context` is necessary to compute relative URL values.
    //
    // TODO(jyc) It is an Option right now because declaration_block performs
    // custom property resolution on its own without access to a
    // computed::Context, for some reason. Should we fix this?
    //
    // `substitute`, when called on late custom properties, will no-op.
    fn new_early(
        context: Option<&'a computed::Context<'b>>,
        registered_property_set: Option<&'a RegisteredPropertySet>,
        environment: &'a CssEnvironment,
        font_size_references: &'a [Name],

        custom_properties: &'a mut CustomPropertiesMap,
    ) -> PVResolutionMap<'a, 'b> {
        let mut font_size_dependent: Option<PrecomputedHashSet<Name>> = None;

        // Populate font_size_dependent with the properties which need to be
        // computed after font-size (and which might create cycles if font-size
        // refers to them).
        for (name, value) in custom_properties.iter() {
            let registration = registered_property_set.and_then(|set| set.get(name));
            let syntax_has_length_component =
                registration
                .map_or(false, |r| r.syntax.has_length_component());
            if !syntax_has_length_component {
                continue;
            }

            let mut input = ParserInput::new(&value.css);
            let mut input = Parser::new(&mut input);
            if detect_ems(&mut input).unwrap() {
                font_size_dependent
                    .get_or_insert_with(|| Default::default())
                    .insert(name.clone());
            }
        }

        PVResolutionMap {
            context,
            registered_property_set,
            environment,
            font_size_references: Some(font_size_references),

            font_size_dependent,
            font_size_cyclical: false,
            custom_properties,
        }
    }

    // Create a PVResolutionMap for the resolution of late custom properties
    // (i.e. those dependent on standard properties, e.g. font-size).
    //
    // `substitute`, when called on a custom property with no references, will
    // no-op (as it would if the map were created using `new_early`). This means
    // that early custom properties which have already been resolved will not
    // incur extra work. `substitute` called on all other properties will
    // perform resolution and computation as expected.
    fn new_late(
        context: &'a computed::Context<'b>,
        registered_property_set: &'a RegisteredPropertySet,
        environment: &'a CssEnvironment,

        custom_properties: &'a mut CustomPropertiesMap,
    ) -> PVResolutionMap<'a, 'b> {
        PVResolutionMap {
            context: Some(context),
            registered_property_set: Some(registered_property_set),
            environment,
            font_size_references: None,

            font_size_dependent: None,
            font_size_cyclical: false,
            custom_properties,
        }
    }
}

impl<'a, 'b: 'a> ResolutionMap for PVResolutionMap<'a, 'b> {
    type Name = PVName;

    fn names(&self) -> Vec<PVName> {
        let mut names: Vec<PVName> = self
            .custom_properties
            .iter()
            .map(|(name, _)| PVName::Variable(name.clone()))
            .collect();

        if matches!(self.font_size_references, Some(r) if r.len() > 0) {
            names.push(PVName::FontSize);
        }
        names
    }

    fn has_references(&self, name: &PVName) -> bool {
        match *name {
            PVName::Variable(ref name) => {
                // Return whether the custom property is in the custom
                // properties map and either has variable references or is for a
                // typed property whose syntax has a length component.
                let has_references = self
                    .custom_properties
                    .get(name)
                    .map_or(false, |value| value.has_references());
                has_references
                    || self
                        .font_size_dependent
                        .as_ref()
                        .map_or(false, |d| d.contains(name))
            }
            PVName::FontSize => {
                self.font_size_references.map_or(false, |r| r.len() > 0)
            }
        }
    }

    fn references(&self, name: &PVName) -> Vec<PVName> {
        match *name {
            PVName::Variable(ref name) => {
                let mut references = self
                    .custom_properties
                    .get(name)
                    .and_then(|value| value.references.as_ref())
                    .map_or(vec![], |names| {
                        names
                            .iter()
                            .map(|name| PVName::Variable(name.clone()))
                            .collect()
                    });
                // https://drafts.css-houdini.org/css-properties-values-api/#dependency-cycles-via-relative-units
                if self
                    .font_size_dependent
                    .as_ref()
                    .map_or(false, |s| s.contains(name))
                {
                    references.push(PVName::FontSize);
                }
                references
            }
            PVName::FontSize => self.font_size_references.map_or(vec![], |r| {
                r.iter()
                    .map(|name| PVName::Variable(name.clone()))
                    .collect()
            }),
        }
    }

    fn substitute(&mut self, name: &PVName) {
        match *name {
            PVName::Variable(ref name) => {
                // We are only substituting early, non-font-size-dependent
                // custom properties here.
                //
                // What about properties that depend on font-size-dependent
                // properties? We will check for them later.
                if self
                    .font_size_dependent
                    .as_ref()
                    .map_or(false, |s| s.contains(name))
                {
                    return;
                }

                // First, substitute references in the variable value to obtain
                // a substituted value.
                let substituted = {
                    // We get value in here so that we don't have to keep its
                    // borrow of self.custom_properties, because at the end we
                    // mutably borrow that to insert/remove.
                    let (value, extra) = match self.custom_properties.get(name) {
                        Some(value) => (value, value.extra.clone()),
                        None => {
                            return;
                        }
                    };
                    if !value.has_references() {
                        // XXX(jyc) Would be nice to only clone this if we
                        // don't return ComputationResult::Shareable.
                        Some((value.token_stream.clone(), extra))
                    } else {
                        // Check if we depend on a font-size-dependent property.
                        match (&value.references, &mut self.font_size_dependent) {
                            (&Some(ref references), &mut Some(ref mut font_size_dependent)) => {
                                let is_font_size_dependent = references
                                    .iter()
                                    .filter(|name| font_size_dependent.contains(*name))
                                    .next()
                                    .is_some();
                                if is_font_size_dependent {
                                    // Mark that we are font-size-dependent so
                                    // things that depend on us will be so marked as
                                    // well.
                                    // XXX(jyc) Would be nice to get rid of the
                                    // name.clone(). Just didn't have time to work
                                    // out how to tell rustc that the PVName's
                                    // lifetime should contain
                                    // self.custom_properties_map.
                                    font_size_dependent.insert(name.clone());
                                    return;
                                }
                            }
                            _ => (),
                        }

                        // |ok()| lets us drop the lifetime bound on the Err
                        // case that ties the returned value to our borrow of
                        // self.custom_properties.
                        custom_properties::substitute_references_in_value(
                            value,
                            &PVSubstitutionMap {
                                registered_property_set: self.registered_property_set,
                                custom_properties: &*self.custom_properties,
                            },
                            self.environment,
                        )
                        .map(|substituted_value| (substituted_value, extra))
                        .ok()
                    }
                };

                // Next, compute the substituted value. If this is an untyped
                // property, its computed value is just its substituted value,
                // but if this is is a typed property we need to try and compute
                // it.
                let computed_value = substituted
                    .map(|(substituted_value, extra)| {
                        let registration =
                            self.registered_property_set.and_then(|set| set.get(name));
                        compute_substituted_value(
                            self.context,
                            registration,
                            substituted_value,
                            &extra,
                        )
                    })
                    .unwrap_or(ComputationResult::Error);

                // Finally, insert the computed value into the output map of
                // computed custom properties, replacing its uncomputed value,
                // or replace it with its initial value it if we failed to
                // compute.
                match computed_value {
                    ComputationResult::Shareable => {
                        let shared_value = self.custom_properties.get(name).unwrap().clone();
                        self.custom_properties.insert(name.clone(), shared_value);
                    }
                    ComputationResult::Computed(computed_value) => {
                        self.custom_properties
                            .insert(name.clone(), Arc::new(computed_value));
                    }
                    ComputationResult::Error => {
                        self.custom_properties.remove(name);
                    }
                }
            }
            // We aren't actually going to substitute anything depending on
            // font-size in this pass, so we don't need to substitute references
            // in font-size. That'll be handled by |cascade| before we
            // substitute the non-early custom properties.
            PVName::FontSize => (),
        }
    }

    fn cyclical(&mut self, name: &PVName) {
        match *name {
            PVName::Variable(ref name) => {
                self.custom_properties.remove(name);
                // The variable has been unset; its initial value is
                // necessarily font-size-independent.
                self.font_size_dependent.as_mut().map(|d| d.remove(name));
            }
            PVName::FontSize => {
                self.font_size_cyclical = true;
            }
        }
    }
}

/// A struct that takes care of encapsulating the cascade process for custom
/// properties.
pub struct CustomPropertiesBuilder<'a> {
    // Inputs

    registered_property_set: Option<&'a RegisteredPropertySet>,
    // This is an Arc so that we can just clone it if no custom properties were
    // set.
    parent_properties: Option<&'a Arc<CustomPropertiesMap>>,
    environment: &'a CssEnvironment,

    // State

    seen: PrecomputedHashSet<&'a Name>,
    reverted: PerOrigin<PrecomputedHashSet<&'a Name>>,
    may_require_resolution: bool,

    // Outputs

    result_properties: Option<CustomPropertiesMap>,
}

impl<'a> CustomPropertiesBuilder<'a> {
    /// Create a new builder, inheriting from a given custom properties map.
    pub fn new(
        registered_property_set: Option<&'a RegisteredPropertySet>,
        parent_properties: Option<&'a Arc<CustomPropertiesMap>>,
        environment: &'a CssEnvironment,
    ) -> Self {
        Self {
            registered_property_set,
            parent_properties,
            environment,

            seen: Default::default(),
            reverted: Default::default(),
            may_require_resolution: false,

            result_properties: None,
        }
    }

    fn registration(&self, name: &Name) -> Option<&Registration> {
        self.registered_property_set
            .and_then(|rps| rps.registrations.get(name))
    }

    // Takes self.parent_properties and removes uninherited properties (if any)
    // to yield a new non-empty initial map to be used to initialize
    // result_properties, or None otherwise.
    fn inherited_properties(&self) -> Option<CustomPropertiesMap> {
        match self.parent_properties {
            Some(properties) if properties.has_uninherited => {
                let mut inherited_properties: CustomPropertiesMap = (**properties).clone();
                properties
                    .iter()
                    .for_each(|(name, _)| match self.registration(name) {
                        Some(Registration { inherits: false, .. }) => {
                            inherited_properties.remove(name);
                        },
                        _ => (),
                    });
                if inherited_properties.is_empty() {
                    return None;
                }
                Some(inherited_properties)
            }
            x => x.map(|x| (**x).clone()),
        }
    }

    /// Cascade a given custom property declaration.
    pub fn cascade(&mut self, declaration: &'a CustomDeclaration, origin: Origin) {
        let CustomDeclaration {
            ref name,
            ref value,
        } = *declaration;

        if self.reverted.borrow_for_origin(&origin).contains(&name) {
            return;
        }

        let was_already_present = !self.seen.insert(name);
        if was_already_present {
            return;
        }

        if !self.value_may_affect_style(name, value) {
            return;
        }

        if self.result_properties.is_none() {
            self.result_properties = Some(self.inherited_properties().unwrap_or_default());
        }
        let map = self.result_properties.as_mut().unwrap();

        // Can't use `self.registration` because we borrow `self` mutably when
        // we borrow `self.result_properties` mutably above.
        let registration = self
            .registered_property_set
            .and_then(|rps| rps.registrations.get(name));

        let inherits = registration.map_or(true, |r| r.inherits);
        if !inherits {
            map.has_uninherited = true;
        }

        match (value, inherits) {
            (&CustomDeclarationValue::Value(ref unparsed_value), _) => {
                let has_references = unparsed_value.has_references();
                self.may_require_resolution |= has_references;
                if !self.may_require_resolution {
                    // We could make this check more fine-grained by parsing the
                    // value and looking for font-size references, as in
                    // PVResolutionMap::new_early.
                    let might_depend_on_font_size = registration
                        .map(|r| &r.syntax)
                        .map_or(false, Syntax::has_length_component);
                    self.may_require_resolution |= might_depend_on_font_size;
                }

                // If the variable value doesn't require resolution but contains
                // an environment variable reference, perform substitution here
                // instead of forcing a full traversal in `resolve_all`
                // afterwards (if we're doing resolution anyways, don't bother.)
                let should_substitute_environment_references =
                    !self.may_require_resolution &&
                    unparsed_value.references_environment;
                let value = if should_substitute_environment_references {
                    debug_assert!(!unparsed_value.is_computed());
                    let substituted_value = substitute_references_in_value(
                        unparsed_value,
                        &EmptySubstitutionMap {},
                        &self.environment,
                    )
                    .ok();

                    if let Some(value) = substituted_value {
                        Arc::new(VariableValue::computed(value))
                    } else {
                        map.remove(name);
                        return;
                    }
                } else {
                    unparsed_value.clone()
                };
                map.insert(name.clone(), value);
            }

            (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Revert), _) => {
                self.seen.remove(name);
                for origin in origin.following_including() {
                    self.reverted.borrow_mut_for_origin(&origin).insert(name);
                }
            }

            (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Initial), false)
            | (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Unset), false) => {
                unreachable!()
            }

            (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Inherit), false) => {
                if let Some(parent_value) = self.parent_properties.and_then(|ps| ps.get(name)) {
                    map.insert(name.clone(), parent_value.clone());
                }
            }

            (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Inherit), true)
            | (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Unset), true) => {
                unreachable!()
            }

            (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Initial), true) => {
                map.remove(name);
            }
        }
    }

    fn value_may_affect_style(&self, name: &Name, value: &CustomDeclarationValue) -> bool {
        // This is related closely related to to ::inherited_properties.
        let registration = self.registration(name);
        let inherits = registration.map_or(true, |r| r.inherits);
        let parent_value = self.parent_properties.and_then(|ps| ps.get(name));

        match (value, inherits) {
            (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Revert), _) => {
                parent_value.is_some()
            }

            // An explicit 'inherit', or 'unset' for an inherited property (e.g.
            // untyped custom properties or typed inherited properties, as it is
            // then equivalent to 'inherit') means we can just use the inherited
            // value.
            (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Inherit), true)
            | (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Unset), true) => {
                false
            }

            // inherited_properties did something that we will have to undo if
            // we are supposed to inherit from the parent.
            //
            // Inherited properties are copied into result_properties by
            // default. 'initial' means we need to remove the inherited value.
            //
            // Uninherited properties are removed from parent_properties before
            // it is copied to produce result_properties. 'inherit' means we
            // need to add the inherited value back.
            //
            // If both cases, if the parent had no value anyways, we don't need
            // to do anything.
            (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Initial), true)
            | (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Inherit), false) => {
                parent_value.is_some()
            }

            // For an uninherited property, 'initial' or 'unset' mean that the
            // property should be set to its initial value. But
            // inherited_properties already removes uninherited property values
            // from the map, so we don't need to do anything here.
            (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Initial), false)
            | (&CustomDeclarationValue::CSSWideKeyword(CSSWideKeyword::Unset), false) => {
                false
            }

            (&CustomDeclarationValue::Value(ref unparsed_value), true) => {
                parent_value.map_or(true, |v| v != unparsed_value)
            }

            (&CustomDeclarationValue::Value(ref unparsed_value), false) => {
                if let Some(parent_value) = parent_value {
                    return unparsed_value != parent_value
                }
                if let Some(&(ref _value, ref css)) = registration.and_then(|r| r.initial_value.as_ref()) {
                    return unparsed_value.token_stream != *css
                }
                true
            },
        }
    }

    /// Returns a map of applicable custom properties, with early custom
    /// properties fully resolved, but with late custom properties awaiting
    /// resolution with |resolve_late|.
    ///
    /// If possible, we will just return the parent's custom properties map.
    /// Otherwise (e.g. if there was a declaration that affected the style, or
    /// if some declaration requires variable resolution), we will perform
    /// custom property resolution here and wrap the resulting map in an arc.
    ///
    /// TODO Ideally |context| would not be an Option. it is only that way for
    /// |PropertyDeclarationBlock::cascade_custom_properties|, where we are
    /// using this function but aren't actually computing custom properties for
    /// now.
    ///
    /// Returns |(resolved_properties, early_only, font_size_cyclical)|.
    pub fn build_early(
        mut self,
        context: Option<&computed::Context>,
        font_size_references: &[Name],
    ) -> (Option<Arc<CustomPropertiesMap>>, bool, bool) {
        // Get the CustomPropertiesMap to return. If we saw no declarations, we
        // might even be able to return a clone of the Arc for our parent's
        // properties. This will be determined by `inherited_properties`.
        let mut custom_properties = match self.result_properties.take() {
            Some(ps) => ps,
            None => {
                let parent_has_uninherited = self.parent_properties.map_or(false, |x| x.has_uninherited);
                let inherited_properties = if !parent_has_uninherited {
                    self.parent_properties.cloned()
                } else {
                    self.inherited_properties().map(Arc::new)
                };
                return (
                    inherited_properties,
                    /* early_only */ true,
                    /* font_size_cyclical */ false,
                );
            }
        };

        // If we saw any declarations that might require variable resolution,
        // do that now.
        self.may_require_resolution |= !font_size_references.is_empty();
        let (early_only, font_size_cyclical) = if !self.may_require_resolution {
            (true, false)
        } else {
            let mut resolution_map = PVResolutionMap::new_early(
                context,
                self.registered_property_set,
                self.environment,
                font_size_references,
                &mut custom_properties,
            );
            custom_properties::resolve_all(&mut resolution_map);
            let early_only = resolution_map
                .font_size_dependent
                .map_or(true, |d| d.is_empty());
            (early_only, resolution_map.font_size_cyclical)
        };

        (
            Some(Arc::new(custom_properties)),
            early_only,
            font_size_cyclical,
        )
    }
}

/// Performs custom property resolution of 'late' custom properties, i.e. those
/// which depend on the values of early properties, e.g. font-size.
pub fn resolve_late<'a, 'b: 'a>(
    context: &'a computed::Context<'b>,
    registered_property_set: &'a RegisteredPropertySet,
    environment: &'a CssEnvironment,

    custom_properties: &'a mut CustomPropertiesMap,
) {
    let mut resolution_map = PVResolutionMap::new_late(
        context,
        registered_property_set,
        environment,
        custom_properties,
    );
    custom_properties::resolve_all(&mut resolution_map);
}
