use std::{collections::HashMap, hash::Hash};

use cow_utils::CowUtils;
use lazy_regex::Regex;
use oxc_ast::{
    AstKind,
    ast::{Argument, BinaryExpression, Expression},
};
use oxc_diagnostics::OxcDiagnostic;
use oxc_macros::declare_oxc_lint;
use oxc_span::{CompactStr, GetSpan, Span};
use rustc_hash::FxHashMap;
use schemars::JsonSchema;
use serde::Deserialize;

use crate::{
    context::LintContext,
    rule::{DefaultRuleConfig, Rule},
    utils::{JestFnKind, JestGeneralFnKind, PossibleJestNode, parse_general_jest_fn_call},
};

fn title_must_be_string_diagnostic(span: Span) -> OxcDiagnostic {
    OxcDiagnostic::warn("Title must be a string")
        .with_help("Replace your title with a string")
        .with_label(span)
}

fn empty_title_diagnostic(span: Span) -> OxcDiagnostic {
    OxcDiagnostic::warn("Should not have an empty title")
        .with_help("Write a meaningful title for your test")
        .with_label(span)
}

fn duplicate_prefix_diagnostic(span: Span) -> OxcDiagnostic {
    OxcDiagnostic::warn("Should not have duplicate prefix")
        .with_help("The function name already has the prefix, try to remove the duplicate prefix")
        .with_label(span)
}

fn accidental_space_diagnostic(span: Span) -> OxcDiagnostic {
    OxcDiagnostic::warn("Should not have leading or trailing spaces")
        .with_help("Remove the leading or trailing spaces")
        .with_label(span)
}

fn disallowed_word_diagnostic(word: &str, span: Span) -> OxcDiagnostic {
    OxcDiagnostic::warn(format!("{word} is not allowed in test title"))
        .with_help("It is included in the `disallowedWords` of your config file, try to remove it from your title")
        .with_label(span)
}

fn must_match_diagnostic(message: &str, span: Span) -> OxcDiagnostic {
    OxcDiagnostic::warn(message.to_string())
        .with_help("Make sure the title matches the `mustMatch` of your config file")
        .with_label(span)
}

fn must_not_match_diagnostic(message: &str, span: Span) -> OxcDiagnostic {
    OxcDiagnostic::warn(message.to_string())
        .with_help("Make sure the title does not match the `mustNotMatch` of your config file")
        .with_label(span)
}

/// A regex pattern, or a `[pattern, message]` pair.
///
/// - When a `string`, it is treated as a regex pattern.
/// - When an `array`, the first element is the pattern and the second
///   (optional) element is a custom error message displayed when the pattern
///   does not match.
///
/// Patterns may be written as JS regex literals (e.g. `/foo/i`) or as plain
/// strings (e.g. `"^foo"`).
#[derive(Debug, Clone, JsonSchema, Deserialize)]
#[serde(untagged)]
pub enum MatcherPattern {
    /// A plain regex pattern string.
    String(String),
    /// A `[pattern, optional_message]` pair.
    Array(Vec<String>),
}

/// Pattern configuration for `mustMatch` and `mustNotMatch`.
///
/// Can be one of:
/// - A [`MatcherPattern`] applied uniformly to all test-function kinds
///   (`describe`, `test`, `it`).
/// - An object whose keys are any of `"describe"`, `"test"`, or `"it"` and
///   whose values are [`MatcherPattern`] entries, allowing different patterns
///   per kind.
///
/// An empty object (`{}`) is valid and disables the constraint.
#[derive(Debug, Clone, JsonSchema, Deserialize)]
#[serde(untagged)]
pub enum MatcherConfig {
    /// Apply the same pattern to all test-function kinds.
    Pattern(MatcherPattern),
    /// Apply different patterns per test-function kind.
    PerKind(HashMap<String, MatcherPattern>),
}

/// Configuration for the [`ValidTitle`] rule.
#[derive(Debug, Default, Clone, JsonSchema, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct ValidTitleConfig {
    /// When `true`, non-string titles passed to `test` or `it` are not
    /// reported.
    ///
    /// Defaults to `false`.
    pub ignore_type_of_test_name: bool,

    /// When `true`, non-string titles passed to `describe` are not reported.
    ///
    /// Defaults to `false`.
    pub ignore_type_of_describe_name: bool,

    /// When `true`, function-argument variables used as test titles are
    /// allowed.  This is primarily useful for Vitest's parameterized tests.
    ///
    /// Defaults to `false`.
    pub allow_arguments: bool,

    /// A list of words that must not appear in test titles.  Words are matched
    /// case-insensitively and as whole words.
    ///
    /// Example: `["correct", "properly"]`
    pub disallowed_words: Vec<String>,

    /// When `true`, leading and trailing whitespace in test titles is ignored.
    ///
    /// Defaults to `false`.
    pub ignore_spaces: bool,

    /// Regex pattern(s) that test titles **must** match.
    ///
    /// Accepts a [`MatcherPattern`] (applied to all kinds) or an object
    /// mapping `"describe"`, `"test"`, and/or `"it"` to their own
    /// [`MatcherPattern`].
    pub must_match: Option<MatcherConfig>,

    /// Regex pattern(s) that test titles **must not** match.
    ///
    /// Accepts a [`MatcherPattern`] (applied to all kinds) or an object
    /// mapping `"describe"`, `"test"`, and/or `"it"` to their own
    /// [`MatcherPattern`].
    pub must_not_match: Option<MatcherConfig>,
}

// Internal compiled representation stored inside the rule at runtime.
#[derive(Clone)]
pub struct ValidTitleCompiled {
    ignore_type_of_test_name: bool,
    ignore_type_of_describe_name: bool,
    allow_arguments: bool,
    disallowed_words: Vec<CompactStr>,
    ignore_spaces: bool,
    must_not_match_patterns: FxHashMap<MatchKind, CompiledMatcherAndMessage>,
    must_match_patterns: FxHashMap<MatchKind, CompiledMatcherAndMessage>,
}

#[derive(Debug, Default, Clone)]
pub struct ValidTitle(Box<ValidTitleCompiled>);

impl std::fmt::Debug for ValidTitleCompiled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ValidTitleCompiled")
            .field("ignore_type_of_test_name", &self.ignore_type_of_test_name)
            .field("ignore_type_of_describe_name", &self.ignore_type_of_describe_name)
            .field("allow_arguments", &self.allow_arguments)
            .field("disallowed_words", &self.disallowed_words)
            .field("ignore_spaces", &self.ignore_spaces)
            .finish_non_exhaustive()
    }
}

impl Default for ValidTitleCompiled {
    fn default() -> Self {
        Self {
            ignore_type_of_test_name: false,
            ignore_type_of_describe_name: false,
            allow_arguments: false,
            disallowed_words: Vec::new(),
            ignore_spaces: false,
            must_not_match_patterns: FxHashMap::default(),
            must_match_patterns: FxHashMap::default(),
        }
    }
}

impl std::ops::Deref for ValidTitle {
    type Target = ValidTitleCompiled;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

declare_oxc_lint!(
    /// ### What it does
    ///
    /// Checks that the titles of Jest and Vitest blocks are valid.
    ///
    /// Titles must be:
    /// - not empty,
    /// - strings,
    /// - not prefixed with their block name,
    /// - have no leading or trailing spaces.
    ///
    /// ### Why is this bad?
    ///
    /// Titles that are not valid can be misleading and make it harder to understand the purpose of the test.
    ///
    /// ### Examples
    ///
    /// Examples of **incorrect** code for this rule:
    /// ```javascript
    /// describe('', () => {});
    /// describe('foo', () => {
    ///   it('', () => {});
    /// });
    /// it('', () => {});
    /// test('', () => {});
    /// xdescribe('', () => {});
    /// xit('', () => {});
    /// xtest('', () => {});
    /// ```
    /// Examples of **correct** code for this rule:
    /// ```javascript
    /// describe('foo', () => {});
    /// it('bar', () => {});
    /// test('baz', () => {});
    /// ```
    ///
    ValidTitle,
    jest,
    correctness,
    conditional_fix,
    config = ValidTitleConfig,
);

impl Rule for ValidTitle {
    fn from_configuration(value: serde_json::Value) -> Result<Self, serde_json::error::Error> {
        let raw = serde_json::from_value::<DefaultRuleConfig<ValidTitleConfig>>(value)
            .unwrap_or_default()
            .into_inner();

        let must_match_patterns =
            raw.must_match.as_ref().map(compile_matcher_config).unwrap_or_default();
        let must_not_match_patterns =
            raw.must_not_match.as_ref().map(compile_matcher_config).unwrap_or_default();

        Ok(Self(Box::new(ValidTitleCompiled {
            ignore_type_of_test_name: raw.ignore_type_of_test_name,
            ignore_type_of_describe_name: raw.ignore_type_of_describe_name,
            allow_arguments: raw.allow_arguments,
            disallowed_words: raw.disallowed_words.into_iter().map(CompactStr::from).collect(),
            ignore_spaces: raw.ignore_spaces,
            must_match_patterns,
            must_not_match_patterns,
        })))
    }

    fn run_on_jest_node<'a, 'c>(
        &self,
        jest_node: &PossibleJestNode<'a, 'c>,
        ctx: &'c LintContext<'a>,
    ) {
        self.run(jest_node, ctx);
    }
}

impl ValidTitle {
    fn run<'a>(&self, possible_jest_fn_node: &PossibleJestNode<'a, '_>, ctx: &LintContext<'a>) {
        let node = possible_jest_fn_node.node;
        let AstKind::CallExpression(call_expr) = node.kind() else {
            return;
        };
        let Some(jest_fn_call) = parse_general_jest_fn_call(call_expr, possible_jest_fn_node, ctx)
        else {
            return;
        };

        if !matches!(
            jest_fn_call.kind,
            JestFnKind::General(JestGeneralFnKind::Describe | JestGeneralFnKind::Test)
        ) {
            return;
        }

        // Check if extend keyword has been used (vitest feature)
        if let Some(member) = jest_fn_call.members.first()
            && member.is_name_equal("extend")
        {
            return;
        }

        let Some(arg) = call_expr.arguments.first() else {
            return;
        };

        // Handle typecheck settings - skip for describe when enabled (vitest feature)
        if ctx.settings().vitest.typecheck
            && matches!(jest_fn_call.kind, JestFnKind::General(JestGeneralFnKind::Describe))
        {
            return;
        }

        // Handle allowArguments option (vitest feature)
        if self.allow_arguments && matches!(arg, Argument::Identifier(_)) {
            return;
        }

        let need_report_name = match jest_fn_call.kind {
            JestFnKind::General(JestGeneralFnKind::Test) => !self.ignore_type_of_test_name,
            JestFnKind::General(JestGeneralFnKind::Describe) => !self.ignore_type_of_describe_name,
            _ => unreachable!(),
        };

        match arg {
            Argument::StringLiteral(string_literal) => {
                validate_title(
                    &string_literal.value,
                    string_literal.span,
                    self,
                    &jest_fn_call.name,
                    ctx,
                );
            }
            Argument::TemplateLiteral(template_literal) => {
                if let Some(quasi) = template_literal.single_quasi() {
                    validate_title(
                        quasi.as_str(),
                        template_literal.span,
                        self,
                        &jest_fn_call.name,
                        ctx,
                    );
                }
            }
            Argument::BinaryExpression(binary_expr) => {
                if does_binary_expression_contain_string_node(binary_expr) {
                    return;
                }
                if need_report_name {
                    ctx.diagnostic(title_must_be_string_diagnostic(arg.span()));
                }
            }
            _ => {
                if need_report_name {
                    ctx.diagnostic(title_must_be_string_diagnostic(arg.span()));
                }
            }
        }
    }
}

type CompiledMatcherAndMessage = (Regex, Option<CompactStr>);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum MatchKind {
    Describe,
    It,
    Test,
}

impl MatchKind {
    fn from(name: &str) -> Option<Self> {
        match name {
            "describe" => Some(Self::Describe),
            "it" => Some(Self::It),
            "test" => Some(Self::Test),
            _ => None,
        }
    }
}

/// Compile a JS-style regex string (e.g. `/pattern/flags` or `pattern`) into
/// a [`Regex`].  Flags are currently ignored.
fn compile_regex_str(s: &str) -> Option<Regex> {
    if let Some(stripped) = s.strip_prefix('/')
        && let Some(end) = stripped.rfind('/')
    {
        let (pat, _flags) = stripped.split_at(end);
        return Regex::new(pat).ok();
    }
    Regex::new(&format!("(?u){s}")).ok()
}

fn compile_matcher_pattern(pattern: &MatcherPattern) -> Option<CompiledMatcherAndMessage> {
    match pattern {
        MatcherPattern::String(s) => Some((compile_regex_str(s)?, None)),
        MatcherPattern::Array(v) => {
            let pattern_str = v.first()?;
            let regex = compile_regex_str(pattern_str)?;
            let message = v.get(1).map(|s| CompactStr::from(s.as_str()));
            Some((regex, message))
        }
    }
}

fn compile_matcher_config(
    config: &MatcherConfig,
) -> FxHashMap<MatchKind, CompiledMatcherAndMessage> {
    let mut map = FxHashMap::default();
    match config {
        MatcherConfig::Pattern(pattern) => {
            if let Some(compiled) = compile_matcher_pattern(pattern) {
                map.insert(MatchKind::Describe, compiled.clone());
                map.insert(MatchKind::Test, compiled.clone());
                map.insert(MatchKind::It, compiled);
            }
        }
        MatcherConfig::PerKind(per_kind) => {
            for (key, pattern) in per_kind {
                if let Some(kind) = MatchKind::from(key) {
                    if let Some(compiled) = compile_matcher_pattern(pattern) {
                        map.insert(kind, compiled);
                    }
                }
            }
        }
    }
    map
}

fn validate_title(
    title: &str,
    span: Span,
    valid_title: &ValidTitle,
    name: &str,
    ctx: &LintContext,
) {
    if title.is_empty() {
        ctx.diagnostic(empty_title_diagnostic(span));
        return;
    }

    if !valid_title.disallowed_words.is_empty() {
        let Ok(disallowed_words_reg) = Regex::new(&format!(
            r"(?iu)\b(?:{})\b",
            valid_title.disallowed_words.join("|").cow_replace('.', r"\.")
        )) else {
            return;
        };

        if let Some(matched) = disallowed_words_reg.find(title) {
            ctx.diagnostic(disallowed_word_diagnostic(matched.as_str(), span));
        }
        return;
    }

    let trimmed_title = title.trim();
    if !valid_title.ignore_spaces && trimmed_title != title {
        ctx.diagnostic_with_fix(accidental_space_diagnostic(span), |fixer| {
            let inner_span = span.shrink(1);
            let raw_text = fixer.source_range(inner_span);
            let trimmed_raw = raw_text.trim().to_string();
            fixer.replace(inner_span, trimmed_raw)
        });
    }

    let un_prefixed_name = name.trim_start_matches(['f', 'x']);
    let Some(first_word) = title.split(' ').next() else {
        return;
    };

    if first_word == un_prefixed_name {
        ctx.diagnostic_with_fix(duplicate_prefix_diagnostic(span), |fixer| {
            // Use raw source text to preserve escape sequences
            let inner_span = span.shrink(1);
            let raw_text = fixer.source_range(inner_span);
            // Find the first space in raw text to avoid byte offset issues
            // if the prefix word ever contains escapable characters
            let space_pos = raw_text.find(' ').unwrap_or(raw_text.len());
            let replaced_raw = raw_text[space_pos..].trim().to_string();
            fixer.replace(inner_span, replaced_raw)
        });
        return;
    }

    let Some(jest_fn_name) = MatchKind::from(un_prefixed_name) else {
        return;
    };

    if let Some((regex, message)) = valid_title.must_match_patterns.get(&jest_fn_name)
        && !regex.is_match(title)
    {
        let raw_pattern = regex.as_str();
        let message = match message.as_ref() {
            Some(message) => message.as_str(),
            None => &format!("{un_prefixed_name} should match {raw_pattern}"),
        };
        ctx.diagnostic(must_match_diagnostic(message, span));
    }

    if let Some((regex, message)) = valid_title.must_not_match_patterns.get(&jest_fn_name)
        && regex.is_match(title)
    {
        let raw_pattern = regex.as_str();
        let message = match message.as_ref() {
            Some(message) => message.as_str(),
            None => &format!("{un_prefixed_name} should not match {raw_pattern}"),
        };

        ctx.diagnostic(must_not_match_diagnostic(message, span));
    }
}

fn does_binary_expression_contain_string_node(expr: &BinaryExpression) -> bool {
    if expr.left.is_string_literal() || expr.right.is_string_literal() {
        return true;
    }

    match &expr.left {
        Expression::BinaryExpression(left) => does_binary_expression_contain_string_node(left),
        _ => false,
    }
}

#[test]
fn test() {
    use crate::tester::Tester;

    let pass = vec![
        ("describe('the correct way to properly handle all the things', () => {});", None),
        ("test('that all is as it should be', () => {});", None),
        (
            "it('correctly sets the value', () => {});",
            Some(serde_json::json!([
              { "ignoreTypeOfDescribeName": false, "disallowedWords": ["correct"] },
            ])),
        ),
        ("it('correctly sets the value', () => {});", Some(serde_json::json!([]))),
        ("describe('the correct way to properly handle all the things', () => {});", None),
        ("test('that all is as it should be', () => {});", None),
        (
            "it('correctly sets the value', () => {});",
            Some(serde_json::json!([{ "mustMatch": {} }])),
        ),
        (
            "it('correctly sets the value', () => {});",
            Some(serde_json::json!([{ "mustMatch": " " }])),
        ),
        (
            "it('correctly sets the value', () => {});",
            Some(serde_json::json!([{ "mustMatch": [" "] }])),
        ),
        (
            "it('correctly sets the value #unit', () => {});",
            Some(serde_json::json!([{ "mustMatch": "#(?:unit|integration|e2e)" }])),
        ),
        (
            "it('correctly sets the value', () => {});",
            Some(serde_json::json!([{ "mustMatch": "^[^#]+$|(?:#(?:unit|e2e))" }])),
        ),
        (
            "it('correctly sets the value', () => {});",
            Some(serde_json::json!([{ "mustMatch": { "test": "#(?:unit|integration|e2e)" } }])),
        ),
        (
            "
            describe('things to test', () => {
                describe('unit tests #unit', () => {
                it('is true', () => {
                    expect(true).toBe(true);
                });
                });

                describe('e2e tests #e2e', () => {
                it('is another test #jest4life', () => {});
                });
            });
            ",
            Some(serde_json::json!([{ "mustMatch": { "test": "^[^#]+$|(?:#(?:unit|e2e))" } }])),
        ),
        ("it('is a string', () => {});", None),
        ("it('is' + ' a ' + ' string', () => {});", None),
        ("it(1 + ' + ' + 1, () => {});", None),
        ("test('is a string', () => {});", None),
        ("xtest('is a string', () => {});", None),
        ("xtest(`${myFunc} is a string`, () => {});", None),
        ("describe('is a string', () => {});", None),
        ("describe.skip('is a string', () => {});", None),
        ("describe.skip(`${myFunc} is a string`, () => {});", None),
        ("fdescribe('is a string', () => {});", None),
        (
            "describe(String(/.+/), () => {});",
            Some(serde_json::json!([{ "ignoreTypeOfDescribeName": true }])),
        ),
        (
            "describe(myFunction, () => {});",
            Some(serde_json::json!([{ "ignoreTypeOfDescribeName": true }])),
        ),
        (
            "xdescribe(skipFunction, () => {});",
            Some(serde_json::json!([{ "ignoreTypeOfDescribeName": true, "disallowedWords": [] }])),
        ),
        ("describe()", None),
        ("someFn('', function () {})", None),
        ("describe('foo', function () {})", None),
        ("describe('foo', function () { it('bar', function () {}) })", None),
        ("test('foo', function () {})", None),
        ("test.concurrent('foo', function () {})", None),
        ("test(`foo`, function () {})", None),
        ("test.concurrent(`foo`, function () {})", None),
        ("test(`${foo}`, function () {})", None),
        ("test.concurrent(`${foo}`, function () {})", None),
        ("it('foo', function () {})", None),
        ("it.each([])()", None),
        ("it.concurrent('foo', function () {})", None),
        ("xdescribe('foo', function () {})", None),
        ("xit('foo', function () {})", None),
        ("xtest('foo', function () {})", None),
        ("it()", None),
        ("it.concurrent()", None),
        ("describe()", None),
        ("it.each()()", None),
        ("describe('foo', function () {})", None),
        ("fdescribe('foo', function () {})", None),
        ("xdescribe('foo', function () {})", None),
        ("it('foo', function () {})", None),
        ("it.concurrent('foo', function () {})", None),
        ("fit('foo', function () {})", None),
        ("fit.concurrent('foo', function () {})", None),
        ("xit('foo', function () {})", None),
        ("test('foo', function () {})", None),
        ("test.concurrent('foo', function () {})", None),
        ("xtest('foo', function () {})", None),
        ("xtest(`foo`, function () {})", None),
        ("someFn('foo', function () {})", None),
        (
            "
                describe('foo', () => {
                it('bar', () => {})
                })
            ",
            None,
        ),
        (
            "it(`GIVEN...
            `, () => {});",
            Some(serde_json::json!([{ "ignoreSpaces": true }])),
        ),
        ("describe('foo', function () {})", None),
        ("fdescribe('foo', function () {})", None),
        ("xdescribe('foo', function () {})", None),
        ("xdescribe(`foo`, function () {})", None),
        ("test('foo', function () {})", None),
        ("test('foo', function () {})", None),
        ("xtest('foo', function () {})", None),
        ("xtest(`foo`, function () {})", None),
        ("test('foo test', function () {})", None),
        ("xtest('foo test', function () {})", None),
        ("it('foo', function () {})", None),
        ("fit('foo', function () {})", None),
        ("xit('foo', function () {})", None),
        ("xit(`foo`, function () {})", None),
        ("it('foos it correctly', function () {})", None),
        (
            "
                describe('foo', () => {
                it('bar', () => {})
                })
            ",
            None,
        ),
        (
            "
                describe('foo', () => {
                it('describes things correctly', () => {})
                })
            ",
            None,
        ),
        ("it(abc, function () {})", Some(serde_json::json!([{ "ignoreTypeOfTestName": true }]))),
        // Vitest-specific tests with allowArguments option
        ("it(foo, () => {});", Some(serde_json::json!([{ "allowArguments": true }]))),
        ("describe(bar, () => {});", Some(serde_json::json!([{ "allowArguments": true }]))),
        ("test(baz, () => {});", Some(serde_json::json!([{ "allowArguments": true }]))),
        // Vitest-specific tests with .extend()
        (
            "export const myTest = test.extend({
                archive: []
            })",
            None,
        ),
        ("const localTest = test.extend({})", None),
        (
            "import { it } from 'vitest'

            const test = it.extend({
                fixture: [
                    async ({}, use) => {
                        setup()
                        await use()
                        teardown()
                    },
                    { auto: true }
                ],
            })

            test('', () => {})",
            None,
        ),
    ];

    let fail = vec![
        (
            "test('the correct way to properly handle all things', () => {});",
            Some(serde_json::json!([{ "disallowedWords": ["correct", "properly", "all"] }])),
        ),
        (
            "describe('the correct way to do things', function () {})",
            Some(serde_json::json!([{ "disallowedWords": ["correct"] }])),
        ),
        (
            "it('has ALL the things', () => {})",
            Some(serde_json::json!([{ "disallowedWords": ["all"] }])),
        ),
        (
            "xdescribe('every single one of them', function () {})",
            Some(serde_json::json!([{ "disallowedWords": ["every"] }])),
        ),
        (
            "describe('Very Descriptive Title Goes Here', function () {})",
            Some(serde_json::json!([{ "disallowedWords": ["descriptive"] }])),
        ),
        (
            "test(`that the value is set properly`, function () {})",
            Some(serde_json::json!([{ "disallowedWords": ["properly"] }])),
        ),
        // TODO: The regex `(?:#(?!unit|e2e))\w+` in those test cases is not valid in Rust
        // (
        //     "
        //         describe('things to test', () => {
        //             describe('unit tests #unit', () => {
        //                 it('is true', () => {
        //                     expect(true).toBe(true);
        //                 });
        //             });

        //             describe('e2e tests #e4e', () => {
        //                 it('is another test #e2e #jest4life', () => {});
        //             });
        //         });
        //     ",
        //     Some(serde_json::json!([
        //         {
        //             "mustNotMatch": r#"(?:#(?!unit|e2e))\w+"#,
        //             "mustMatch": "^[^#]+$|(?:#(?:unit|e2e))",
        //         },
        //     ])),
        // ),
        // (
        //     "
        //         import { describe, describe as context, it as thisTest } from '@jest/globals';

        //         describe('things to test', () => {
        //             context('unit tests #unit', () => {
        //             thisTest('is true', () => {
        //                 expect(true).toBe(true);
        //             });
        //             });

        //             context('e2e tests #e4e', () => {
        //                 thisTest('is another test #e2e #jest4life', () => {});
        //             });
        //         });
        //         ",
        //     Some(
        //         serde_json::json!([ { "mustNotMatch": r#"(?:#(?!unit|e2e))\w+"#, "mustMatch": "^[^#]+$|(?:#(?:unit|e2e))", }, ]),
        //     ),
        // ),
        // (
        //     "
        //         describe('things to test', () => {
        //             describe('unit tests #unit', () => {
        //                 it('is true', () => {
        //                     expect(true).toBe(true);
        //                 });
        //             });

        //             describe('e2e tests #e4e', () => {
        //                 it('is another test #e2e #jest4life', () => {});
        //             });
        //         });
        //     ",
        //     Some(serde_json::json!([
        //       {
        //         "mustNotMatch": [
        //           r#"(?:#(?!unit|e2e))\w+"#,
        //           "Please include '#unit' or '#e2e' in titles",
        //         ],
        //         "mustMatch": [
        //           "^[^#]+$|(?:#(?:unit|e2e))",
        //           "Please include '#unit' or '#e2e' in titles",
        //         ],
        //       },
        //     ])),
        // ),
        // (
        //     "
        //         describe('things to test', () => {
        //             describe('unit tests #unit', () => {
        //                 it('is true', () => {
        //                     expect(true).toBe(true);
        //                 });
        //             });

        //             describe('e2e tests #e4e', () => {
        //                 it('is another test #e2e #jest4life', () => {});
        //             });
        //         });
        //     ",
        //     Some(serde_json::json!([
        //       {
        //         "mustNotMatch": { "describe": [r#"(?:#(?!unit|e2e))\w+"#] },
        //         "mustMatch": { "describe": "^[^#]+$|(?:#(?:unit|e2e))" },
        //       },
        //     ])),
        // ),
        // (
        //     "
        //         describe('things to test', () => {
        //             describe('unit tests #unit', () => {
        //             it('is true', () => {
        //                 expect(true).toBe(true);
        //             });
        //             });

        //             describe('e2e tests #e4e', () => {
        //                 it('is another test #e2e #jest4life', () => {});
        //             });
        //         });
        //     ",
        //     Some(serde_json::json!([
        //       {
        //         "mustNotMatch": { "describe": r#"(?:#(?!unit|e2e))\w+"# },
        //         "mustMatch": { "it": "^[^#]+$|(?:#(?:unit|e2e))" },
        //       },
        //     ])),
        // ),
        // (
        //     "
        //         describe('things to test', () => {
        //             describe('unit tests #unit', () => {
        //             it('is true #jest4life', () => {
        //                 expect(true).toBe(true);
        //             });
        //             });

        //             describe('e2e tests #e4e', () => {
        //             it('is another test #e2e #jest4life', () => {});
        //             });
        //         });
        //     ",
        //     Some(serde_json::json!([
        //       {
        //         "mustNotMatch": {
        //           "describe": [
        //             r#"(?:#(?!unit|e2e))\w+"#,
        //             "Please include '#unit' or '#e2e' in describe titles",
        //           ],
        //         },
        //         "mustMatch": {
        //           "it": [
        //             "^[^#]+$|(?:#(?:unit|e2e))",
        //             "Please include '#unit' or '#e2e' in it titles",
        //           ],
        //         },
        //       },
        //     ])),
        // ),
        (
            "test('the correct way to properly handle all things', () => {});",
            Some(serde_json::json!([{ "mustMatch": "#(?:unit|integration|e2e)" }])),
        ),
        (
            "describe('the test', () => {});",
            Some(serde_json::json!([
              { "mustMatch": { "describe": "#(?:unit|integration|e2e)" } },
            ])),
        ),
        (
            "xdescribe('the test', () => {});",
            Some(serde_json::json!([
              { "mustMatch": { "describe": "#(?:unit|integration|e2e)" } },
            ])),
        ),
        (
            "describe.skip('the test', () => {});",
            Some(serde_json::json!([
              { "mustMatch": { "describe": "#(?:unit|integration|e2e)" } },
            ])),
        ),
        ("it.each([])(1, () => {});", None),
        ("it.skip.each([])(1, () => {});", None),
        ("it.skip.each``(1, () => {});", None),
        ("it(123, () => {});", None),
        ("it.concurrent(123, () => {});", None),
        ("it(1 + 2 + 3, () => {});", None),
        ("it.concurrent(1 + 2 + 3, () => {});", None),
        (
            "test.skip(123, () => {});",
            Some(serde_json::json!([{ "ignoreTypeOfDescribeName": true }])),
        ),
        ("describe(String(/.+/), () => {});", None),
        (
            "describe(myFunction, () => 1);",
            Some(serde_json::json!([{ "ignoreTypeOfDescribeName": false }])),
        ),
        ("describe(myFunction, () => {});", None),
        ("xdescribe(myFunction, () => {});", None),
        ("describe(6, function () {})", None),
        ("describe.skip(123, () => {});", None),
        ("describe('', function () {})", None),
        (
            "
                describe('foo', () => {
                    it('', () => {});
                });
            ",
            None,
        ),
        ("it('', function () {})", None),
        ("it.concurrent('', function () {})", None),
        ("test('', function () {})", None),
        ("test.concurrent('', function () {})", None),
        ("test(``, function () {})", None),
        ("test.concurrent(``, function () {})", None),
        ("xdescribe('', () => {})", None),
        ("xit('', () => {})", None),
        ("xtest('', () => {})", None),
        ("describe(' foo', function () {})", None),
        ("describe.each()(' foo', function () {})", None),
        ("describe.only.each()(' foo', function () {})", None),
        ("describe(' foo foe fum', function () {})", None),
        ("describe('foo foe fum ', function () {})", None),
        ("fdescribe(' foo', function () {})", None),
        ("fdescribe(' foo', function () {})", None),
        ("xdescribe(' foo', function () {})", None),
        ("it(' foo', function () {})", None),
        ("it.concurrent(' foo', function () {})", None),
        ("fit(' foo', function () {})", None),
        ("it.skip(' foo', function () {})", None),
        ("fit('foo ', function () {})", None),
        ("it.skip('foo ', function () {})", None),
        (
            "
                import { test as testThat } from '@jest/globals';

                testThat('foo works ', () => {});
            ",
            None,
        ),
        ("xit(' foo', function () {})", None),
        ("test(' foo', function () {})", None),
        ("test.concurrent(' foo', function () {})", None),
        ("test(` foo`, function () {})", None),
        ("test.concurrent(` foo`, function () {})", None),
        ("test(` foo bar bang`, function () {})", None),
        ("test.concurrent(` foo bar bang`, function () {})", None),
        ("test(` foo bar bang  `, function () {})", None),
        ("test.concurrent(` foo bar bang  `, function () {})", None),
        ("xtest(' foo', function () {})", None),
        ("xtest(' foo  ', function () {})", None),
        (
            "
                describe(' foo', () => {
                    it('bar', () => {})
                })
            ",
            None,
        ),
        (
            "
                describe('foo', () => {
                    it(' bar', () => {})
                })
            ",
            None,
        ),
        ("describe('describe foo', function () {})", None),
        ("fdescribe('describe foo', function () {})", None),
        ("xdescribe('describe foo', function () {})", None),
        ("describe('describe foo', function () {})", None),
        ("fdescribe(`describe foo`, function () {})", None),
        ("test('test foo', function () {})", None),
        ("xtest('test foo', function () {})", None),
        ("test(`test foo`, function () {})", None),
        ("test(`test foo test`, function () {})", None),
        ("it('it foo', function () {})", None),
        ("fit('it foo', function () {})", None),
        ("xit('it foo', function () {})", None),
        ("it('it foos it correctly', function () {})", None),
        (
            "
                describe('describe foo', () => {
                    it('bar', () => {})
                })
            ",
            None,
        ),
        (
            "
                describe('describe foo', () => {
                    it('describes things correctly', () => {})
                })
            ",
            None,
        ),
        (
            "
                describe('foo', () => {
                    it('it bar', () => {})
                })
            ",
            None,
        ),
        ("it(abc, function () {})", None),
        // Vitest-specific fail test with allowArguments: false
        ("test(bar, () => {});", Some(serde_json::json!([{ "allowArguments": false }]))),
    ];

    let fix = vec![
        ("describe(' foo', function () {})", "describe('foo', function () {})"),
        ("describe.each()(' foo', function () {})", "describe.each()('foo', function () {})"),
        (
            "describe.only.each()(' foo', function () {})",
            "describe.only.each()('foo', function () {})",
        ),
        ("describe(' foo foe fum', function () {})", "describe('foo foe fum', function () {})"),
        ("describe('foo foe fum ', function () {})", "describe('foo foe fum', function () {})"),
        ("fdescribe(' foo', function () {})", "fdescribe('foo', function () {})"),
        ("fdescribe(' foo', function () {})", "fdescribe('foo', function () {})"),
        ("xdescribe(' foo', function () {})", "xdescribe('foo', function () {})"),
        ("it(' foo', function () {})", "it('foo', function () {})"),
        ("it.concurrent(' foo', function () {})", "it.concurrent('foo', function () {})"),
        ("fit(' foo', function () {})", "fit('foo', function () {})"),
        ("it.skip(' foo', function () {})", "it.skip('foo', function () {})"),
        ("fit('foo ', function () {})", "fit('foo', function () {})"),
        ("it.skip('foo ', function () {})", "it.skip('foo', function () {})"),
        (
            "
                import { test as testThat } from '@jest/globals';

                testThat('foo works ', () => {});
            ",
            "
                import { test as testThat } from '@jest/globals';

                testThat('foo works', () => {});
            ",
        ),
        ("xit(' foo', function () {})", "xit('foo', function () {})"),
        ("test(' foo', function () {})", "test('foo', function () {})"),
        ("test.concurrent(' foo', function () {})", "test.concurrent('foo', function () {})"),
        ("test(` foo`, function () {})", "test(`foo`, function () {})"),
        ("test.concurrent(` foo`, function () {})", "test.concurrent(`foo`, function () {})"),
        ("test(` foo bar bang`, function () {})", "test(`foo bar bang`, function () {})"),
        (
            "test.concurrent(` foo bar bang`, function () {})",
            "test.concurrent(`foo bar bang`, function () {})",
        ),
        ("test(` foo bar bang  `, function () {})", "test(`foo bar bang`, function () {})"),
        (
            "test.concurrent(` foo bar bang  `, function () {})",
            "test.concurrent(`foo bar bang`, function () {})",
        ),
        ("xtest(' foo', function () {})", "xtest('foo', function () {})"),
        ("xtest(' foo  ', function () {})", "xtest('foo', function () {})"),
        (
            "
                describe(' foo', () => {
                    it('bar', () => {})
                })
            ",
            "
                describe('foo', () => {
                    it('bar', () => {})
                })
            ",
        ),
        (
            "
                describe('foo', () => {
                    it(' bar', () => {})
                })
            ",
            "
                describe('foo', () => {
                    it('bar', () => {})
                })
            ",
        ),
        ("describe('describe foo', function () {})", "describe('foo', function () {})"),
        ("fdescribe('describe foo', function () {})", "fdescribe('foo', function () {})"),
        ("xdescribe('describe foo', function () {})", "xdescribe('foo', function () {})"),
        ("describe('describe foo', function () {})", "describe('foo', function () {})"),
        ("fdescribe(`describe foo`, function () {})", "fdescribe(`foo`, function () {})"),
        ("test('test foo', function () {})", "test('foo', function () {})"),
        ("xtest('test foo', function () {})", "xtest('foo', function () {})"),
        ("test(`test foo`, function () {})", "test(`foo`, function () {})"),
        ("test(`test foo test`, function () {})", "test(`foo test`, function () {})"),
        ("it('it foo', function () {})", "it('foo', function () {})"),
        ("fit('it foo', function () {})", "fit('foo', function () {})"),
        ("xit('it foo', function () {})", "xit('foo', function () {})"),
        ("it('it foos it correctly', function () {})", "it('foos it correctly', function () {})"),
        (
            "
                describe('describe foo', () => {
                    it('bar', () => {})
                })
            ",
            "
                describe('foo', () => {
                    it('bar', () => {})
                })
            ",
        ),
        (
            "
                describe('describe foo', () => {
                    it('describes things correctly', () => {})
                })
            ",
            "
                describe('foo', () => {
                    it('describes things correctly', () => {})
                })
            ",
        ),
        (
            "
                describe('foo', () => {
                    it('it bar', () => {})
                })
            ",
            "
                describe('foo', () => {
                    it('bar', () => {})
                })
            ",
        ),
        // AccidentalSpace: preserve escape sequences when trimming spaces
        (
            "test('issue #225513: Cmd-Click doesn\\'t work on JSDoc {@link URL|LinkText} format ', () => { assert(true); });",
            "test('issue #225513: Cmd-Click doesn\\'t work on JSDoc {@link URL|LinkText} format', () => { assert(true); });",
        ),
        // DuplicatePrefix: preserve escape sequences when removing prefix
        (
            "test('test that it doesn\\'t break', () => {});",
            "test('that it doesn\\'t break', () => {});",
        ),
    ];

    Tester::new(ValidTitle::NAME, ValidTitle::PLUGIN, pass, fail)
        .with_jest_plugin(true)
        .expect_fix(fix)
        .test_and_snapshot();
}
