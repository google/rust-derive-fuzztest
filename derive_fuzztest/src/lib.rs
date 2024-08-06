// Copyright 2024 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! Derive macros that generates both a fuzz target for use with `cargo fuzz`, and a property test
//! (via `quickcheck` or `proptest`) for use with `cargo test`.
//!
//! The reason for having both is that property testing allows for quick iteration to make sure the
//! test works, and can be checked in presubmit CI, while fuzzing can test the input space more
//! exhaustively and run continuously.
//!
//! # Example
//!
//! ```no_run
//! #![cfg_attr(fuzzing, no_main)]
//!
//! #[derive_fuzztest::fuzztest]
//! fn transitive_ord(a: u32, b: u32, c: u32) {
//!     if a >= b && b >= c {
//!         assert!(a >= c);
//!     }
//!     if a <= b && b <= c {
//!         assert!(a <= c);
//!     }
//! }
//!
//! #[test]
//! fn additional_test_here() {
//!     /* ... */
//! }
//! ```
//!
//! # Result reporting
//!
//! Test functions report test failures by panicking, similar to how you would write a regular
//! `#[test]`. If the annotated test function completes without panicking, the test is considered to
//! have passed.
//!
//! In some cases, you may want to discard some inputs, treating them neither as passes nor
//! failures, just continue onto testing another generated input. This can be done by returning a
//! [`TestResult`] from the annotated function. Property testing frameworks will try to generate
//! more test cases to replace the discarded one, up to a certain limit. For fuzzing, test results
//! that are discarded will not be added to the corpus.
//!
//! ```
//! use derive_fuzztest::TestResult;
//!
//! #[derive_fuzztest::fuzztest]
//! fn increment(a: u8) -> TestResult {
//!     if a < u8::MAX {
//!         assert_eq!(a + 1 - 1, a);
//!         TestResult::Passed
//!     } else {
//!         TestResult::Discard
//!     }
//! }
//! ```
//!
//! # Usage
//!
//!
//! Run the generated property tests
//! ```sh
//! cargo test
//! ```
//!
//! Run continuous fuzzing
//! ```sh
//! cargo +nightly fuzz run <binary name>
//! ```
//!
//! # Crate structure
//!
//! If you use `#[fuzz]` or `#[fuzztest]`, the fuzz target imposes the following requirements:
//!
//! * The target must be in a separate `[[bin]]` target that only contains a single fuzz target.
//! * The crate containing the bin target has `[package.metadata] cargo-fuzz = true`
//! * The bin target is annotated with `#![cfg_attr(fuzzing, no_main)]`
//!
//! The recommended structure for your crate `foo` is to put your tests under `foo/fuzz/src/bin`:
//!
//! ```text
//! foo
//! ├── fuzz
//! │   ├── src
//! │   │   └── bin
//! │   │       └── fuzz_target_1.rs
//! │   └── Cargo.toml
//! ├── src
//! │   └── [project source]
//! └── Cargo.toml
//! ```
//!
//! This is different from the default structure generated by `cargo fuzz init` or `cargo fuzz add`
//! so that we can take advantage of [target
//! auto-discovery](https://doc.rust-lang.org/cargo/reference/cargo-targets.html#target-auto-discovery).
//! If you prefer, the default structure generated by `cargo fuzz` can also work, but make sure you
//! remove `test = false` from the generated target in `Cargo.toml`.
//!
//! You will also need to declare a dependency on the `libfuzzer-sys` crate, but only if fuzzing is
//! requested:
//!
//! ```toml
//! [target.'cfg(fuzzing)'.dependencies]
//! libfuzzer-sys = "*"
//! ```
//!
//! (The reason for this conditional dependency is that `libfuzzer-sys` injects a main function to
//! the resulting binary, and there will be linking failures if we link that in without defining a
//! corresponding `fuzz_target`.)
//!
//! # Features
//!
//! * `quickcheck` (default) — Enable generation of
//!   [`quickcheck`](https://docs.rs/quickcheck/latest/quickcheck/) property tests.
//! * `proptest` — Enable generation of [`proptest`](https://docs.rs/proptest/latest/proptest/)
//!   property tests.
//!
//! #### See also
//! * [Announcing Better Support for Fuzzing with Structured Inputs in
//!   Rust](https://fitzgeraldnick.com/2020/01/16/better-support-for-fuzzing-structured-inputs-in-rust.html#how-is-all-this-different-from-quickcheck-and-proptest)
//! * [Bridging Fuzzing and Property
//!   Testing](https://blog.yoshuawuyts.com/bridging-fuzzing-and-property-testing/)

pub use derive_fuzztest_macro::{fuzz, fuzztest, proptest};

#[doc(hidden)]
pub mod reexport {
    #[cfg(feature = "proptest")]
    pub use proptest;
    #[cfg(feature = "proptest")]
    pub use proptest_arbitrary_interop;
    #[cfg(feature = "quickcheck")]
    pub use quickcheck;
}

/// A test result reported from the test case.
///
/// A test case annotated with `#[fuzztest]`, `#[fuzz]`, or `#[proptest]` can optionally return a
/// `TestResult` to indicate whether a test should be treated as discarded or passed.
///
/// If the test does not have a return value, all non-panicking test cases default to passed.
pub enum TestResult {
    /// Indicates that a test passed.
    Passed,
    /// Indicates that a test should be discarded.
    ///
    /// For property testing, discarding a test result will cause `quickcheck` or `proptest` to try
    /// to generate more test cases to find non-discarded ones, up to a certain limit set by the
    /// framework. This corresponds to [`quickcheck::TestResult::discard`] and
    /// [`proptest::test_runner::TestCaseError::Reject`](https://docs.rs/proptest/latest/proptest/test_runner/enum.TestCaseError.html#variant.Reject).
    ///
    /// For fuzzing, a discarded result will not be added to the corpus, corresponding to
    /// [`libfuzzer_sys::Corpus::Reject`](https://docs.rs/libfuzzer-sys/latest/libfuzzer_sys/enum.Corpus.html#variant.Reject).
    Discard,
}

impl From<()> for TestResult {
    fn from(_: ()) -> Self {
        TestResult::Passed
    }
}

#[cfg(fuzzing)]
impl From<TestResult> for ::libfuzzer_sys::Corpus {
    fn from(test_result: TestResult) -> Self {
        match test_result {
            TestResult::Passed => Self::Keep,
            TestResult::Discard => Self::Reject,
        }
    }
}

#[cfg(feature = "proptest")]
impl From<TestResult> for proptest::test_runner::TestCaseResult {
    fn from(value: TestResult) -> Self {
        use proptest::{prelude::TestCaseError, test_runner::TestCaseResult};
        match value {
            TestResult::Passed => TestCaseResult::Ok(()),
            TestResult::Discard => {
                TestCaseResult::Err(TestCaseError::reject("Discarded by test case"))
            }
        }
    }
}

#[cfg(feature = "quickcheck")]
impl From<TestResult> for quickcheck::TestResult {
    fn from(value: TestResult) -> Self {
        match value {
            TestResult::Passed => quickcheck::TestResult::passed(),
            TestResult::Discard => quickcheck::TestResult::discard(),
        }
    }
}

#[cfg(feature = "quickcheck")]
#[doc(hidden)]
pub mod arbitrary_bridge {
    use std::fmt::{Debug, Formatter};

    /// Wrapper type that allows `arbitrary::Arbitrary` to be used as `quickcheck::Arbitrary`
    #[derive(Clone)]
    pub struct ArbitraryAdapter<T: for<'a> arbitrary::Arbitrary<'a>>(
        pub Result<T, arbitrary::Error>,
    );

    impl<T> Debug for ArbitraryAdapter<T>
    where
        T: for<'a> arbitrary::Arbitrary<'a> + Debug,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match &self.0 {
                Ok(v) => v.fmt(f),
                Err(e) => e.fmt(f),
            }
        }
    }

    impl<T> quickcheck::Arbitrary for ArbitraryAdapter<T>
    where
        T: for<'a> arbitrary::Arbitrary<'a> + Clone + 'static,
    {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            let bytes = Vec::<u8>::arbitrary(g);
            let mut unstructured = arbitrary::Unstructured::new(&bytes);
            Self(T::arbitrary(&mut unstructured))
        }
    }
}
