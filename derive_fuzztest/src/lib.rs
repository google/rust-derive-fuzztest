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

//! Derive macros that generates both a fuzz target for use with `cargo fuzz`, and a `proptest` for
//! use with `cargo test`.
//!
//! The reason for having both is that `proptest` allows for quick iteration to make sure the test
//! works, while fuzzing can test the input space more exhaustively and run continuously.
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

#[cfg(feature = "quickcheck")]
#[doc(hidden)]
pub mod arbitrary_bridge {

    /// Wrapper type that allows `arbitrary::Arbitrary` to be used as `quickcheck::Arbitrary`
    #[derive(Debug, Clone)]
    pub struct ArbitraryAdapter<T: for<'a> arbitrary::Arbitrary<'a>>(
        pub Result<T, arbitrary::Error>,
    );

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
