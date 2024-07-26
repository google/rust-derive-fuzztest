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

#![cfg_attr(fuzzing, no_main)]
#![no_std]

use derive_fuzztest::{fuzztest, TestResult};

#[fuzztest]
pub fn test(a: [u8; 5]) -> TestResult {
    if a[0].is_ascii_lowercase() {
        TestResult::Passed
    } else {
        // Discard any test results with first character not a lowercase ASCII.
        // To verify:
        // - For fuzzing, you can verify that by opening the `corpus` directory.
        // - For proptest, `PROPTEST_MAX_GLOBAL_REJECTS=1 cargo test` will fail.
        // - For quickcheck, `QUICKCHECK_MIN_TESTS_PASSED=1024 cargo test` will fail.
        TestResult::Discard
    }
}
