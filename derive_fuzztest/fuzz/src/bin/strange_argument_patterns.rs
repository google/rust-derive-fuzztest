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

use derive_fuzztest::fuzztest;

/// Test case to make sure the code generation works for unusual patterns in the function
/// parameters.
#[fuzztest]
#[allow(clippy::too_many_arguments)]
fn test(
    a: u8,
    mut b: u8,
    (ref c, mut d, _): (u8, u8, u8),
    r#try: (),
    0..: u8,
    0..=255: u8,
    (..): (),
    (..): (u8, u8, u8),
) {
}
