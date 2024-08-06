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

use arbitrary::Arbitrary;
use derive_fuzztest::fuzztest;

#[derive(Arbitrary, Clone, Debug)]
struct UnitStruct;

#[derive(Arbitrary, Clone, Debug)]
struct TestStruct {
    field1: u8,
    field2: u8,
}

#[derive(Arbitrary, Clone, Debug)]
enum Either {
    A { a: u8 },
    B { b: u8 },
}

/// Test case to make sure the code generation works for unusual patterns in the function
/// parameters.
#[fuzztest]
fn test(
    UnitStruct: UnitStruct,
    TestStruct { field1, mut field2 }: TestStruct,
    (Either::A { a } | Either::B { b: a }): Either,
    (a2 @ Either::A { a: _ } | a2 @ Either::B { .. }): Either,
    [i1, i2, i3, ..]: [u8; 12],
) {
}
