(**
 * libjass-fs
 *
 * https://github.com/Arnavion/libjass-fs
 *
 * Copyright 2015 Arnav Singh
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

#light

[<FunScript.JS>]
module platform

// Deliberately not private, so that it can have a custom JS implementation
let StringToHexInt32 (isNegative: bool) (str: string): int option =
    try
        (System.Convert.ToInt64(str, 16))
        |> fun value -> if isNegative then -value else value
        |> fun value ->
            if value < int64 System.Int32.MinValue then System.Int32.MinValue
            else if value > int64 System.Int32.MaxValue then System.Int32.MaxValue
            else int value
        |> Some
    with
    | :? System.OverflowException -> Some 0xFFFFFFFF
    | _ -> None
