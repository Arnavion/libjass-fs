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

module platform

module Managed =
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


[<FunScript.JS>]
module Native =
    [<FunScript.JSEmitInline("{0}.substring({1}, {2})")>]
    let private String_Substring_Static (source: string) (start: int) (finish: int): string = failwith "never"

    let OperatorIntrinsics_GetStringSlice (source: string) (start: int option) (finish: int option): string =
        String_Substring_Static source (defaultArg start 0) ((defaultArg finish (source.Length - 1)) + 1)

    [<FunScript.JSEmitInline("{0}.charCodeAt(0)")>]
    let Convert_ToInt32 (value: char): int = failwith "never"

    [<FunScript.JSEmitInline("parseInt({0}, {1})")>]
    let private parseInt (str: string) (``base``: int): int = failwith "never"

    let StringToHexInt32 (isNegative: bool) (str: string): int option =
        if str.Length > 32 / 4 then
            if isNegative then Some System.Int32.MinValue else Some System.Int32.MaxValue
        else
            let result = parseInt ((if isNegative then "-" else "") + str) 16
            if result < System.Int32.MinValue then Some System.Int32.MinValue
            else if result > System.Int32.MaxValue then Some System.Int32.MaxValue
            else Some result

    [<FunScript.JSEmitInline("{0}")>]
    let Operator_enum<'U> (value: int32): 'U = failwith "never"

    [<FunScript.JSEmitInline("{1}.has({0})")>]
    let Set_contains (element: 'T) (set: Set<'T>): bool = failwith "never"

    [<FunScript.JSEmitInline("new Set({0})")>]
    let Set_ofArray (array: 'a array): Set<'a> = failwith "never"
