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
module libjass.streams

open FSharp.Control

let inline reassemble (func: 'T -> ('T * 'T) option) (initial: 'T) (sequence: AsyncSeq<'T>): AsyncSeq<'T> =
    let enumerator = sequence.GetEnumerator()

    let rec pullNext state =
        asyncSeq {
            let! next = enumerator.MoveNext()
            match next with
            | Some element ->
                yield! pushNext (state + element)
            | None ->
                yield state
        }
    and pushNext state =
        asyncSeq {
            let next = func state
            match next with
            | Some (result, state) ->
                yield result
                yield! pushNext state
            | None ->
                yield! pullNext state
        }

    pushNext initial

let getLines (stream: AsyncSeq<string>): AsyncSeq<string> =
    stream
    |> reassemble
        (fun chunk ->
            match chunk.IndexOf('\n') with
            | -1 -> None
            | index ->
                if chunk.[index - 1] = '\r' then
                    Some (chunk.[0..index - 2], chunk.[index + 1..])
                else
                    Some (chunk.[0..index - 1], chunk.[index + 1..])
        )
        ""
