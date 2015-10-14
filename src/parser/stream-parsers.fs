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


let private (|Line|_|) (str: string): (string * string) option =
    let newLineIndex = str.IndexOf('\n')
    if newLineIndex = -1
    then None
    else
        let line = str.[0..newLineIndex - 1]
        if line.EndsWith("\r") then
            Some (line.[0..line.Length - 2], str.[newLineIndex + 1..])
        else
            Some (line, str.[newLineIndex + 1..])

let rec private nextLine_rec (stream: IAsyncEnumerator<string>) (buffer: string): AsyncSeq<string * string> =
    asyncSeq {
        match buffer with
        | Line (line, rest) -> yield line, rest
        | rest ->
            let! nextChunk = stream.MoveNext()
            match nextChunk with
            | Some chunk -> yield! nextLine_rec stream (rest + chunk)
            | None ->
                if buffer <> "" then yield buffer, ""
    }

let nextLine (stream: IAsyncEnumerator<string>) (buffer: string): IAsyncEnumerator<string * string> =
    (nextLine_rec stream buffer).GetEnumerator()
