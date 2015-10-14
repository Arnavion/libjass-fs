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

module program

[<EntryPoint>]
let main args =
    let input = "{\\\\k0}{\\1c&H35C2BD&}s{\\1c&H53A2C0&}e{\\1c&H757FC4&}i{\\1c&H866EC6&}j{\\1c&H995AC8&}a{\\1c&HBC36CC&}k{\\1c&HD420C8&}u{\\1c&HB942A7&} {\\1c&HA25F8A&}w{\\1c&H799356&}a{\\1c&H63B03A&} {\\1c&H54C723&}k{\\1c&H68C126&}i{\\1c&H7BBC29&}r{\\1c&H8CB82C&}i{\\1c&HA2B230&}s{\\1c&HBEAB34&}a{\\1c&HD8A339&}k{\\1c&HB68D3B&}u{\\1c&HA4813C&} {\\1c&H88703E&}y{\\1c&H665A41&}o{\\1c&H444444&"
    //let input = "abc{\\1cdeqrs}ghi"
    //let input = "{\\a5}"
    match libjass.parser.parse libjass.parser.ParserRule.DialogueParts input with
    | Some parts -> List.map (printfn "%A") parts |> ignore
    | None -> eprintfn "Parse failed!"

    0
