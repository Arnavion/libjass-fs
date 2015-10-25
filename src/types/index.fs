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

module libjass.types


open FSharp.Control


let private (|AsFloat|_|) (str: string): float option =
    let mutable result = 0.0
    if System.Double.TryParse(str, &result) then
        Some result
    else
        None

let private (|AsTime|) (str: string): float =
    str.Split(':') |> Array.fold (fun previous current -> previous * 60.0 + (System.Convert.ToDouble current)) 0.0

let private (|AsProperty|_|) (str: string): (string * string) option =
    let colonPos = str.IndexOf(":")
    if colonPos = -1 then None
    else
        let name = str.[0..colonPos - 1]
        let value = str.[colonPos + 1..].TrimStart()
        Some (name, value)


let private (|AsMap|_|) (formatSpecifier: string array) (str: string): Map<string, string> option =
    let components = str.Split(',')
    if components.Length < formatSpecifier.Length then None
    else
        let components =
            if components.Length = formatSpecifier.Length then components
            else
                Array.concat [ components.[0..formatSpecifier.Length - 2]; [| components.[formatSpecifier.Length - 1..] |> String.concat "," |]]
        (Array.zip formatSpecifier components) |> Map.ofArray |> Some


type WrappingStyle =
| SmartWrappingWithWiderTopLine = 0
| SmartWrappingWithWiderBottomLine = 3
| EndOfLineWrapping = 1
| NoLineWrapping = 2


type BorderStyle =
| Outline = 1
| OpaqueBox = 3


type ScriptProperties(resolutionX: int, resolutionY: int, wrappingStyle: WrappingStyle, scaleBorderAndShadow: bool) =
    member x.ResolutionX = resolutionX
    member x.ResolutionY = resolutionY
    member x.WrappingStyle = wrappingStyle
    member x.ScaleBorderAndShadow = scaleBorderAndShadow

    member x.withResolutionX (resolutionX: int) =
        new ScriptProperties(resolutionX, x.ResolutionY, x.WrappingStyle, x.ScaleBorderAndShadow)

    member x.withResolutionY (resolutionY: int) =
        new ScriptProperties(x.ResolutionX, resolutionY, x.WrappingStyle, x.ScaleBorderAndShadow)

    member x.withWrappingStyle (wrappingStyle: WrappingStyle) =
        new ScriptProperties(x.ResolutionX, x.ResolutionY, wrappingStyle, x.ScaleBorderAndShadow)

    member x.withScaleBorderAndShadow (scaleBorderAndShadow: bool) =
        new ScriptProperties(x.ResolutionX, x.ResolutionY, x.WrappingStyle, scaleBorderAndShadow)


type Style = {
    name: string
    italic: bool
    bold: bool
    underline: bool
    strikeThrough: bool

    fontName: string
    fontSize: float

    fontScaleX: float
    fontScaleY: float

    letterSpacing: float

    rotationZ: float

    primaryColor: parts.Color
    secondaryColor: parts.Color
    outlineColor: parts.Color
    shadowColor: parts.Color

    outlineThickness: float
    borderStyle: BorderStyle

    shadowDepth: float

    alignment: parts.AlignmentValue

    marginLeft: float
    marginRight: float
    marginVertical: float
}

type Dialogue(style: Style, start: float, ``end``: float, layer: int, alignment: parts.AlignmentValue, text: string) =
    let parts = lazy(5)

    member x.Style = style
    member x.Start = start
    member x.End = ``end``
    member x.Layer = layer
    member x.Alignment = alignment
    member x.Text = text
    member x.Parts = parts

    member x.withStyle (style: Style) =
        new Dialogue(style, start, ``end``, layer, alignment, text)

    member x.withStart (start: float) =
        new Dialogue(style, start, ``end``, layer, alignment, text)

    member x.withEnd (``end``: float) =
        new Dialogue(style, start, ``end``, layer, alignment, text)

    member x.withLayer (layer: int) =
        new Dialogue(style, start, ``end``, layer, alignment, text)

    member x.withAlignment (alignment: parts.AlignmentValue) =
        new Dialogue(style, start, ``end``, layer, alignment, text)

    member x.withText (text: string) =
        new Dialogue(style, start, ``end``, layer, alignment, text)


type ASS(scriptProperties: ScriptProperties, styles: Map<string, Style>, dialogues: Dialogue list, stylesFormatSpecifier: string array option, dialoguesFormatSpecifier: string array option) =
    member x.ScriptProperties = scriptProperties
    member x.Styles = styles
    member x.Dialogues = dialogues
    member x.StylesFormatSpecifier = stylesFormatSpecifier
    member x.DialoguesFormatSpecifier = dialoguesFormatSpecifier

    member x.withScriptProperties (scriptProperties: ScriptProperties): ASS =
        new ASS(scriptProperties, styles, dialogues, stylesFormatSpecifier, dialoguesFormatSpecifier)

    member x.withStyle (style: Style): ASS =
        new ASS(scriptProperties, Map.add style.name style styles, dialogues, stylesFormatSpecifier, dialoguesFormatSpecifier)

    member x.withDialogue (dialogue: Dialogue): ASS =
        new ASS(scriptProperties, styles, dialogues @ [dialogue], stylesFormatSpecifier, dialoguesFormatSpecifier)

    member x.withStylesFormatSpecifier (stylesFormatSpecifier: string array): ASS =
        new ASS(scriptProperties, styles, dialogues, Some stylesFormatSpecifier, dialoguesFormatSpecifier)

    member x.withDialoguesFormatSpecifier (dialoguesFormatSpecifier: string array): ASS =
        new ASS(scriptProperties, styles, dialogues, stylesFormatSpecifier, Some dialoguesFormatSpecifier)


let rec private styleFromTemplate_rec (style: Style) (name: string) (value: string): Style =
    match name, value with
    | "name", value ->
        { style with name = value.TrimStart('*') }

    | "italic", AsFloat value ->
        { style with italic = value <> 0.0 }

    | "bold", AsFloat value ->
        { style with bold = value <> 0.0 }

    | "underline", AsFloat value ->
        { style with underline = value <> 0.0 }

    | "strikeout", AsFloat value ->
        { style with strikeThrough = value <> 0.0 }

    | "fontname", value ->
        { style with fontName = value.TrimStart('*') }

    | "fontsize", AsFloat value ->
        { style with fontSize = value }

    | "scalex", AsFloat value ->
        if value >= 0.0 then { style with fontScaleX = value / 100.0 } else style

    | "scaley", AsFloat value ->
        if value >= 0.0 then { style with fontScaleY = value / 100.0 } else style

    | "spacing", AsFloat value ->
        if value >= 0.0 then { style with letterSpacing = value } else style

    | "angle", AsFloat value ->
        { style with rotationZ = value }

    | "primarycolour", _parser_parse.AsColorWithAlphaValue (value, "") ->
        { style with primaryColor = value }

    | "secondarycolour", _parser_parse.AsColorWithAlphaValue (value, "") ->
        { style with secondaryColor = value }

    | "outlinecolour", _parser_parse.AsColorWithAlphaValue (value, "") ->
        { style with outlineColor = value }

    | "backcolour", _parser_parse.AsColorWithAlphaValue (value, "") ->
        { style with shadowColor = value }

    | "outline", AsFloat value ->
        if value >= 0.0 then { style with outlineThickness = value } else style

    | "borderstyle", _parser_parse.RegexMatch "[13]" (value, "") ->
        { style with borderStyle = value |> System.Convert.ToInt32 |> enum<BorderStyle> }

    | "shadow", AsFloat value ->
        if value >= 0.0 then { style with shadowDepth = value } else style

    | "alignment", _parser_parse.AsAlignmentValue (value, "") ->
        { style with alignment = value }

    | "marginl", AsFloat value ->
        { style with marginLeft = value }

    | "marginr", AsFloat value ->
        { style with marginRight = value }

    | "marginv", AsFloat value ->
        { style with marginVertical = value }

    | "encoding", _ ->
        style

    | name, _ ->
        eprintfn "Unknown property %s for style %s" name style.name
        style

let private styleFromTemplate (map: Map<string, string>): Style option =
    let style = {
        name = ""

        italic = false
        bold = false
        underline = false
        strikeThrough = false

        fontName = "sans-serif"
        fontSize = 18.0

        fontScaleX = 1.0
        fontScaleY = 1.0

        letterSpacing = 0.0

        rotationZ = 0.0

        primaryColor = { red = 255; green = 255; blue = 255; alpha = 1.0; }
        secondaryColor = { red = 0; green = 255; blue = 255; alpha = 1.0; }
        outlineColor = { red = 0; green = 0; blue = 0; alpha = 1.0; }
        shadowColor = { red = 0; green = 0; blue = 0; alpha = 1.0 - 128.0 / 255.0; }

        outlineThickness = 2.0
        borderStyle = BorderStyle.Outline

        shadowDepth = 3.0

        alignment = parts.AlignmentValue.BottomCenter

        marginLeft = 20.0
        marginRight = 20.0
        marginVertical = 20.0
    }

    let style = Map.fold styleFromTemplate_rec style map
    if style.name = "" then None else Some style


let rec private dialogueFromTemplate_rec (ass: ASS) (dialogue: Dialogue) (name: string) (value: string): Dialogue =
    match name, value with
    | "style", value ->
        match ass.Styles.TryFind(value) with
        | Some style ->
            style |> dialogue.withStyle
        | None ->
            dialogue

    | "start", AsTime value ->
        value |> dialogue.withStart

    | "end", AsTime value ->
        value |> dialogue.withEnd

    | "layer", _parser_parse.RegexMatch "[0-9]+" (value, "") ->
        value |> System.Convert.ToInt32 |> dialogue.withLayer

    | "text", value ->
        value |> dialogue.withText

    | "effect", _
    | "marginl", _
    | "marginr", _
    | "marginv", _
    | "name", _ ->
        dialogue

    | name, value ->
        eprintfn "Unknown property %s for dialogue" name
        dialogue

let private dialogueFromTemplate (ass: ASS) (map: Map<string, string>): Dialogue =
    match ass.Styles.TryFind("Default") with
    | Some style ->
        let dialogue = new Dialogue(style = style, start = 0.0, ``end`` = 0.0, layer = 0, alignment = parts.AlignmentValue.BottomCenter, text = "")

        Map.fold (dialogueFromTemplate_rec ass) dialogue map
    | None ->
        failwith "Default style not found in ASS"


type private ASSParserState =
| Beginning
| InSection of name: string

type ASS with
    static member fromStream (stream: AsyncSeq<string>): Async<ASS> =
        let lines = streams.getLines stream

        let defaultStyle = (styleFromTemplate (Map.add "name" "Default" Map.empty)).Value
        let ass =
            new ASS(
                scriptProperties = new ScriptProperties(resolutionX = 0, resolutionY = 0, wrappingStyle = WrappingStyle.EndOfLineWrapping, scaleBorderAndShadow = true),
                styles = Map.empty,
                dialogues = List.empty,
                stylesFormatSpecifier = None,
                dialoguesFormatSpecifier = None)

        let result =
            lines
            |> AsyncSeq.fold
                (fun (ass: ASS, state) line ->
                    let line = if state = Beginning && line.Length > 0 && line.[0] = '\uFEFF' then line.[1..] else line
                    match line with
                    | "" -> ass, state
                    | _parser_parse.StartsWith ";" rest -> ass, state
                    | _parser_parse.RegexMatch """\[[^]]+\]""" (value, rest) ->
                        let name = value.[1..value.Length - 2]
                        ass, InSection name
                    | line ->
                        match state with
                        | Beginning
                        | InSection "Script Info" ->
                            match line with
                            | AsProperty (name, value) ->
                                match name, value with
                                | "PlayResX", _parser_parse.RegexMatch "[0-9]+" (value, "") ->
                                    (value |> System.Convert.ToInt32 |> ass.ScriptProperties.withResolutionX |> ass.withScriptProperties), state
                                | "PlayResY", _parser_parse.RegexMatch "[0-9]+" (value, "") ->
                                    (value |> System.Convert.ToInt32 |> ass.ScriptProperties.withResolutionY |> ass.withScriptProperties), state
                                | "WrapStyle", _parser_parse.RegexMatch "[0-3]" (value, "") ->
                                    (value |> System.Convert.ToInt32 |> enum<WrappingStyle> |> ass.ScriptProperties.withWrappingStyle |> ass.withScriptProperties), state
                                | "ScaledBorderAndShadow", _parser_parse.RegexMatch "yes" (value, "") ->
                                    (true |> ass.ScriptProperties.withScaleBorderAndShadow |> ass.withScriptProperties), state
                                | "ScaledBorderAndShadow", _ ->
                                    (false |> ass.ScriptProperties.withScaleBorderAndShadow |> ass.withScriptProperties), state
                                | _ ->
                                    ass, state
                            | _ ->
                                ass, state
                        | InSection "V4+ Styles"
                        | InSection "V4 Styles" ->
                            match ass.StylesFormatSpecifier with
                            | None ->
                                match line with
                                | AsProperty ("Format", value) ->
                                    (value.Split(',') |> Array.map (fun v -> v.Trim().ToLowerInvariant()) |> ass.withStylesFormatSpecifier), state
                                | _ ->
                                    ass, state
                            | Some stylesFormatSpecifier ->
                                match line with
                                | AsProperty ("Style", AsMap stylesFormatSpecifier value) ->
                                    match styleFromTemplate value with
                                    | Some style -> (style |> ass.withStyle), state
                                    | None -> ass, state
                                | _ ->
                                    ass, state
                        | InSection "Events" ->
                            match ass.DialoguesFormatSpecifier with
                            | None ->
                                match line with
                                | AsProperty ("Format", value) ->
                                    (value.Split(',') |> Array.map (fun v -> v.Trim().ToLowerInvariant()) |> ass.withDialoguesFormatSpecifier), state
                                | _ ->
                                    ass, state
                            | Some dialoguesFormatSpecifier ->
                                match line with
                                | AsProperty ("Dialogue", AsMap dialoguesFormatSpecifier value) ->
                                    (value |> (dialogueFromTemplate ass) |> ass.withDialogue), state
                                | _ ->
                                    ass, state
                        | _ ->
                            ass, state
                )
                (ass.withStyle defaultStyle, Beginning)

        async {
            let! ass, state = result
            return ass
        }

    static member fromStreamReader (streamReader: System.IO.StreamReader): Async<ASS> =
        let buffer = Array.zeroCreate 4096
        let rec getChunks() =
            asyncSeq {
                let! read = Async.AwaitTask (streamReader.ReadAsync(buffer, 0, buffer.Length))
                if read <> 0 then
                    yield new string(buffer.[0..read - 1])
                    yield! getChunks()
            }

        getChunks() |> ASS.fromStream

    static member fromFile (filename: string): Async<ASS> =
        async {
            use fileStream = System.IO.File.OpenRead(filename)
            use streamReader = new System.IO.StreamReader(fileStream)
            return! ASS.fromStreamReader streamReader
        }

    static member fromString (str: string): Async<ASS> =
        AsyncSeq.singleton str |> ASS.fromStream
