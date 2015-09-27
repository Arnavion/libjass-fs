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
module _parser_parse

open types


let private (|StartsWith|_|) (prefix: string) (str: string): string option =
    if str.StartsWith prefix then Some (str.[prefix.Length..]) else None


let private (|StartsWithOneOf|_|) (prefixes: string list) (str: string): string option =
    prefixes
    |> Seq.tryFind (fun prefix -> str.StartsWith prefix)
    |> Option.map (fun prefix -> str.[prefix.Length..])


let private (|RegexMatch|_|) (pattern: string) (str: string): (string * string) option =
    let m = System.Text.RegularExpressions.Regex.Match(str, "^" + pattern)
    if m.Success
    then Some (m.Value, str.[m.Value.Length..])
    else None


let rec private ZeroOrMoreOf_rec (``of``: Set<char>) (str: string): string =
    if str.Length = 0 then str else
    if ``of``.Contains str.[0] then ZeroOrMoreOf_rec ``of`` str.[1..]
    else str

let rec private (|ZeroOrMoreOf|_|) (``of``: Set<char>) (str: string): string option =
    Some (ZeroOrMoreOf_rec ``of`` str)


let private fromLegacy (a: LegacyAlignmentValue): AlignmentValue option =
    match a with
    | LegacyAlignmentValue.BottomLeft -> Some AlignmentValue.BottomLeft
    | LegacyAlignmentValue.BottomCenter -> Some AlignmentValue.BottomCenter
    | LegacyAlignmentValue.BottomRight -> Some AlignmentValue.BottomRight
    | LegacyAlignmentValue.CenterLeft -> Some AlignmentValue.CenterLeft
    | LegacyAlignmentValue.Center -> Some AlignmentValue.Center
    | LegacyAlignmentValue.CenterRight -> Some AlignmentValue.CenterRight
    | LegacyAlignmentValue.TopLeft -> Some AlignmentValue.TopLeft
    | LegacyAlignmentValue.TopCenter -> Some AlignmentValue.TopCenter
    | LegacyAlignmentValue.TopRight -> Some AlignmentValue.TopRight
    | _ -> None


let private (|AsEnableDisable|_|) (str: string): (EnableDisable * string) option =
    match str with
    | StartsWith "0" rest -> Some (false, rest)
    | StartsWith "1" rest -> Some (true, rest)
    | _ -> None


let private (|AsUnsignedDecimal|_|) (str: string): (float * string) option =
    match str with
    | RegexMatch "[0-9]+(?:\.[0-9+])?" (value, rest) ->
        Some (float value, rest)
    | _ -> None


let private (|AsDecimal|_|) (str: string): (float * string) option =
    match str with
    | StartsWith "-" (AsUnsignedDecimal (result, rest)) ->
        Some (-result, rest)
    | AsUnsignedDecimal (result, rest) ->
        Some (result, rest)
    | _ -> None


let private (|AsHexInt32|_|) (str: string): (int * string) option =
    if str.Length = 0 then None else
    let isNegative = str.[0] = '-'

    let rest = if isNegative then str.[1..] else str

    match rest with
    | RegexMatch "[0-9a-fA-F]+" (value, rest) ->
        match platform.StringToHexInt32 isNegative value with
        | Some value -> Some (value, rest)
        | None -> None
    | _ -> None


let private (|AsDecimalOrHexInt32|_|) (str: string): (int * string) option =
    match str with
    | StartsWithOneOf ["&H"; "&h"] (AsHexInt32 (value, rest)) ->
        Some (value, rest)
    | AsDecimal (value, rest) ->
        Some (int value, rest)
    | _ -> None

let private (|AsAlphaValue|_|) (str: string): (float * string) option =
    match str with
    | ZeroOrMoreOf (set ['&'; 'H']) (AsHexInt32 (value, ZeroOrMoreOf (set ['&'; 'H']) rest)) ->
        Some ((1.0 - (float (value &&& 0xFF) / 255.0)), rest)
    | _ -> None


let private (|AsColorValue|_|) (str: string): (Color * string) option =
    match str with
    | ZeroOrMoreOf (set ['&'; 'H']) (AsHexInt32 (value, ZeroOrMoreOf (set ['&'; 'H']) rest)) ->
        Some ({ red = value &&& 0xFF; green = (value >>> 8) &&& 0xFF; blue = (value >>> 16) &&& 0xFF; alpha = 1.0; }, rest)
    | _ -> None


let private (|AsColorWithAlphaValue|_|) (str: string): (Color * string) option =
    match str with
    | ZeroOrMoreOf (set ['&'; 'H']) (AsDecimalOrHexInt32 (value, ZeroOrMoreOf (set ['&'; 'H']) rest)) ->
        Some ({ red = value &&& 0xFF; green = (value >>> 8) &&& 0xFF; blue = (value >>> 16) &&& 0xFF; alpha = 1.0 - float ((value >>> 24) &&& 0xFF) / 255.0; }, rest)
    | _ -> None


let private (|AsBoldValue|_|) (str: string): (BoldValue * string) option =
    match str with
    | RegexMatch "[1-9]00" (value, rest) -> Some (Weight (enum<BoldWeight> (int value)), rest)
    | AsEnableDisable (value, rest) -> Some (Set value, rest)
    | _ -> None


let private (|AsLegacyAlignmentValue|_|) (str: string): (LegacyAlignmentValue * string) option =
    if str.Length = 0 then None else
    match str.[0] with
    | '1' ->
        if str.Length = 1 then Some (enum<LegacyAlignmentValue> 1, str.[1..]) else
        match str.[1] with
        | '0' -> Some (enum<LegacyAlignmentValue> 10, str.[2..])
        | '1' -> Some (enum<LegacyAlignmentValue> 11, str.[2..])
        | _ -> Some (enum<LegacyAlignmentValue> 1, str.[1..])
    | '2' -> Some (enum<LegacyAlignmentValue> 2, str.[1..])
    | '3' -> Some (enum<LegacyAlignmentValue> 3, str.[1..])
    | '5' -> Some (enum<LegacyAlignmentValue> 5, str.[1..])
    | '6' -> Some (enum<LegacyAlignmentValue> 6, str.[1..])
    | '7' -> Some (enum<LegacyAlignmentValue> 7, str.[1..])
    | '9' -> Some (enum<LegacyAlignmentValue> 9, str.[1..])
    | _ -> None


let private (|AsAlignmentValue|_|) (str: string): (AlignmentValue * string) option =
    if str.Length = 0 then None else
    match str.[0] with
    | c when c >= '1' && c <= '9' -> Some (enum<AlignmentValue> (System.Convert.ToInt32 c - System.Convert.ToInt32 '0'), str.[1..])
    | _ -> None


let private (|AsNewLinePart|_|) (str: string): (NewLine * string) option =
    match str with
    | StartsWith "\\N" rest -> Some (new NewLine(), rest)
    | _ -> None


let private (|AsHardSpacePart|_|) (str: string): (Text * string) option =
    match str with
    | StartsWith "\\h" rest -> Some ({ value = "\u00A0" }, rest)
    | _ -> None


let private (|AsTextPart|_|) (str: string): (Text * string) option =
    Some ({ value = str.[0..0] }, str.[1..])


let private (|AsCommentPart|_|) (str: string): (Comment * string) option =
    Some ({ value = str.[0..0] }, str.[1..])


let private As_optional_value (tagName: string) (|Matcher|_|) (str: string) =
    match str with
    | StartsWith tagName rest ->
        match rest with
        | Matcher (value, rest) ->
            Some (Some value, rest)
        | _ ->
            Some (None, rest)
    | _ -> None


let private As_required_value (tagName: string) (|Matcher|_|) (str: string) =
    match str with
    | StartsWith tagName (Matcher (value, rest)) ->
        Some (value, rest)
    | _ -> None


let private (|As_1a_tag|_|) (str: string): (PrimaryAlpha * string) option =
    As_optional_value "1a" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_1c_tag|_|) (str: string): (PrimaryColor * string) option =
    As_optional_value "1c" (|AsColorValue|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_2a_tag|_|) (str: string): (SecondaryAlpha * string) option =
    As_optional_value "2a" (|AsAlphaValue|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_2c_tag|_|) (str: string): (SecondaryColor * string) option =
    As_optional_value "2c" (|AsColorValue|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_3a_tag|_|) (str: string): (OutlineAlpha * string) option =
    As_optional_value "3a" (|AsAlphaValue|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_3c_tag|_|) (str: string): (OutlineColor * string) option =
    As_optional_value "3c" (|AsColorValue|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_4a_tag|_|) (str: string): (ShadowAlpha * string) option =
    As_optional_value "4a" (|AsAlphaValue|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_4c_tag|_|) (str: string): (ShadowColor * string) option =
    As_optional_value "4c" (|AsColorValue|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_a_tag|_|) (str: string): (Alignment * string) option =
    match str with
    | StartsWith "a" (AsLegacyAlignmentValue (value, rest)) ->
        fromLegacy value
        |> Option.map (fun value -> ({ value = value }, rest))
    | _ -> None


let private (|As_alpha_tag|_|) (str: string): (Alpha * string) option =
    As_optional_value "alpha" (|AsAlphaValue|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_an_tag|_|) (str: string): (Alignment * string) option =
    As_required_value "an" (|AsAlignmentValue|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_b_tag|_|) (str: string): (Bold * string) option =
    As_optional_value "b" (|AsBoldValue|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_be_tag|_|) (str: string): (Blur * string) option =
    As_optional_value "be" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_blur_tag|_|) (str: string): (GaussianBlur * string) option =
    As_optional_value "blur" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_bord_tag|_|) (str: string): (Border * string) option =
    As_optional_value "bord" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_c_tag|_|) (str: string): (PrimaryColor * string) option =
    As_optional_value "c" (|AsColorValue|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_fad_tag|_|) (str: string): (Fade * string) option =
    match str with
    | StartsWith "fad(" (AsDecimal (start, StartsWith "," (AsDecimal (``end``, StartsWith ")" rest)))) ->
        Some ({ start = start; ``end`` = ``end``; }, rest)
    | _ -> None


let private (|As_fade_tag|_|) (str: string): (ComplexFade * string) option =
    match str with
    | StartsWith "fade("
        (AsDecimal (a1, StartsWith "," (AsDecimal (a2, StartsWith "," (AsDecimal (a3, StartsWith "," (
            AsDecimal (t1, StartsWith "," (AsDecimal (t2, StartsWith "," (AsDecimal (t3, StartsWith "," (AsDecimal (t4, StartsWith ")" rest)))))))
        ))))))) ->
        Some ({
                a1 = 1.0 - a1 / 255.0; a2 = 1.0 - a2 / 255.0; a3 = 1.0 - a3 / 255.0;
                t1 = t1 / 1000.0; t2 = t2 / 1000.0; t3 = t3 / 1000.0; t4 = t4 / 1000.0;
        }, rest)
    | _ -> None


let private (|As_fax_tag|_|) (str: string): (SkewX * string) option =
    As_optional_value "fax" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_fay_tag|_|) (str: string): (SkewY * string) option =
    As_optional_value "fay" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_fn_tag|_|) (str: string): (FontName * string) option =
    As_optional_value "fn" ((|RegexMatch|_|) "[^\\\}]+") str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_fr_tag|_|) (str: string): (RotateZ * string) option =
    As_optional_value "fr" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_frx_tag|_|) (str: string): (RotateX * string) option =
    As_optional_value "frx" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_fry_tag|_|) (str: string): (RotateY * string) option =
    As_optional_value "fry" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_frz_tag|_|) (str: string): (RotateZ * string) option =
    As_optional_value "frz" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_fs_tag|_|) (str: string): (FontSize * string) option =
    As_optional_value "fs" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_fscx_tag|_|) (str: string): (FontScaleX * string) option =
    As_optional_value "fscx" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value |> Option.map (fun value -> value / 100.0) }, rest))


let private (|As_fscy_tag|_|) (str: string): (FontScaleY * string) option =
    As_optional_value "fscy" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value |> Option.map (fun value -> value / 100.0) }, rest))


let private (|As_fsp_tag|_|) (str: string): (LetterSpacing * string) option =
    As_optional_value "fsp" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_fsplus_tag|_|) (str: string): (FontSizePlus * string) option =
    As_required_value "fs+" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_fsminus_tag|_|) (str: string): (FontSizeMinus * string) option =
    As_required_value "fs-" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_i_tag|_|) (str: string): (Italic * string) option =
    As_optional_value "i" (|AsEnableDisable|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_K_tag|_|) (str: string): (SweepingColorKaraoke * string) option =
    As_required_value "K" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ duration = value / 100.0 }, rest))


let private (|As_k_tag|_|) (str: string): (ColorKaraoke * string) option =
    As_required_value "k" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ duration = value / 100.0 }, rest))


let private (|As_kf_tag|_|) (str: string): (SweepingColorKaraoke * string) option =
    As_required_value "kf" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ duration = value / 100.0 }, rest))


let private (|As_ko_tag|_|) (str: string): (OutlineKaraoke * string) option =
    As_required_value "ko" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ duration = value / 100.0 }, rest))


let private As_move_tag_6 (move: Move) (str: string): (Move * string) option =
    match str with
    | AsDecimal (t1, StartsWith "," (AsDecimal (t2, StartsWith ")" rest))) ->
        Some ({
            x1 = move.x1; y1 = move.y1; x2 = move.x2; y2 = move.y2;
            t1 = Some (t1 / 1000.0); t2 = Some (t2 / 1000.0);
        }, rest)
    | _ -> None


let private As_move_tag_4 (move: Move) (str: string): (Move * string) option =
    match str with
    | StartsWith "," rest ->
        As_move_tag_6 move rest
    | StartsWith ")" rest ->
        Some ({ x1 = move.x1; y1 = move.y1; x2 = move.x2; y2 = move.y2; t1 = None; t2 = None; }, rest)
    | _ -> None


let private (|As_move_tag|_|) (str: string): (Move * string) option =
    match str with
    | StartsWith "move(" (AsDecimal (x1, StartsWith "," (AsDecimal (y1, StartsWith "," (AsDecimal (x2, StartsWith "," (AsDecimal (y2, rest)))))))) ->
        As_move_tag_4 { x1 = x1; y1 = y1; x2 = x2; y2 = y2; t1 = None; t2 = None; } rest
    | _ -> None


let private (|As_org_tag|_|) (str: string): (RotationOrigin * string) option =
    match str with
    | StartsWith "org(" (AsDecimal (x, StartsWith "," (AsDecimal (y, StartsWith ")" rest)))) ->
        Some ({ x = x; y = y; }, rest)
    | _ -> None


let private (|As_p_tag|_|) (str: string): (DrawingMode * string) option =
    As_required_value "p" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ scale = value }, rest))


let private (|As_q_tag|_|) (str: string): (WrappingStyle * string) option =
    match str with
    | StartsWith "q" rest ->
        if rest.Length = 0 then None else
        match rest.[0] with
        | c when c >= '0' && c <= '3' -> Some ({ value = enum<WrappingStyleValue> (System.Convert.ToInt32 c - System.Convert.ToInt32 '0') }, rest.[1..])
        | _ -> None
    | _ -> None


let private (|As_pbo_tag|_|) (str: string): (DrawingBaselineOffset * string) option =
    As_required_value "pbo" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_pos_tag|_|) (str: string): (Position * string) option =
    match str with
    | StartsWith "pos(" (AsDecimal (x, StartsWith "," (AsDecimal (y, StartsWith ")" rest)))) ->
        Some ({ x = x; y = y; }, rest)
    | _ -> None


let private (|As_r_tag|_|) (str: string): (Reset * string) option =
    As_optional_value "r" ((|RegexMatch|_|) "[^\\\}]+") str
    |> Option.map (fun (value, rest) -> ({ styleName = value }, rest))


let private (|As_s_tag|_|) (str: string): (StrikeThrough * string) option =
    As_optional_value "s" (|AsEnableDisable|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_shad_tag|_|) (str: string): (Shadow * string) option =
    As_optional_value "shad" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_xbord_tag|_|) (str: string): (BorderX * string) option =
    As_optional_value "xbord" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_xshad_tag|_|) (str: string): (ShadowX * string) option =
    As_optional_value "xshad" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_ybord_tag|_|) (str: string): (BorderY * string) option =
    As_optional_value "ybord" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_yshad_tag|_|) (str: string): (ShadowY * string) option =
    As_optional_value "yshad" (|AsDecimal|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|As_u_tag|_|) (str: string): (Underline * string) option =
    As_optional_value "u" (|AsEnableDisable|_|) str
    |> Option.map (fun (value, rest) -> ({ value = value }, rest))


let private (|AsTransformableTag|_|) (str: string): (TransformableTag * string) option =
    match str with
    | As_alpha_tag (alpha, rest) -> Some (TransformableTag.Alpha alpha, rest)
    //| As_iclip_tag (iclip, rest) -> Some (TransformableTag.Alpha alpha, rest)
    | As_xbord_tag (xbord, rest) -> Some (TransformableTag.BorderX xbord, rest)
    | As_ybord_tag (ybord, rest) -> Some (TransformableTag.BorderY ybord, rest)
    | As_xshad_tag (xshad, rest) -> Some (TransformableTag.ShadowX xshad, rest)
    | As_yshad_tag (yshad, rest) -> Some (TransformableTag.ShadowY yshad, rest)

    | As_blur_tag (blur, rest) -> Some (TransformableTag.GaussianBlur blur, rest)
    | As_bord_tag (bord, rest) -> Some (TransformableTag.Border bord, rest)
    //| As_clip_tag (clip, rest) -> Some (TransformableTag.Alpha alpha, rest)
    | As_fscx_tag (fscx, rest) -> Some (TransformableTag.FontScaleX fscx, rest)
    | As_fscy_tag (fscy, rest) -> Some (TransformableTag.FontScaleY fscy, rest)
    | As_shad_tag (shad, rest) -> Some (TransformableTag.Shadow shad, rest)

    | As_fax_tag (fax, rest) -> Some (TransformableTag.SkewX fax, rest)
    | As_fay_tag (fay, rest) -> Some (TransformableTag.SkewY fay, rest)
    | As_frx_tag (frx, rest) -> Some (TransformableTag.RotateX frx, rest)
    | As_fry_tag (fry, rest) -> Some (TransformableTag.RotateY fry, rest)
    | As_frz_tag (frz, rest) -> Some (TransformableTag.RotateZ frz, rest)
    | As_fsp_tag (fsp, rest) -> Some (TransformableTag.LetterSpacing fsp, rest)
    | As_fsplus_tag (fsplus, rest) -> Some (TransformableTag.FontSizePlus fsplus, rest)
    | As_fsminus_tag (fsminus, rest) -> Some (TransformableTag.FontSizeMinus fsminus, rest)

    | As_be_tag (be, rest) -> Some (TransformableTag.Blur be, rest)
    | As_fr_tag (fr, rest) -> Some (TransformableTag.RotateZ fr, rest)
    | As_fs_tag (fs, rest) -> Some (TransformableTag.FontSize fs, rest)
    | As_1a_tag (_1a, rest) -> Some (TransformableTag.PrimaryAlpha _1a, rest)
    | As_1c_tag (_1c, rest) -> Some (TransformableTag.PrimaryColor _1c, rest)
    | As_2a_tag (_2a, rest) -> Some (TransformableTag.SecondaryAlpha _2a, rest)
    | As_2c_tag (_2c, rest) -> Some (TransformableTag.SecondaryColor _2c, rest)
    | As_3a_tag (_3a, rest) -> Some (TransformableTag.OutlineAlpha _3a, rest)
    | As_3c_tag (_3c, rest) -> Some (TransformableTag.OutlineColor _3c, rest)
    | As_4a_tag (_4a, rest) -> Some (TransformableTag.ShadowAlpha _4a, rest)
    | As_4c_tag (_4c, rest) -> Some (TransformableTag.ShadowColor _4c, rest)

    | As_c_tag (c, rest) -> Some (TransformableTag.PrimaryColor c, rest)

    | _ -> None


let rec private AsTransformableTags_rec (result: TransformableTag list) (str: string): (TransformableTag list * string) option =
    match str with
    | s when s.Length = 0 -> Some (result, str)
    | StartsWith ")" rest -> Some (result, rest)
    | StartsWith "}" rest -> Some (result, rest)
    | StartsWith "\\" (AsTransformableTag (tag, rest)) -> AsTransformableTags_rec (result @ [tag]) rest
    | _ -> None


let private (|AsTransformableTags|_|) (str: string): (TransformableTag list * string) option = AsTransformableTags_rec [] str


(*
// This simple and intuitive implementation unfortunately generates overly excessive branching in the resulting IL, necessitating the following separated-out implementation

let private (|As_t_tag|_|) (str: string): (Transform * string) option =
    match str with
    | StartsWith "t(" (AsDecimal (start, StartsWith "," (AsDecimal (``end``, StartsWith "," (AsDecimal (accel, StartsWith "," (AsTransformableTags (tags, rest)))))))) ->
        Some ({ start = Some start; ``end`` = Some ``end``; accel = Some accel; tags = tags; }, rest)
    | StartsWith "t(" (AsDecimal (start, StartsWith "," (AsDecimal (``end``, StartsWith "," (AsTransformableTags (tags, rest)))))) ->
        Some ({ start = Some start; ``end`` = Some ``end``; accel = None; tags = tags; }, rest)
    | StartsWith "t(" (AsDecimal (accel, StartsWith "," (AsTransformableTags (tags, rest)))) ->
        Some ({ start = None; ``end`` = None; accel = Some accel; tags = tags; }, rest)
    | StartsWith "t(" (AsTransformableTags (tags, rest)) ->
        Some ({ start = None; ``end`` = None; accel = None; tags = tags; }, rest)
    | _ -> None
*)


let private As_t_tag_tags (t: Transform) (str: string): (Transform * string) option =
    match str with
    | AsTransformableTags (tags, rest) ->
        Some ({ t with tags = tags; }, rest)
    | _ -> None


let private As_t_tag_times (str: string): (Transform * string) option =
    match str with
    | AsDecimal (first, StartsWith "," rest) ->
        match rest with
        | AsDecimal (second, StartsWith "," rest) ->
            match rest with
            | AsDecimal (third, StartsWith "," rest) ->
                As_t_tag_tags { start = Some first; ``end`` = Some second; accel = Some third; tags = []; } rest
            | rest ->
                As_t_tag_tags { start = Some first; ``end`` = Some second; accel = None; tags = []; } rest
        | rest ->
            As_t_tag_tags { start = None; ``end`` = None; accel = Some first; tags = []; } rest
    | rest ->
        As_t_tag_tags { start = None; ``end`` = None; accel = None; tags = []; } rest


let private (|As_t_tag|_|) (str: string): (Transform * string) option =
    match str with
    | StartsWith "t(" rest -> As_t_tag_times rest
    | _ -> None


let private (|AsAnyTag|_|) (str: string): (Part * string) option =
    match str with
    | As_alpha_tag (alpha, rest) -> Some (Part.Alpha alpha, rest)
    //| As_iclip_tag (iclip, rest) -> Some (Part.Alpha alpha, rest)
    | As_xbord_tag (xbord, rest) -> Some (Part.BorderX xbord, rest)
    | As_ybord_tag (ybord, rest) -> Some (Part.BorderY ybord, rest)
    | As_xshad_tag (xshad, rest) -> Some (Part.ShadowX xshad, rest)
    | As_yshad_tag (yshad, rest) -> Some (Part.ShadowY yshad, rest)

    | As_blur_tag (blur, rest) -> Some (Part.GaussianBlur blur, rest)
    | As_bord_tag (bord, rest) -> Some (Part.Border bord, rest)
    //| As_clip_tag (clip, rest) -> Some (Part.Alpha alpha, rest)
    | As_fade_tag (fade, rest) -> Some (Part.ComplexFade fade, rest)
    | As_fscx_tag (fscx, rest) -> Some (Part.FontScaleX fscx, rest)
    | As_fscy_tag (fscy, rest) -> Some (Part.FontScaleY fscy, rest)
    | As_move_tag (move, rest) -> Some (Part.Move move, rest)
    | As_shad_tag (shad, rest) -> Some (Part.Shadow shad, rest)

    | As_fad_tag (fad, rest) -> Some (Part.Fade fad, rest)
    | As_fax_tag (fax, rest) -> Some (Part.SkewX fax, rest)
    | As_fay_tag (fay, rest) -> Some (Part.SkewY fay, rest)
    | As_frx_tag (frx, rest) -> Some (Part.RotateX frx, rest)
    | As_fry_tag (fry, rest) -> Some (Part.RotateY fry, rest)
    | As_frz_tag (frz, rest) -> Some (Part.RotateZ frz, rest)
    | As_fsp_tag (fsp, rest) -> Some (Part.LetterSpacing fsp, rest)
    | As_fsplus_tag (fsplus, rest) -> Some (Part.FontSizePlus fsplus, rest)
    | As_fsminus_tag (fsminus, rest) -> Some (Part.FontSizeMinus fsminus, rest)
    | As_org_tag (org, rest) -> Some (Part.RotationOrigin org, rest)
    | As_pbo_tag (pbo, rest) -> Some (Part.DrawingBaselineOffset pbo, rest)
    | As_pos_tag (pos, rest) -> Some (Part.Position pos, rest)

    | As_an_tag (an, rest) -> Some (Part.Alignment an, rest)
    | As_be_tag (be, rest) -> Some (Part.Blur be, rest)
    | As_fn_tag (fn, rest) -> Some (Part.FontName fn, rest)
    | As_fr_tag (fr, rest) -> Some (Part.RotateZ fr, rest)
    | As_fs_tag (fs, rest) -> Some (Part.FontSize fs, rest)
    | As_kf_tag (kf, rest) -> Some (Part.SweepingColorKaraoke kf, rest)
    | As_ko_tag (ko, rest) -> Some (Part.OutlineKaraoke ko, rest)
    | As_1a_tag (_1a, rest) -> Some (Part.PrimaryAlpha _1a, rest)
    | As_1c_tag (_1c, rest) -> Some (Part.PrimaryColor _1c, rest)
    | As_2a_tag (_2a, rest) -> Some (Part.SecondaryAlpha _2a, rest)
    | As_2c_tag (_2c, rest) -> Some (Part.SecondaryColor _2c, rest)
    | As_3a_tag (_3a, rest) -> Some (Part.OutlineAlpha _3a, rest)
    | As_3c_tag (_3c, rest) -> Some (Part.OutlineColor _3c, rest)
    | As_4a_tag (_4a, rest) -> Some (Part.ShadowAlpha _4a, rest)
    | As_4c_tag (_4c, rest) -> Some (Part.ShadowColor _4c, rest)

    | As_a_tag (a, rest) -> Some (Part.Alignment a, rest)
    | As_b_tag (b, rest) -> Some (Part.Bold b, rest)
    | As_c_tag (c, rest) -> Some (Part.PrimaryColor c, rest)
    | As_i_tag (i, rest) -> Some (Part.Italic i, rest)
    | As_k_tag (k, rest) -> Some (Part.ColorKaraoke k, rest)
    | As_K_tag (K, rest) -> Some (Part.SweepingColorKaraoke K, rest)
    | As_p_tag (p, rest) -> Some (Part.DrawingMode p, rest)
    | As_q_tag (q, rest) -> Some (Part.WrappingStyle q, rest)
    | As_r_tag (r, rest) -> Some (Part.Reset r, rest)
    | As_s_tag (s, rest) -> Some (Part.StrikeThrough s, rest)
    | As_t_tag (t, rest) -> Some (Part.Transform t, rest)
    | As_u_tag (u, rest) -> Some (Part.Underline u, rest)

    | _ -> None


let rec private parse_rec (result: Part list) (str: string): Part list =
    match str with
    | s when s.Length = 0 -> result
    | StartsWith "{" rest -> parse_rec_braces result rest
    | _ -> parse_rec (result @ [Text { value = str.[0..0] }]) str.[1..]


and private parse_rec_braces (result: Part list) (str: string): Part list =
    match str with
    | s when s.Length = 0 -> result
    | StartsWith "}" rest -> parse_rec result rest
    | StartsWith "\\" (AsAnyTag (tag, rest)) -> parse_rec_braces (result @ [tag]) rest
    | _ -> parse_rec_braces (result @ [Comment { value = str.[0..0] }]) str.[1..]


type private Mergeable =
    | Text of Text
    | Comment of Comment


let rec private mergeAdjacent (result: Part list) (lastSeen: Mergeable option) (parts: Part list): Part list =
    match parts with
    | [] ->
        match lastSeen with
        | Some (Mergeable.Text text) -> result @ [Part.Text text]
        | Some (Mergeable.Comment comment) -> result @ [Part.Comment comment]
        | None -> result
    | current :: rest ->
        match lastSeen, current with
        | None, Part.Text t -> mergeAdjacent result (Some (Mergeable.Text t)) rest
        | None, Part.Comment c -> mergeAdjacent result (Some (Mergeable.Comment c)) rest
        | None, other -> mergeAdjacent (result @ [other]) None rest
        | Some (Mergeable.Text t1), Part.Text t2 -> mergeAdjacent result (Some (Mergeable.Text { value = t1.value + t2.value })) rest
        | Some (Mergeable.Text t), other -> mergeAdjacent (result @ [Part.Text t]) None parts
        | Some (Mergeable.Comment c1), Part.Comment c2 -> mergeAdjacent result (Some (Mergeable.Comment { value = c1.value + c2.value })) rest
        | Some (Mergeable.Comment c), other -> mergeAdjacent (result @ [Part.Comment c]) None parts


type ParserRule =
    | DialogueParts = 0


let parse (str: string) (rule: ParserRule): Part list option =
    match rule with
    | ParserRule.DialogueParts ->
        str
        |> parse_rec []
        |> mergeAdjacent [] None
        |> Some
    | _ -> None
