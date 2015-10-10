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

namespace libjass.parts

type EnableDisable = bool

type BoldWeight =
    | B100 = 100
    | B200 = 200
    | B300 = 300
    | B400 = 400
    | B500 = 500
    | B600 = 600
    | B700 = 700
    | B800 = 800
    | B900 = 900

type BoldValue =
    | Set of EnableDisable
    | Weight of BoldWeight

type Color = { red: int; green: int; blue: int; alpha: float; }

type AlignmentValue =
    | BottomLeft = 1
    | BottomCenter = 2
    | BottomRight = 3
    | CenterLeft = 4
    | Center = 5
    | CenterRight = 6
    | TopLeft = 7
    | TopCenter = 8
    | TopRight = 9

type LegacyAlignmentValue =
    | BottomLeft = 1
    | BottomCenter = 2
    | BottomRight = 3
    | CenterLeft = 9
    | Center = 10
    | CenterRight = 11
    | TopLeft = 5
    | TopCenter = 6
    | TopRight = 7

type WrappingStyleValue =
    | SmartWrappingWithWiderTopLine = 0
    | SmartWrappingWithWiderBottomLine = 3
    | EndOfLineWrapping = 1
    | NoLineWrapping = 2

type Comment = { value: string; }
and Text = { value: string; }
and NewLine = new() = {}
and Italic = { value: EnableDisable option; }
and Bold = { value: BoldValue option; }
and Underline = { value: EnableDisable option; }
and StrikeThrough = { value: EnableDisable option; }
and Border = { value: float option; }
and BorderX = { value: float option; }
and BorderY = { value: float option; }
and Shadow = { value: float option; }
and ShadowX = { value: float option; }
and ShadowY = { value: float option; }
and Blur = { value: float option; }
and GaussianBlur = { value: float option; }
and FontName = { value: string option; }
and FontSize = { value: float option; }
and FontSizePlus = { value: float; }
and FontSizeMinus = { value: float; }
and FontScaleX = { value: float option; }
and FontScaleY = { value: float option; }
and LetterSpacing = { value: float option; }
and RotateX = { value: float option; }
and RotateY = { value: float option; }
and RotateZ = { value: float option; }
and SkewX = { value: float option; }
and SkewY = { value: float option; }
and PrimaryColor = { value: Color option; }
and SecondaryColor = { value: Color option; }
and OutlineColor = { value: Color option; }
and ShadowColor = { value: Color option; }
and Alpha = { value: float option; }
and PrimaryAlpha = { value: float option; }
and SecondaryAlpha = { value: float option; }
and OutlineAlpha = { value: float option; }
and ShadowAlpha = { value: float option; }
and Alignment = { value: AlignmentValue; }
and ColorKaraoke = { duration: float; }
and SweepingColorKaraoke = { duration: float; }
and OutlineKaraoke = { duration: float; }
and WrappingStyle = { value: WrappingStyleValue; }
and Reset = { styleName: string option; }
and Position = { x: float; y: float; }
and Move = { x1: float; y1: float; x2: float; y2: float; t1: float option; t2: float option; }
and RotationOrigin = { x: float; y: float; }
and Fade = { start: float; ``end``: float; }
and ComplexFade = { a1: float; a2: float; a3: float; t1: float; t2: float; t3: float; t4: float; }
and Transform = { start: float option; ``end``: float option; accel: float option; tags: TransformableTag list; }
and RectangularClip = { x1: float; y1: float; x2: float; y2: float; inside: bool; }
and VectorClip = { scale: float; (* instructions: ???; *) inside: bool; }
and DrawingMode = { scale: float; }
and DrawingBaselineOffset = { value: float; }

and Part =
    | Comment of Comment
    | Text of Text
    | NewLine of NewLine
    | Italic of Italic
    | Bold of Bold
    | Underline of Underline
    | StrikeThrough of StrikeThrough
    | Border of Border
    | BorderX of BorderX
    | BorderY of BorderY
    | Shadow of Shadow
    | ShadowX of ShadowX
    | ShadowY of ShadowY
    | Blur of Blur
    | GaussianBlur of GaussianBlur
    | FontName of FontName
    | FontSize of FontSize
    | FontSizePlus of FontSizePlus
    | FontSizeMinus of FontSizeMinus
    | FontScaleX of FontScaleX
    | FontScaleY of FontScaleY
    | LetterSpacing of LetterSpacing
    | RotateX of RotateX
    | RotateY of RotateY
    | RotateZ of RotateZ
    | SkewX of SkewX
    | SkewY of SkewY
    | PrimaryColor of PrimaryColor
    | SecondaryColor of SecondaryColor
    | OutlineColor of OutlineColor
    | ShadowColor of ShadowColor
    | Alpha of Alpha
    | PrimaryAlpha of PrimaryAlpha
    | SecondaryAlpha of SecondaryAlpha
    | OutlineAlpha of OutlineAlpha
    | ShadowAlpha of ShadowAlpha
    | Alignment of Alignment
    | ColorKaraoke of ColorKaraoke
    | SweepingColorKaraoke of SweepingColorKaraoke
    | OutlineKaraoke of OutlineKaraoke
    | WrappingStyle of WrappingStyle
    | Reset of Reset
    | Position of Position
    | Move of Move
    | RotationOrigin of RotationOrigin
    | Fade of Fade
    | ComplexFade of ComplexFade
    | Transform of Transform
    | RectangularClip of RectangularClip
    | VectorClip of VectorClip
    | DrawingMode of DrawingMode
    | DrawingBaselineOffset of DrawingBaselineOffset
    //| DrawingInstructions of ???

and TransformableTag =
    | Border of Border
    | BorderX of BorderX
    | BorderY of BorderY
    | Shadow of Shadow
    | ShadowX of ShadowX
    | ShadowY of ShadowY
    | Blur of Blur
    | GaussianBlur of GaussianBlur
    | FontSize of FontSize
    | FontSizePlus of FontSizePlus
    | FontSizeMinus of FontSizeMinus
    | FontScaleX of FontScaleX
    | FontScaleY of FontScaleY
    | LetterSpacing of LetterSpacing
    | RotateX of RotateX
    | RotateY of RotateY
    | RotateZ of RotateZ
    | SkewX of SkewX
    | SkewY of SkewY
    | PrimaryColor of PrimaryColor
    | SecondaryColor of SecondaryColor
    | OutlineColor of OutlineColor
    | ShadowColor of ShadowColor
    | Alpha of Alpha
    | PrimaryAlpha of PrimaryAlpha
    | SecondaryAlpha of SecondaryAlpha
    | OutlineAlpha of OutlineAlpha
    | ShadowAlpha of ShadowAlpha
    | RectangularClip of RectangularClip
