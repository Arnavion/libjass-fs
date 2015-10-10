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

module js_compiler

[<EntryPoint>]
let main args =
    let source = FunScript.Compiler.Compiler.Compile(<@ parser.parse @>, noReturn = true, components =
        [
            FunScript.ExpressionReplacer.createUnsafe <@ OperatorIntrinsics.GetStringSlice @> <@ platform.Native.OperatorIntrinsics_GetStringSlice @>
            FunScript.ExpressionReplacer.createUnsafe <@ platform.Managed.StringToHexInt32 @> <@ platform.Native.StringToHexInt32 @>
            FunScript.ExpressionReplacer.createUnsafe <@ fun (value: int32) -> enum<SourceConstructFlags> value @> <@ platform.Native.Operator_enum @>
            FunScript.ExpressionReplacer.createUnsafe <@ fun (value: char) -> System.Convert.ToInt32(value) @> <@ platform.Native.Convert_ToInt32 @>
            FunScript.ExpressionReplacer.createUnsafe <@ Set.contains @> <@ platform.Native.Set_contains @>
            FunScript.ExpressionReplacer.createUnsafe <@ Set.ofArray @> <@ platform.Native.Set_ofArray @>
        ])

    let wrapped = sprintf """/**
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
 */
(function (root, factory) {
    var global = this;
    if (typeof System === "object" && typeof System.register === "function") {
        System.register([], function ($__export) {
            $__export("default", factory(global));
            $__export("__useDefault", true);
            return {
                setters: function () {},
                execute: function () {}
            };
        });
    } else if (typeof exports === "object" && typeof module === "object") {
        module.exports = factory(global);
    } else if (typeof define === "function" && define.amd) {
        define([], function () {
            return factory(global);
        });
    } else if (typeof exports === "object") {
        exports["libjass"] = factory(global);
    } else {
        root["libjass"] = factory(global);
    }
})(this, function (global) {
%s
    return {
        parser: {
            parse: parser__parse
        }
    };
});
"""

    System.IO.File.WriteAllText(System.IO.Path.Combine(System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location), "libjass.js"), wrapped source)

    0
