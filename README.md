This repository is no longer maintained.

If you have questions about this repository, open an issue at https://github.com/Arnavion/archived-repos-issues

---

An F# port of some of [libjass](https://github.com/Arnavion/libjass) that uses FunScript to compile itself to JavaScript.


### Build

```batchfile
REM In "Developer Command Prompt for VS 2015"
msbuild .\libjass-fs.sln
.\libjass-fs\bin\Debug\libjass-fs.exe
REM Now have .\src\bin\Debug\libjass.js
```


### License

```
libjass-fs

https://github.com/Arnavion/libjass-fs

Copyright 2015 Arnav Singh

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

FunScript is used to compile the code to JavaScript and is available under the Apache-2.0 license.
