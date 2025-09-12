module FSharpBits.ForFunAndProfit.Catamorphism.FileSystem.SampleValues

open FileSystem

let readme = File { name = "readme.txt"; fileSize = 1 }
let config = File { name = "config.xml"; fileSize = 2 }
let build = File { name = "build.bat"; fileSize = 3 }

(*
root (dir, size=5)
├── src (dir, size=10)
│   ├── readme.txt (file, size=1)
│   ├── config.xml (file, size=2)
│   └── build.bat (file, size=3)
└── bin (dir, size=10)
*)

let src =
    Directory
        { name = "src"
          dirSize = 10
          subItems = [ readme; config; build ] }

let bin =
    Directory
        { name = "bin"
          dirSize = 10
          subItems = [] }

let root =
    Directory
        { name = "root"
          dirSize = 5
          subItems = [ src; bin ] }
