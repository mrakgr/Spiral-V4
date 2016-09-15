#if INTERACTIVE
#load "load-project-release.fsx"
#endif

open System
open System.Collections.Generic
open FParsec
open FParsec.Pipes

type FutharkType =
| FuthInt8
| FuthInt16
| FuthInt32
| FuthInt64
| FuthUInt8
| FuthUInt16
| FuthUInt32
| FuthUInt64
| FuthFloat32
| FuthFloat64
| FuthBool
| FuthIntDefault
| FuthFloatDefault
| FuthIntAlias
| FuthUndefined

type UserState =
    {
    state : FutharkType
    max_depth : int option
    tree_sizes : Map<int,int>
    }

    static member Default = {state = FuthUndefined; max_depth=None; tree_sizes=Map.empty}

type FutharkArray =
    | Int8Ar of int8[]
    | Int16Ar of int16[]
    | Int32Ar of int32[]
    | Int64Ar of int64[]
    | UInt8Ar of uint8[]
    | UInt16Ar of uint16[]
    | UInt32Ar of uint32[]
    | UInt64Ar of uint64[]
    | Float32Ar of float32[]
    | Float64Ar of float[]
    | BoolAr of bool[]
    | NestedAr of FutharkArray[]

/// The template function for Futhark value parsers.
/// Note: Empty separator that always returns success is not the same as no separators.
let parseTempl can_change_max_depth (sep: Parser<unit,UserState> option) (p: Parser<_,UserState>) depth (str: CharStream<UserState>) = 
    match str.UserState.max_depth with
    | Some max_depth when depth > max_depth ->
        Reply(Error, expected "Depth exceeded. Array is irregular.")
    | _ ->
        match str.UserState.tree_sizes.TryFind depth with
        | Some v -> 
            match sep with
            | Some sep -> qty.[v] / sep * p 
            | None -> qty.[v] * p
        | None -> 
            match sep with
            | Some sep -> qty.[1..] / sep * p 
            | None -> qty.[1..] * p
            |>> fun x ->
                if can_change_max_depth && str.UserState.max_depth.IsNone then 
                    str.UserState <- {str.UserState with max_depth = Some depth}
                str.UserState <- {str.UserState with tree_sizes = 
                    str.UserState.tree_sizes.Add(depth, x.Count)}
                x.ToArray()
        <| str

let sep = %% spaces -- ',' -- spaces -|> ()
let parseInt8Ar depth = parseTempl true (Some sep)   (pint8 .>> pstring "i8") depth |>> Int8Ar
let parseInt16Ar depth = parseTempl true (Some sep) (pint16 .>> pstring "i16") depth |>> Int16Ar
let parseInt32Ar depth = parseTempl true (Some sep) (pint32 .>> pstring "i32") depth |>> Int32Ar
let parseIntAliasAr depth = parseTempl true (Some sep) (pint32 .>> pstring "int") depth |>> Int32Ar
let parseInt64Ar depth = parseTempl true (Some sep) (pint64 .>> pstring "i64") depth |>> Int64Ar
let parseUInt8Ar depth = parseTempl true (Some sep)   (puint8 .>> pstring "u8") depth |>> UInt8Ar
let parseUInt16Ar depth = parseTempl true (Some sep) (puint16 .>> pstring "u16") depth |>> UInt16Ar
let parseUInt32Ar depth = parseTempl true (Some sep) (puint32 .>> pstring "u32") depth |>> UInt32Ar
let parseUInt64Ar depth = parseTempl true (Some sep) (puint64 .>> pstring "u64") depth |>> UInt64Ar
let parseFloat32Ar depth = parseTempl true (Some sep) (pfloat .>> pstring "f32" |>> float32) depth |>> Float32Ar
let parseFloat64Ar depth = parseTempl true (Some sep) (pfloat .>> pstring "f64") depth |>> Float64Ar
let parseBoolAr depth = 
    let tr = pstringCI "true" |>> (fun _ -> true)
    let fl = pstringCI "false" |>> (fun _ -> false)
    parseTempl true (Some sep) (tr <|> fl) depth |>> BoolAr

let parseIntDefaultAr depth = parseTempl true (Some sep) (pint32) depth |>> Int32Ar
let parseFloatDefaultAr depth = parseTempl true (Some sep) (pfloat) depth |>> Float64Ar

let parseAnyNum depth (str: CharStream<UserState>) =
    match str.UserState.state with
    | FuthInt8 -> parseInt8Ar depth
    | FuthInt16 -> parseInt16Ar depth
    | FuthInt32 -> parseInt32Ar depth
    | FuthInt64 -> parseInt64Ar depth
    | FuthUInt8 -> parseUInt8Ar depth
    | FuthUInt16 -> parseUInt16Ar depth
    | FuthUInt32 -> parseUInt32Ar depth
    | FuthUInt64 -> parseUInt64Ar depth
    | FuthFloat32 -> parseFloat32Ar depth
    | FuthFloat64 -> parseFloat64Ar depth
    | FuthBool -> parseBoolAr depth
    | FuthIntAlias -> parseIntAliasAr depth
    | FuthIntDefault -> parseIntDefaultAr depth
    | FuthFloatDefault -> parseFloatDefaultAr depth
    | FuthUndefined ->
        let inline c p u = // Just a little helper so I do not have to write up updateUserState. It came in handy for attempt too.
            attempt p .>> updateUserState (fun us -> {us with state = u})
        choice [
            c (parseInt8Ar depth) FuthInt8
            c (parseInt16Ar depth) FuthInt16
            c (parseInt32Ar depth) FuthInt32
            c (parseInt64Ar depth) FuthInt64
            c (parseUInt8Ar depth) FuthUInt8
            c (parseUInt16Ar depth) FuthUInt16
            c (parseUInt32Ar depth) FuthUInt32
            c (parseUInt64Ar depth) FuthUInt64
            c (parseFloat32Ar depth) FuthFloat32
            c (parseFloat64Ar depth) FuthFloat64
            c (parseBoolAr depth) FuthBool
            c (parseIntAliasAr depth) FuthIntAlias
            c (parseIntDefaultAr depth .>> notFollowedByString ".") FuthIntDefault
            c (parseFloatDefaultAr depth) FuthFloatDefault
            ]
    <| str

let parseAnyAr (p: int -> Parser<_,UserState>) depth =
    (%% '[' -- spaces -- +.(p depth) -- ']' -|> id)
    |> fun p -> parseTempl false None p depth
    |>> NestedAr 

/// The main value parser function.
let rec parseAny depth (str: CharStream<UserState>): Reply<FutharkArray> =
    match str.UserState.max_depth with
    | Some max_depth -> 
        if max_depth-1 = depth then 
            parseAnyNum (depth+1) // Don't mind parseAnyNum depth. This is needed so the parser works correctly.
        else 
            parseAnyAr parseAny (depth+1)
    | _ -> 
        parseAnyAr parseAny (depth+1) <|> parseAnyNum (depth+1)
    <| str

let str = "[[1,2]][[1,2]]"

let r = runParserOnString (parseAny 0) UserState.Default "" str

open Fuchu

let getSuccess r =
    match r with
    | Success(x,_,_) -> x
    | Failure(_,_,_) -> failwith "asd"

let tests = 
    testList "Tests" [
        testCase "" <|
            fun _ -> Assert.Equal("",
                NestedAr [|NestedAr [|Float64Ar [|1.3; 2.5|]|]|],
                runParserOnString (parseAny 0) UserState.Default "" "[[1.3,2.5]]" |> getSuccess)
        testCase "" <|
            fun _ -> Assert.Equal("",
                NestedAr [|NestedAr [|Int32Ar [|1; 2|]|]|],
                runParserOnString (parseAny 0) UserState.Default "" "[[1,2]]" |> getSuccess)
        testCase "" <|
            fun _ -> Assert.Equal("",
                NestedAr [|Int32Ar [|1; 2|]; Int32Ar [|3; 4|]|],
                runParserOnString (parseAny 0) UserState.Default "" "[1,2][3,4]" |> getSuccess)
        testCase "" <|
            fun _ -> Assert.Equal("",
                NestedAr [|NestedAr [|Int32Ar [|1; 2|]|]; NestedAr [|Int32Ar [|1; 2|]|]|],
                runParserOnString (parseAny 0) UserState.Default "" "[[1,2]][[1,2]]" |> getSuccess)
        ]

run tests
