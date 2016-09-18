/// Here is an alternative way of doing pipes. Fparsec would be better if it replaced what it did today with the following
/// technique. The Pipes library has more elegant syntax, but in terms simplicity, the code below is hard to beat.

type Stream() = class end

let a (s: Stream) = 1
let b (s: Stream) = "asd"
let c (s: Stream) = 5.5

let (<?|) (cont: Stream -> _) (fn: Stream -> _) (s: Stream) =
    let cont = cont s
    let fn = fn s
    cont fn

let f k = k <?| a <?| b <?| c
let runStream f k = f <| (fun s -> k) <| Stream()

let r = runStream f <| fun a b c -> b
