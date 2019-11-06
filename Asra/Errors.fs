module Errors

let collectErrors (results: Result<'a, 'b> list): 'a list * 'b list =
    List.fold (fun (oks, errs) r -> 
        match r with
            | Ok o -> o :: oks, errs
            | Error e -> oks, e :: errs) ([], []) results

type ResultBuilder () =
    member this.Bind (r, f) = Result.bind f r
    member this.Return (x) = Ok x

let result = new ResultBuilder()