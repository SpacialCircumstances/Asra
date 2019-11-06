module Errors

let collectErrors (results: Result<'a, 'b> list): 'a list * 'b list =
    List.fold (fun (oks, errs) r -> 
        match r with
            | Ok o -> o :: oks, errs
            | Error e -> oks, e :: errs) ([], []) results