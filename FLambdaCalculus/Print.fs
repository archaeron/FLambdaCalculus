
module Lambda.Print

open Lambda.Eval

let rec printText term =
    match term with
    | Constant c ->
        match c with
        | Num n -> sprintf "%i" n
        | Add -> "+"
        | Succ -> "Succ"
        | IsZero -> "0"
    | Variable v -> v
    | Application (t1, t2) -> sprintf "(%s %s)" (printText t1) (printText t2)
    | Lambda (x, t) -> sprintf "λ%s . %s" x (printText t)


let rec printLatex term =
    match term with
    | Constant c ->
        match c with
        | Num n -> sprintf "%i" n
        | Add -> "+"
        | Succ -> "Succ"
        | IsZero -> "0"
    | Variable v -> v
    | Application (t1, t2) -> sprintf "(%s \; %s)" (printLatex t1) (printLatex t2)
    | Lambda (x, t) -> sprintf "[\\lambda %s . %s]" x (printLatex t)