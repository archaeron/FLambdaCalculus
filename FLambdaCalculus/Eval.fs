﻿
module Lambda.Eval
open System

type Const =
    | Num of int
    | Add
    | Succ
    | IsZero

type Var = string

type Term =
    | Constant of Const             // 5
    | Variable of Var               // A
    | Application of Term * Term    // (A B)
    | Lambda of Var * Term          // (\lambda x.A)

/// Union of two lists, remove the doubles
let rec (|||) xs =
    function
    | [] -> xs
    | y :: ys -> (|||) (y :: (List.filter (fun a -> a <> y) xs))  ys

/// Remove an element from a list
let (---) list element = List.filter (fun y -> y <> element) list

/// Free variables of a term (variables that are not bound by a lambda)
let rec freeVariables =
    function
    | Constant c -> []
    | Variable v -> [v]
    | Application (a, b) -> (freeVariables a) ||| (freeVariables b)
    | Lambda (x, b) -> (freeVariables b) --- x

/// Checks if a variable is free in a term
let isFreeVariableOf var = freeVariables >> List.exists (fun a -> a = var)

/// Create a new variable name, that is not free in a term 
let newVar term =
    let rec newVar' term nVar =
        if isFreeVariableOf nVar term
        then
            newVar' term (nVar+"1")
        else
            nVar
        
    newVar' term "new-var"

/// substitute a variable in a term with another term
let rec substitution term1 var term2 =
    match term1 with
    | Variable x when x = var -> term2
    | Application (t1, t2) -> Application (substitution t1 var term2, substitution t2 var term2)
    | Lambda (x, t) when x <> var && not (isFreeVariableOf x term2) -> Lambda (x, substitution t var term2)
    | Lambda (x, t) when x <> var ->
        let z = newVar (Application (t, term2))
        let subst1 = substitution t x (Variable z)
        let subst2 = substitution subst1 var term2
        Lambda (z, subst2) //todo
    | _ -> term1


/// apply parameters to a function
let betaConversion term =
    match term with
    | Application (Lambda(x, a), b) -> substitution a x b
    | _ -> term

let etaConversion term =
    match term with
    | Lambda (x, Application (t, Variable x2)) when x2 = x ->
        match t with
        | Constant (Num _) -> term
        | _ ->
            if not (isFreeVariableOf x t)
            then
                t
            else
                term
    | _ -> term

/// give the constants some meaning
let deltaConversion term =
    match term with
    | Application (Constant Succ, Constant (Num n)) -> Constant (Num (n + 1))
    | Application (Application (Constant Add, Constant (Num n)), Constant (Num m)) -> Constant (Num (n + m))
    | _ -> term

// helper functions
let number = Constant << Num
