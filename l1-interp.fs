type variable = string

type operator =
    | Sum
    | Sub
    | Mul
    | Div
    | Eq
    | Neq
    | Lt
    | Gt
    | Leq
    | Geq
    
type term = 
    | Num of int
    | Bool of bool
    | Nil
    | Op of term * operator * term
    | If of term * term * term
    | Var of variable
    | Aplic of term * term
    | Fn of variable * term
    | Let of variable * term * term
    | LRec of variable * term * term 
    | List of term * term
    | Isempty of term
    | Hd of term
    | Tl of term
    | Raise
    | Try of term * term

(* Excessao a ser ativada quando o termo for uma FORMA NORMAL
    - Pode ser um VALOR
    - ou, um ERRO DE EXECUCAO *)
exception NoRuleApplies

(* funcao auxiliar : verifica se valor é inteiro *)
let isint v = 
    match v with
        | Num(_) -> true
        | _ -> false

(* Funcao STEP *)
let rec step t = 
    match t with
        | If(Bool(true), t2, t3) -> t2
        | If(Bool(false), t2, t3) -> t3
        | If(t1, t2, t3) ->
            let t1' = step t1 in If(t1', t2, t3)
        | Op(Num(v1), Sum, Num(v2)) when (isint(Num(v1)) && isint(Num(v2))) ->
            let res = v1+v2 in Num(res) 
        | Op(Num(v1), Sub, Num(v2)) when (isint(Num(v1)) && isint(Num(v2))) ->
            let res = v1-v2 in Num(res) 
        | Op(Num(v1), Mul, Num(v2)) when (isint(Num(v1)) && isint(Num(v2))) ->
            let res = v1*v2 in Num(res) 
        | Op(Num(v1), Lt, Num(v2)) ->
            let res = (v1 < v2) in
                match res with
                | true -> Bool(true)
                | _ -> Bool(false)
        | Op(Num(v1), Gt, Num(v2)) ->
            let res = (v1 > v2) in
                match res with
                | true -> Bool(true)
                | _ -> Bool(false)
        | Op(Num(v1), Lte, Num(v2)) ->
            let res = (v1 >= v2) in
                match res with
                | true -> Bool(true)
                | _ -> Bool(false)
        | Op(Num(v1), Gte, Num(v2)) ->
            let res = (v1 <= v2) in
                match res with
                | true -> Bool(true)
                | _ -> Bool(false)
        | Op(Num(v1), Eq, Num(v2)) ->
            let res = (v1 = v2) in
                match res with
                | true -> Bool(true)
                | _ -> Bool(false)
        | Op(Num(v1), Neq, Num(v2)) ->
            let res = (v1 <> v2) in
                match res with
                | true -> Bool(true)
                | _ -> Bool(false)
        | Op(t1, op, t2) ->
            let t1' = step t1 in Op(t1', op, t2) 
        | Op(Num(v1), op, t2) -> 
            let t2' = step t2 in Op(Num(v1), op, t2') 
        | _ -> raise NoRuleApplies

(* Implentacao de EVAL *)
let rec eval t = 
    try let t' = step t in
        eval t'
    with NoRuleApplies -> t


(* TESTES *)
let y = Op(Num(17), Sum, Num(9)) in
    let x = eval y in
        printfn "%A evaluates to %A" y x 

let y = Op(Op(Num(3), Sum, Num(6)), Mul, Num(9)) in
    let x = eval y in
        printfn "%A evaluates to %A" y x 

let y = If(Op(Num(0), Lt, Num(10)), Bool(true), Bool(false)) in
    let x = eval y in
        printfn "%A evaluates to %A" y x 
