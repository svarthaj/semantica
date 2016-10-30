﻿type variable = string

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

(* funcao auxiliar : verifica se é valor *)
let isvalue v =
    match v with
    | Num(_) -> true
    | Bool(_) -> true
    | Fn(_,_) -> true
    | Nil -> true
    | Raise -> true
    | _ -> false

(* funcao auxiliar : subsstitui as variaveis livres na expressao *)
let rec subst (v, x, e) =
    match e with
        | Num(n) -> Num(n)
        | Bool(b) -> Bool(b)
        | Op(e1, op, e2) -> 
            let e1' = subst (v, x, e1) in
                let e2' = subst(v, x, e2) in Op(e1', op, e2')
        | If(e1, e2, e3) ->
            let e1' = subst (v, x, e1) in
                let e2' = subst (v, x, e2) in
                    let e3' = subst (v, x, e3) in If(e1', e2', e3')
        | Aplic(e1, e2) ->
            let e1' = subst(v, x, e1) in
                let e2' = subst(v,x,e2) in Aplic(e1', e2')
        | Var(y) -> if y = x then v
                    else Var(y)  
        | Fn(y, e1) -> if y = x then e
                       else let e' = subst(v, x, e1) in Fn(y, e')   
        | Let(y, e1, e2) -> let e1' = subst(v, x, e1) in
                                if y = x then Let(y, e1', e2)
                                else let e2' = subst(v, x, e2) in
                                        Let(y, e1', e2')
        | LRec(y, Fn(k, e1), e2) -> if y = x then e
                                    else let fn' = subst(v, x, Fn(k, e1)) in
                                            let e2' = subst(v, x, e2) in 
                                                LRec(y, fn', e2') 
        | _ -> raise NoRuleApplies
            

(* Funcao STEP *)
let rec step t = 
    match t with
        | If(Bool(true), t2, t3) -> t2
        | If(Bool(false), t2, t3) -> t3
        | If(t1, t2, t3) ->
            let t1' = step t1 in If(t1', t2, t3)
        | Op(Num(v1), Sum, Num(v2)) ->
            let res = v1+v2 in Num(res) 
        | Op(Num(v1), Sub, Num(v2)) ->
            let res = v1-v2 in Num(res) 
        | Op(Num(v1), Mul, Num(v2)) ->
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
        | Op(Num(v1), Leq, Num(v2)) ->
            let res = (v1 <= v2) in
                match res with
                | true -> Bool(true)
                | _ -> Bool(false)
        | Op(Num(v1), Geq, Num(v2)) ->
            let res = (v1 >= v2) in
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
        | Op(Num(v1), op, t2) -> 
            let t2' = step t2 in Op(Num(v1), op, t2') 
        | Op(t1, op, t2) ->
            let t1' = step t1 in Op(t1', op, t2) 
        | Aplic(Fn(var, t1), t2) when isvalue t2 -> subst (t2, var, t1)  
        | Aplic(Fn(var, t1), t2) ->
            let t2' = step t2 in Aplic(Fn(var, t1), t2')
        | Aplic(t1, t2) ->
            let t1' = step t1 in Aplic(t1', t2)
        | Let(var, t1, t2) when isvalue t1 -> subst(t1, var, t2)
        | Let(var, t1, t2) ->
            let t1' = step t1 in Let(var, t1', t2)
        | LRec(var1, Fn(var2, t1), t2) -> 
            let recur = LRec(var1, Fn(var2, t1), t1) in
                subst(Fn(var2, recur), var1, t2)  
        | _ -> raise NoRuleApplies

(* Implentacao de EVAL *)
let rec eval t = 
    try let t' = step t in
        eval t'
    with NoRuleApplies -> t


(* TESTES *)
(* CASOS TESTE *)
let e1 = Op(Num(17), Sum, Num(9))
let e2 = Op(Op(Num(3), Sum, Num(6)), Mul, Num(9))
let e3 = Op(Op(Num(3), Sum, Num(6)), Mul, Op(Num(9), Sub, Num(4)))
let e4 = If(Op(Num(0), Lt, Num(10)), Bool(true), Bool(false))
let e5 = If(Op(Num(10), Leq, Num(4)), Bool(true), Bool(false))
let e6 = If(Op(Num(10), Eq, Num(4)), Bool(true), Bool(false))
let e7 = If(Op(Num(10), Neq, Num(4)), Bool(true), Bool(false))
let e8 = Aplic(Fn("x", Var("x")), Num(10))
let e9 = Aplic(If(Bool(false), Fn("x", Var("x")), Fn("x",Op(Var("x"),Sub,Num(5)))), Num(10))
let e10 = Let("x", Num(1), Op(Var("x"), Sum, Num(10)))
let e11 = Let("x", Op(Num(5), Sub, Num(4)), Op(Var("x"), Sub, Num(10)))
(* exemplo fatorial recursivo *)
let e12 = LRec(
            "fat", 
            Fn( 
                "x",
                If(
                    Op(Var "x", Eq, Num 0),
                    Num 1,
                    Op( Var "x", Mul, Aplic(Var "fat", Op(Var "x", Sub, Num 1)))                )
            ), 
            Aplic(Var "fat", Num 12))
(* AVALIAÇÕES *)
let ev = eval e12 in printfn "%A evaluates to %A" e12 ev
