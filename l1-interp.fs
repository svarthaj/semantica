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
    | Op of term * operator * term
    | If of term * term * term
    | Var of variable
    | Aplic of term * term
    | Fn of variable * term
    | Let of variable * term * term
    | LRec of variable * term * term 
    | Cons of term * term
    | Nil
    | Isempty of term
    | Hd of term
    | Tl of term
    | Raise
    | Try of term * term

(* Excessao a ser ativada quando o termo for uma FORMA NORMAL
    - Pode ser um VALOR
    - ou, um ERRO DE EXECUCAO *)
exception NoRuleApplies

(* funcao auxiliar : verifica se termo é valor inteiro *)
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
    | Cons(_,_) -> true
    | Nil -> true
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
        | If(Raise, t2, t3) -> Raise
        | If(t1, t2, t3) ->
            let t1' = step t1 in If(t1', t2, t3)
        | Op(Num(v1), Sum, Num(v2)) ->
            let res = v1+v2 in Num(res) 
        | Op(Num(v1), Sub, Num(v2)) ->
            let res = v1-v2 in Num(res) 
        | Op(Num(v1), Mul, Num(v2)) ->
            let res = v1*v2 in Num(res) 
        | Op(Num(v1), Div, Num(v2)) ->  match v2 with
                                        | 0 -> Raise
                                        | _ -> let res = v1/v2 in Num(res) 
        | Op(Num(v1), Lt, Num(v2)) ->
            let res = (v1 < v2) in Bool(res)
        | Op(Num(v1), Gt, Num(v2)) ->
            let res = (v1 > v2) in Bool(res)
        | Op(Num(v1), Leq, Num(v2)) ->
            let res = (v1 <= v2) in Bool(res)
        | Op(Num(v1), Geq, Num(v2)) ->
            let res = (v1 >= v2) in Bool(res)
        | Op(Num(v1), Eq, Num(v2)) ->
            let res = (v1 = v2) in Bool(res)
        | Op(Num(v1), Neq, Num(v2)) ->
            let res = (v1 <> v2) in Bool(res)
        | Op(Num(_), op, Raise) -> Raise
        | Op(Num(v1), op, t2) -> 
            let t2' = step t2 in Op(Num(v1), op, t2') 
        | Op(Raise, op, t2) -> Raise
        | Op(t1, op, t2) ->
            let t1' = step t1 in Op(t1', op, t2) 
        | Aplic(Fn(var, t1), t2) when isvalue t2 -> subst (t2, var, t1)  
        | Aplic(Fn(var, t1), Raise) -> Raise  
        | Aplic(Fn(var, t1), t2) ->
            let t2' = step t2 in Aplic(Fn(var, t1), t2')
        | Aplic(Raise, t2) -> Raise
        | Aplic(t1, t2) ->
            let t1' = step t1 in Aplic(t1', t2)
        | Let(var, Raise, t2) -> Raise
        | Let(var, t1, t2) when isvalue t1 -> subst(t1, var, t2)
        | Let(var, t1, t2) ->
            let t1' = step t1 in Let(var, t1', t2)
        | LRec(var1, Fn(var2, t1), t2) -> 
            let recur = LRec(var1, Fn(var2, t1), t1) in
                subst(Fn(var2, recur), var1, t2)  
        | Try(Raise, e2) -> e2
        | Try(e1, e2) when isvalue e1 -> e1 
        | Try(e1, e2) -> let e1' = step e1 in Try(e1',e2)
        | Cons(e1, e2) when isvalue e1 ->   match e2 with
                                            | Raise -> Raise
                                            | _ -> let e2' = step e2 in Cons(e1, e2') 
        | Cons(Raise, e2) -> Raise
        | Cons(e1, e2) -> 
            let e1' = step e1 in Cons(e1',e2)
        | Hd(Cons(e1,_)) -> e1
        | Hd(Nil) -> Raise
        | Hd(Raise) -> Raise
        | Hd(e1) -> let e1' = step e1 in Hd(e1') 
        | Tl(Cons(_,e1)) -> e1
        | Tl(Nil) -> Raise
        | Tl(Raise) -> Raise
        | Tl(e1) -> let e1' = step e1 in Tl(e1') 
        | Isempty(e1) ->    match e1 with
                            | Nil -> Bool(true)
                            | Cons(_,_) -> Bool(false)
                            | _ -> Raise
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
let e13 = Try(Op(Num 10, Div, Num 0), Var "err: div by zero")
let e14 = Try(Op(Op(Num 10, Div, Num 0), Sum, Num 5), Var "err: div by zero")
let e15 = Cons(Num 5, Cons(Num 10, Nil))
let e16 = Hd(Cons(Num 5, Cons(Num 10, Nil)))
let e17 = Tl(Cons(Num 5, Cons(Num 10, Nil)))
let e18 = Hd(Nil)
let e19 = Tl(Nil)
let e20 = Isempty(Cons(Num 5, Cons(Num 10, Nil)))
let e21 = Isempty(Nil)
(* AVALIAÇÕES *)
let ev1 = eval e20 in printfn "%A evaluates to %A" e20 ev1
let ev2 = eval e21 in printfn "%A evaluates to %A" e21 ev2
