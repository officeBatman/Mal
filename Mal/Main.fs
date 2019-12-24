namespace Mal

type id = string
type typeError = string
module private Result = 
    let bind2 (f:'a*'b->Result<'res,'err list>) (r1,r2) =
        match r1, r2 with
        | Error e1, Error e2 -> Error (e1 @ e2)
        | Error e, _ | _, Error e -> Error e
        | Ok a, Ok b -> f (a,b)
    let rec reduce r : Result<'res list,'err list> =
        let rec f r = 
            match r with
            | [] -> [], []
            | a1::r' -> 
                let oks, errors = f r
                match a1 with
                | Ok a1 -> a1::oks, errors
                | Error a1 -> oks, a1@errors
        match f r with
        | a, [] -> Ok a
        | _, a -> Error a

type [<StructuralEquality; NoComparison>] ``type``=
    ///<summary>An interface for using and repressenting f#-like functions</summary>
    | Lambda of ``type``*``type``
    ///<summary>A user-defined type with structural limitations through generated functions</summary>
    | Category of id
    //static member CurriedLambda (list:_ list) = List.fold (fun x y -> Lambda (x,y)) list.Head list.Tail
    override t.ToString() =
        match t with
        | Lambda (t1,t2) -> sprintf "%O->%O" t1 t2
        | Category id -> id
//let rec (|CurriedLambda|) = function
//    | Lambda (t1,CurriedLambda ts) -> t1::ts
//    | t -> [t]
type [<CustomEquality; NoComparison>] value =
    ///<summary>An instance of a Lambda type</summary>
    | Function of (value->value)
    ///<summary>An undividable unit of abstraction. Repressentation is decided by the category of the object</summary>
    | Atom of id
    ///<summary>A unit of abstraction. Repressentation is decided by the category of the object</summary>
    | Term of id*value
    override t.ToString() =
        match t with
        | Function f -> sprintf "function (%A)" f
        | Term (id,v) -> sprintf "%s %O" id v
        | Atom id -> sprintf "%s" id
    ///<summary>Weather two expressions are equal. All expressions with functions are not equal, as functional equality can not be created</summary>
    override a.Equals b =
        match b with
        | :? value as b ->
            match a, b with
            | Atom m, Atom n when m = n -> true
            | Term (m1,m2), Term (n1,n2) when m1 = n1 && m2 = n2 -> true
            | Function m, Function n (*Cannot determine functional equality*) -> true
            | _ -> false
        | _ -> false
    override a.GetHashCode () = invalidOp "value.GetHashCode is not implemented"

///<summary>A computable expression. Can be computed to a value</summary>
type expr =
    ///<summary>A predecided value of a given type</summary>
    | Constent of ``type``*value
    ///<summary>Applies the second expr's value to the first expr's function</summary>
    | Application of expr*expr
    ///<summary>Returns the value stored under the id in the current context</summary>
    | Var of id
    ///<summary>Returns a function which tries to find a matching pattern for the given value and returns the fitting expression</summary>
    //| Match of (pattern*expr) list
    
///<summary>A pattern is used in code to understand the structure of values and for control flow</summary>
and pattern =
    | TermPattern of id*pattern
    | EqualPattern of expr
    ///<summary>Always matches. Adds a new variable to the context of the given id</summary>
    | BindPattern of id
    
and declaration =
    ///<summary>Adds a variable to the current context</summary>
    | VarDeclaration of id*expr
    ///<summary>Generates functions returning the category and variables of the type of the category and adds them to the context</summary>
    | CategoryDeclaration of id*(id*``type``*(pattern*expr) option) list

///<summary>A block is a collection of declarations and a return expression</summary>
type block = | Block of declaration list*expr

module Expr =
    ///<summary>Returns the type of the value that the expression returns</summary>
    let rec typeOf (vars:Map<id,``type``>) (expr:expr) :Result<``type``,typeError list> =
        match expr with
        | Constent (t,_) -> Ok t
        | Application (e1,e2) ->
            (typeOf vars e1,typeOf vars e2) |> Result.bind2 (function
                | Lambda (input,output) as lambda, input' ->
                    if input' = input then Ok output
                    else Error [sprintf "Type %O cannot be applied to %O" input' lambda]
                | t1, _ -> Error [sprintf "%O cannot be applied" t1])
        | Var v -> 
            match vars.TryFind v with
            | Some v -> Ok v
            | None -> Error [sprintf "Variable %s is not defined in the current scope" v]
    ///<summary>Computes the value of the expression</summary>
    let rec execute (vars:Map<id,value>) (expr:expr) : value =
        match expr with
        | Constent (_,v) -> v
        | Var v -> vars.[v]
        | Application (e1,e2) ->
            match execute vars e1, execute vars e2 with
            | Function f, x -> f x
            | e1, e2 -> failwithf "This line should not be reached! Tried applying %O to %O" e2 e1
        //| Match

module Pattern =
    let rec declareTypes (vars:Map<id,``type``>) (pattern:pattern) (``type``:``type``) :Result<_,typeError list> =
        match pattern with
        | BindPattern v -> (vars.Remove v).Add (v,``type``) |> Ok
        | TermPattern (id,pattern) -> 
            match vars.TryFind id with
            | Some ``type`` ->
                declareTypes vars pattern ``type``
            | None -> Error ["A helpful error message"]
        | EqualPattern expr -> 
            Expr.typeOf vars expr |> Result.bind (fun expected ->
                if ``type`` = expected then Ok vars else Error [sprintf "This expression was expected to have type %O but had %O" expected ``type``])
    let rec matchPattern (vars:Map<id,value>) (pattern:pattern) (value:value) :Map<id,value> option =
        match pattern with
        | BindPattern v -> Some ((vars.Remove v).Add (v,value))
        | TermPattern (id,pattern) ->
            match value with
            | Term (id',value) when id = id' -> matchPattern vars pattern value
            | _ -> None
        | EqualPattern expr -> 
            let expected = Expr.execute vars expr
            if value = expected then Some vars else None

module Declaration =
    let rec declareTypes (vars:Map<id,``type``>) (dec:declaration) :Result<Map<id,``type``>,typeError list> =
        match dec with
        | VarDeclaration (id,e) -> 
            Expr.typeOf vars e |> Result.map (fun e -> (vars.Remove id).Add (id,e))
        | CategoryDeclaration (id,e) -> 
            List.map (fun (id,t,_) -> id, t) e
            |> List.fold (fun (vars:Map<_,_>) x -> (vars.Remove (fst x)).Add x) vars
            |> Ok
    let rec execute (vars:Map<id,value>) (dec:declaration) :Map<id,value> =
        match dec with
        | VarDeclaration (id,e) ->
            (vars.Remove id).Add (id, Expr.execute vars e)
        | CategoryDeclaration (name,e) ->
            let dVars = e |> List.map (fun (id,``type``,p) -> 
                match ``type`` with
                | Category c when c = name -> id, Atom id
                | Lambda (_,Category c) when c = name -> 
                    let f = 
                        match p with 
                        | Some (pattern,expr) -> 
                            Function (fun value -> 
                                let term = Term (id,value)
                                Pattern.matchPattern vars pattern term |> function 
                                    | Some vars -> Expr.execute vars expr 
                                    | None -> term) 
                        | None -> 
                            Function (fun value -> Term (id,value)) 
                    id, f)
            List.fold (fun vars o -> (vars.Remove (fst o)).Add o) vars dVars

module Block =
    let rec typeOf (vars:Map<id,``type``>) (block:block) :Result<``type``,typeError list> =
        let (Block (declarations,expr)) = block
        match declarations with
        | [] -> Expr.typeOf vars expr
        | declaration1::declarations -> 
            Declaration.declareTypes vars declaration1 |> Result.bind (fun vars' ->
                typeOf vars' (Block (declarations,expr)))
    let execute (vars:Map<id,value>) (block:block) :value =
        let (Block (declarations,expr)) = block
        let vars' = List.fold Declaration.execute vars declarations
        Expr.execute vars' expr
