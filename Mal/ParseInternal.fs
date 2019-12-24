module internal Mal.ParseInternal

open Mal
open APC.Main
open APC.ExprTree

let pletter = pchars "letter" (['a'..'z']@['A'..'Z'])
let pdigit = pchars "digit" ['0'..'9']
let psymbol = pchars "symbol" ['+'; '-'; '*'; '/']
let pid = (prepeat1 (pletter<|>pdigit)<|>(pchar '`' >>. prepeat (psymbol<|>pletter<|>pdigit<|>pspace) .>> pchar '`')) |> map (List.map string>>List.reduce (+):char list->id) |> setLabel "identifier" |> setFail "Expected an identifier"
//  let pnumber = prepeat1 pdigit |> map (List.map string>>List.reduce (+)>>System.Int32.Parse) |> setLabel "number"

let pwrap l r p = pchar l >>. pspaces >>. p .>> pspacesOrLines .>> pchar r
let ptype, pitype = recParser "Type expression" 
do 
    let ptypeAtom = (pid |> map Category) <|> (pwrap '(' ')' ptype) |> setLabel "Type expression"
    pitype <| ((ptypeAtom .>> pspaces .>> pstr "->" .>> pspaces .>>. ptype |> map Lambda) <|> ptypeAtom)
let pexpr, piexpr = recParser "Expression"
do 
    let pexprAtom = (map Var pid) <|> pwrap '(' ')' pexpr |> setLabel "Expression" |> setFail "Expected an expression atom"
    piexpr <| ( 
        (pexprAtom .>>. prepeat1 (pspaces >>. pexprAtom) |> map (fun (x,y) -> List.fold (fun m n -> Application (m,n)) x y)) 
        <|> pexprAtom)
let ppattern, pipattern = recParser "Pattern"
do 
    let ppatternAtom = (pid |> map BindPattern) (*<|> (pid |>)*) <|> pwrap '(' ')' ppattern |> setLabel "Pattern"
    pipattern <| ((pid .>> pspaces .>>. ppattern |> map TermPattern) <|> ppatternAtom)
let pcase = pchar '|' >>. pspaces >>. ppattern .>> pspaces .>> pstr "->" .>> pspacesOrLines .>>. pexpr
let pdeclare =
    let pdeclareCategory =
        let pmember = pid .>> pspaces .>> pchar ':' .>> pspaces .>>. ptype |> setLabel "member"
        let pmembers = (pmember .>>. prepeat (pspaces >>. pchar ',' >>. pspacesOrLines >>. pmember)) |> map (fun (a,b) -> a::b)
        ((pstr "declare" >>. pspaces >>. pid .>> pspaces .>> pstr "by") .>> pspacesOrLines .>>. pmembers .>> pspacesOrLines .>>. ptry (pstr "such that" >>. prepeat1 (pspacesOrLines >>. pcase))) 
        |> map (fun ((name,values),patterns) -> 
            match patterns with
            | None -> CategoryDeclaration (name,List.map (fun (x,y) -> x,y,None) values)
            | Some patterns ->
                let patterns' = List.map (fun (p,e) -> match p with | TermPattern (id,_) -> id,(p,e)) patterns |> Map.ofList
                CategoryDeclaration (name, List.map (fun (x,y) -> x,y,patterns'.TryFind x) values))
        |> setLabel "Category declaration"
    let pdeclareVar = 
        pstr "declare" >>. pspaces >>. pid .>> pspaces .>> pchar '=' .>> pspacesOrLines .>>. pexpr
        |> map VarDeclaration
        |> setLabel "Variable declaration"
    pdeclareVar <|> pdeclareCategory |> setLabel "Declaration"

let pblock = prepeat (pspacesOrLines >>. pdeclare) .>> pspacesOrLines .>>. pexpr |> map Block |> setLabel "Block"