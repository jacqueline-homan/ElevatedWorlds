module ElevatedWorlds.AT8

open FParsec
open System
open System.IO
open ElevatedWorlds.Structures

//Weight Qualifier
type WtQual = 
    | GrossWt 

let pWtQual : Parser<WtQual> = 
    field (manyMinMaxSatisfy 1 2 isAsciiLetter) (constant GrossWt)

//
type WtCode =
    | Kilograms
    | Pounds 

let pWtCode : Parser<WtCode> = 
    (field (skipString "K") (constant Kilograms)
    <|> field (skipString "L") (constant Pounds)
    ) <?> "WtCode"

type Weight = Weight of string 
    
let pWtDsc : Parser<Weight> = 
    field (manyMinMaxSatisfy 1 10 isDigit) Weight

type LadingQty =
    | LadingQty of string

let plqty : Parser<LadingQty option> = 
    optfield (pValue 1 7) LadingQty

type AT8 = AT8 of WtQual * WtCode * Weight * LadingQty option

let pAT8 = parse {
    let! q = pWtQual
    let! c = pWtCode
    let! w = pWtDsc
    let! l = plqty

    return AT8(q,c,w,l)}

let pAT8Rec = record "AT8" pAT8
