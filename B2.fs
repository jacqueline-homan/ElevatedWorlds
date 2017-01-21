module ElevatedWorlds.B2

open ElevatedWorlds.Structures
open FParsec
open System
open System.Collections.Generic
open System.IO

//Beginning Segment for Shipment Information Transaction
type StdCarAlphaCode = 
    | StdCarAlphaCode of string

let pStdCarAlphaCode : Parser<StdCarAlphaCode option> = 
    optfield (pAsciiAlpha 2 4) StdCarAlphaCode <?> "StdCarAlphaCode"


type ShipIdNo = 
    | ShipIdNo of string

let pShipIdNo : Parser<Option<ShipIdNo>> = 
    optfield 
        (manyMinMaxSatisfy 1 30 (fun c -> isDigit c || isAsciiLetter c)) ShipIdNo <?> "ShipIdNo"

type ShipPmt = 
    | Collect 
    | Prepaid
    | ThirdPartyPay

let pShipPmt : Parser<ShipPmt> = 
    (field' (skipString "PP") (constant Prepaid) 
    <|> field' (skipString "CC") (constant Collect) 
    <|> field' (skipString "TP") (constant ThirdPartyPay)
    ) <?> "ShipPmt"

type B2 = 
    | B2 of StdCarAlphaCode option * ShipIdNo option * ShipPmt

// B2**BLNJ**BLNJ75035079T**PP~
let pB2 = parse {
    let! alpha = fsep >>. pStdCarAlphaCode
    printfn "alpha: %A" alpha
    let! idNo = fsep >>. pShipIdNo
    printfn "idNo: %A" idNo
    let! ship = fsep >>. pShipPmt
    printfn "ship: %A" ship
    return (B2(alpha, idNo, ship))
}

let pB2Rec = record "B2" pB2 <?> "B2"