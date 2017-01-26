module ElevatedWorlds.L11

open System
open System.Collections.Generic
open System.IO
open ElevatedWorlds.Structures
open FParsec

// 1. R0103 - At least one of L1101 or L1103 is required.

// L1101: R0103
type RefId =
    | RefId of string

let pRefId : Parser<RefId> =
    field (pValue 1 30) RefId

// L1102: L1101 | L1102 -> L1101 & L1102
type RefQual =
    | RefQual of string

let pRefQual : Parser<RefQual> =
    field (pValue 2 3) RefQual

// L1103: R0103
type Description =
    | Description of string

let pDesc : Parser<Description> =
    field' (pValue 1 80) Description

let pOptDesc : Parser<Description option> =
    optfield' (pValue 1 80) Description

type L11 =
    | L11R0103 of RefId * RefQual * Description option
    | L11P0102 of Description

let pL11R0103 : Parser<L11> = pipe3 pRefId pRefQual pOptDesc (fun a b c -> L11R0103 (a, b, c)) <?> "L11"
let pL11P0102 : Parser<L11> = pDesc |>> (fun d -> L11P0102 d) <?> "L11P0102"

let pL11 : Parser<L11> = (attempt pL11R0103) <|> (attempt pL11P0102)
let pL11Rec = record "L11" pL11
