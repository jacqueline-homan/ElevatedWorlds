module ElevatedWorlds.ST

open FParsec
open System
open System.Collections.Generic
open System.IO
open ElevatedWorlds.Structures

type TsID =
    | MotorCarrierLoadTender

let tsId : Parser<TsID> = field (skipString "204") (constant MotorCarrierLoadTender) 

type TctrlNo =
    | TctrlNo of string

let tctrlNo : Parser<TctrlNo> = field' (manyMinMaxSatisfy 4 9 isDigit) TctrlNo

type ST = ST of TsID * TctrlNo

let pST = parse {
    let! id = tsId
    let! tc = tctrlNo 
    return ST(id, tc)}

let pSTrecord = record "ST" pST 
