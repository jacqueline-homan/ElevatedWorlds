module ElevatedWorlds.L11

open System
open System.Collections.Generic
open System.IO
open ElevatedWorlds.Structures
open FParsec

type RefId = 
    | RefId of string 

let pRefId : Parser<RefId> = 
    field (manyMinMaxSatisfy 1 30 (isNoneOf "*~**:***")) RefId


type RefQual = 
    | RefQual of string 

let pRefQual : Parser<RefQual> = 
    field (manyMinMaxSatisfy 2 3 (isNoneOf "*~**:***")) RefQual


type BusInstruct = 
    { referenceId : RefId
      referenceQual : RefQual}

let pBusInstruct = parse {
    let! i = pRefId
    let! q = pRefQual
    return {
        referenceId = i
        referenceQual = q}
    }

type Description =
    | Description of string 

let pDesc : Parser<Description> = 
    field (manyMinMaxSatisfy 1 80 (isNoneOf "*~**:***")) Description 
       
type L11 =
    | L11 of BusInstruct * Description

let pL11 = parse {
    let! b = pBusInstruct
    let! d = pDesc
    return L11(b, d)}

let pL11Rec = record "L11" pL11


