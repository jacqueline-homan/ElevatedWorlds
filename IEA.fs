module ElevatedWorlds.IEA

open System
open System.IO
open FParsec
open ElevatedWorlds.Structures

type InclFunGrps = 
    | InclFunGrps of int

let pFgrps : Parser<InclFunGrps> = 
    field (manyMinMaxSatisfy 1 5 (isDigit) |>> int) InclFunGrps 


type IntchgCtrlNo =
    | IntchgCtrlNo of uint16

let pCtrlNo : Parser<IntchgCtrlNo> = 
    field (manyMinMaxSatisfy 9 9 isDigit |>> uint16) IntchgCtrlNo 

type IEA = IEA of InclFunGrps * IntchgCtrlNo

let pIEA = parse {
    let! x = pFgrps
    let! y = pCtrlNo
    return IEA(x, y)}

let pIEARec =
    record "IEA" pIEA