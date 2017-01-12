module ElevatedWorlds.IEA

open System
open System.IO
open FParsec
open ElevatedWorlds.Structures

type InclFunGrps = 
    | InclFunGrps of string

let pFgrps : Parser<InclFunGrps> = 
    manyMinMaxSatisfy 1 5 (isNoneOf "~") |>> InclFunGrps .>> fsep


type IntchgCtrlNo =
    | IntchgCtrlNo of uint16

let pCtrlNo : Parser<IntchgCtrlNo> = 
    (manyMinMaxSatisfy 9 9 isDigit |>> (fun ctl -> UInt16.Parse(ctl) |> IntchgCtrlNo)) .>> fsep

type IEA = IEA of InclFunGrps * IntchgCtrlNo

let pIEA =
    skipString "IEA" >>. fsep >>. tuple2 pFgrps pCtrlNo |>> IEA .>> rsep
