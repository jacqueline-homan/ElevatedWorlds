module ElevatedWorlds.ST

open FParsec
open System
open System.Collections.Generic
open System.IO
open ElevatedWorlds.Structures

type TsID =
    | MotorCarrierLoadTender

let tsId : Parser<TsID> = skipString "204" >>. preturn MotorCarrierLoadTender .>> fsep

type TctrlNo =
    | TctrlNo of string

let tctrlNo : Parser<TctrlNo> = manyMinMaxSatisfy 4 9 (fun c -> isDigit c ) |>> TctrlNo .>> rsep

type ST = ST of TsID * TctrlNo

let pST =
    skipString "ST" >>. fsep >>. tsId
    >>= fun id ->
        tctrlNo
        >>= fun ctrl ->
            preturn (ST(id, ctrl))
