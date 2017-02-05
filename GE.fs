module ElevatedWorlds.GE

open FParsec
open ElevatedWorlds.Structures

type NoTxnSetsInc = NoTxnSetsInc of int

let ptx : Parser<NoTxnSetsInc> = 
    field (manyMinMaxSatisfy 1 6 (isDigit) |>> int) NoTxnSetsInc

type GrpCtrlNo = GrpCtrlNo of string 

let pgcn : Parser<GrpCtrlNo> = 
    field (manyMinMaxSatisfy 1 9 isDigit) GrpCtrlNo

type GE = GE of NoTxnSetsInc * GrpCtrlNo

let pGE = parse {
    let! x = ptx
    let! y = pgcn
    return GE(x, y)}

let pGERec = record "GE" pGE