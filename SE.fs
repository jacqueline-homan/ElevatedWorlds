module ElevatedWorlds.SE

open FParsec
open ElevatedWorlds.Structures
//Transaction Set Trailer

//SE-01: Number of Included Segments
type NoIncSegmts = NoIncSegmts of string 

let pnis : Parser<NoIncSegmts> = 
    field (manyMinMaxSatisfy 1 10 isDigit) NoIncSegmts

//SE-02: Transaction Set Control Number
type TnxSetCtrlNo = TnxSetCtrlNo of string 

let ptxn : Parser<TnxSetCtrlNo> = 
    field (manyMinMaxSatisfy 4 9 isDigit) TnxSetCtrlNo

type SE = SE of NoIncSegmts * TnxSetCtrlNo

let pSE = parse {
    let! x = pnis
    let! y = ptxn
    return SE(x, y)}

let pSERec = record "SE" pSE