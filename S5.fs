module ElevatedWorlds.S5

open System
open System.Collections.Generic
open System.IO
open FParsec
open ElevatedWorlds.Structures

type StopSeqNo = 
    | StopSeqNo of uint16

let pStopSeqNo : Parser<StopSeqNo> = 
    field (manyMinMaxSatisfy 1 3 isDigit |>> uint16) StopSeqNo 

type StopReason =
    | Complete
    | CompleteUnload
    | PartLoad
    | PartUnload 

let pStopReason : Parser<StopReason> = 
    (tryField (skipString "CL") (constant Complete)
      <|> tryField (skipString "CU") (constant CompleteUnload)
      <|> tryField (skipString "PL") (constant PartLoad) 
      <|> tryField (skipString "PU") (constant PartUnload)
    ) <?> "StopReason"

type S5 =
    | S5 of StopSeqNo * StopReason

let pS5 = parse {
    let! stsq = pStopSeqNo
    let! stre = pStopReason
    return S5(stsq, stre)}
         
let pS5Rec = record "S5" pS5