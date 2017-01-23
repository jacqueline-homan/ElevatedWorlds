module ElevatedWorlds.NTE

open ElevatedWorlds.Structures
open FParsec
open System
open System.Collections.Generic
open System.IO

type RefCode =
    | RefCode of uint16

//converting the result of manyMinMaxSatisfy which is a string, into uint16
let pRefCode : Parser<RefCode option> =
    optfield (manyMinMaxSatisfy 3 3 isDigit) (fun v -> (RefCode (uint16 v))) <?> "RefCode"

type NTEDescription =
    | NTEDescription of string

let pDescription : Parser<NTEDescription> =
    field (manyMinMaxSatisfy 1 80 (isNoneOf "~")) NTEDescription <?> "NTEDescription"

type NTE =
    | NTE of RefCode option * NTEDescription

//stuck again bc this one is being a rat fucker...
let pNTE = parse {
    let! r = pRefCode
    let! d = pDescription
    return NTE(r,d)}

let pNTERec = record "NTE" pNTE
