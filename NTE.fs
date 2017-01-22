module ElevatedWorlds.NTE

open ElevatedWorlds.Structures
open FParsec
open System
open System.Collections.Generic
open System.IO

type RefCode =
    | RefCode of uint16

let pRefCode : Parser<RefCode option> =
//converting the result of manyMinMaxSatisfy which is a string, into uint16 
    optfield (manyMinMaxSatisfy 3 3 isDigit |>> uint16) RefCode 

type NTEDescription =
    | NTEDescription of string

let pDescription : Parser<NTEDescription> =
    field (manyMinMaxSatisfy 1 80 (isNoneOf "~")) NTEDescription

type NTE =
    | NTE of RefCode option * NTEDescription

//stuck again bc this one is being a rat fucker...
let pNTE = parse {
    let! r = oFSep >>. pRefCode
    let! d = fsep >>. pDescription
    return NTE(r,d)}

let pNTERec = record "NTE" pNTE
    
        
