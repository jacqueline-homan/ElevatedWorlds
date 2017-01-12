﻿module ElevatedWorlds.NTE

open ElevatedWorlds.Structures
open FParsec
open System
open System.Collections.Generic
open System.IO

type RefCode =
    | RefCode of uint16

let pRefCode : Parser<RefCode option> =
    (opt
         (manyMinMaxSatisfy 3 3 isDigit
          |>> (fun rc -> UInt16.Parse(rc) |> RefCode))) .>> fsep

type NTEDescription =
    | NTEDescription of string

let pDescription : Parser<NTEDescription> =
    manyMinMaxSatisfy 1 80 (isNoneOf "~") |>> NTEDescription

type NTE =
    | NTE of RefCode option * NTEDescription

let pNTE : Parser<NTE> =
    skipString "NTE" >>. fsep >>. tuple2 pRefCode pDescription |>> NTE
    .>> rsep
