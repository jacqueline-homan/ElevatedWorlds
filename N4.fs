﻿module ElevatedWorlds.N4

open System
open System.Collections.Generic
open System.IO
open FParsec
open ElevatedWorlds.Structures
//Loop 310
type City =
    | City of string

let pCity : Parser<City option> =
    optfield (pValue 2 30) City

type State =
    | State of string

let pState : Parser<State option> =
    optfield (pValue 2 2) State

type Zipcode =
    | Zipcode of string

let pZip : Parser<Zipcode option> =
    optfield
        (pValue 3 15) Zipcode

type Country =
    | Country of string

let pCountry : Parser<Country option> =
    optfield (pAsciiAlpha 2 3) Country

type N4 =
    | N4 of City option * State option * Zipcode option * Country option

let pN4 = parse {
    let! c = pCity
    let! s = pState
    let! z = pZip
    let! o = pCountry

    return (N4(c, s, z, o))
}
let pN4Rec = record "N4" pN4
