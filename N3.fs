module ElevatedWorlds.N3

open System
open System.IO
open FParsec
open ElevatedWorlds.Structures

type AddressInfo =
    | AddressInfo of string

let pAddy : Parser<AddressInfo> = 
    field (manyMinMaxSatisfy 1 55 (isNoneOf "*~")) AddressInfo 

type Details =
    | Details of string

let pDet : Parser<Details option> =
    optfield
        (manyMinMaxSatisfy 1 55 (isNoneOf "*~")) Details 

type OptionalAddressInfo =
    { address : AddressInfo
      optDetails : Details option}

let opAdInf = parse {
    let! a = pAddy
    let! d = pDet

    return {
        address = a
        optDetails = d
    }
}

type N3 =
    | N3 of OptionalAddressInfo

let pN3 = parse {
    let! opAd = opAdInf 
    return N3(opAd)}

let pN3Rec = record "N3" pN3

