module ElevatedWorlds.N3

open System
open System.IO
open FParsec
open ElevatedWorlds.Structures
//Loop 310
type Address1 =
    Address1 of string

let pAddr1 : Parser<Address1> =
    field (pValue 1 55) Address1

type Address2 =
    | Address2 of string

let pAddr2 : Parser<Address2 option> =
    optfield'
        (pValue 1 55) Address2

type AddressInfo = {
    address1 : Address1
    address2 : Address2 Option
    }

let pAddressInfo = parse {
    let! a1 = pAddr1
    let! a2 = pAddr2

    return {
        address1 = a1
        address2 = a2
    }
}

type N3 =
    | N3 of AddressInfo

let pN3 = parse {
    let! ai = pAddressInfo
    return N3(ai)
    }

let pN3Rec = record "N3" pN3
