module ElevatedWorlds.N1

open ElevatedWorlds.Structures
open FParsec
open System
open System.IO

type Entity =
    | Entity of string
let entity : Parser<Entity> =
    field (manyMinMaxSatisfy 2 3 isAsciiLetter) Entity

type Name =
    | Name of string
let name : Parser<Name> =
    field (manyMinMaxSatisfy 1 60 (isNoneOf "*~")) Name

type IdQual =
    | IdQual of string

let idQual : Parser<IdQual> =
    field (manyMinMaxSatisfy 1 2 isDigit) IdQual

type IdCode =
    | IdCode of string

let idCode : Parser<IdCode> =
    field (manyMinMaxSatisfy 2 80 (isNoneOf "*~")) IdCode

type N1 =
    | N1 of Entity * Name * IdQual * IdCode

let pN1 = parse {
    let! e = entity
    let! n = name
    let! iq = idQual
    let! ic = idCode
    return N1(e, n, iq, ic)}

let pN1Rec = record "N1" pN1 <?> "N1"
