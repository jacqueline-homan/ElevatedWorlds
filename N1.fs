module ElevatedWorlds.N1

open ElevatedWorlds.Structures
open FParsec
open System
open System.IO

type Entity =
    | Entity of string
let entity : Parser<Entity> = manyMinMaxSatisfy 2 3 isAsciiLetter |>> Entity .>> fsep

type Name =
    | Name of string
let name : Parser<Name> = manyMinMaxSatisfy 1 60 (isNoneOf "*~") |>> Name .>> fsep

type IdQual =
    | IdQual of string

let idQual : Parser<IdQual> = manyMinMaxSatisfy 1 2 isDigit |>> IdQual .>> fsep

type IdCode =
    | IdCode of string

let idCode : Parser<IdCode> = manyMinMaxSatisfy 2 80 (isNoneOf "*~") |>> IdCode .>> rsep

type N1 =
    | N1 of Entity * Name * IdQual * IdCode

(*
let n1 : Parser<N1> =
    skipString "N1" >>. fsep >>. tuple4 (let entity .>> fsep) ( name .>> fsep) (let idQual .>> fsep) (let idCode .>> rsep)|>> N1
*)
//because I did not want a nasty tuple4 monster staring me down, I refactored
let pN1 : Parser<N1> =
    skipString "N1" >>. fsep >>. entity
        >>= fun n ->
            name
            >>= fun q ->
                idQual
                >>= fun c ->
                    idCode
                    >>= fun x ->
                        preturn (N1(n, q, c, x))
