module ElevatedWorlds.N4

open System
open System.Collections.Generic
open System.IO
open FParsec
open ElevatedWorlds.Structures

type City =
    | City of string

let pCity : Parser<City option> =
    (opt
        (manyMinMaxSatisfy 2 30 (isNoneOf "*~") |>> City)) .>> fsep

type State =
    | State of string

let pState : Parser<State option> =
    (opt
        (anyString 2 |>> State)) .>> fsep

type Zipcode =
    | Zipcode of string

let pZip : Parser<Zipcode option> =
    (opt
        (manyMinMaxSatisfy 3 15 (isNoneOf"*~.,':;' '") |>> Zipcode)) .>> fsep

type Country =
    | Country of string

let pCountry : Parser<Country option> =
    (opt
        (manyMinMaxSatisfy 2 3 isAsciiLetter |>> Country)) //.>> fsep



type Location =
    { city : City option
      state : State option
      zip : Zipcode option
      country : Country option}

let loc =
    pipe4 pCity pState pZip pCountry (fun m s z c ->
        {city = m
         state = s
         zip = z
         country = c})

type N4 =
    | N4 of Location * City option * State option * Zipcode option * Country option //City * State * Zipcode * Country

let pN4 =
    loc
    >>= fun a ->
        pCity
        >>= fun b ->
            pState
            >>= fun c ->
                pZip
                >>= fun d ->
                    pCountry
                    >>= fun e ->
                        preturn (N4(a,b,c,d,e))

let N4record = skipString "N4" >>. fsep >>. pN4 .>> rsep

