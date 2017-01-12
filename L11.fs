module ElevatedWorlds.L11

open System
open System.Collections.Generic
open System.IO
open ElevatedWorlds.Structures
open FParsec

type RefId = 
    | RefId of string 

let pRefId : Parser<RefId> = manyMinMaxSatisfy 1 30 (isNoneOf "*~**:***") |>> RefId .>> fsep


type RefQual = 
    | RefQual of string 

let pRefQual : Parser<RefQual> = manyMinMaxSatisfy 2 3 (isNoneOf "*~**:***") |>> RefQual .>> fsep


type BusInstruct = 
    { referenceId : RefId
      referenceQual : RefQual}

let pBus = 
    pipe2 pRefId pRefQual (fun i q -> {referenceId = i; referenceQual = q}) 


type Description =
    | Description of string 

let pDesc : Parser<Description> = manyMinMaxSatisfy 1 80 (isNoneOf "*~**:***") |>> Description .>> rsep
       
type L11 =
    | L11 of BusInstruct * Description

let pL11Record = 
    pBus 
    >>= fun x ->
        pDesc 
        >>= fun y -> 
        preturn (L11(x, y))

let pL11 = 
    skipString "L11" >>. fsep >>. pL11Record

(*
type L11Short = 
    | L11Short of string

let pL11Short : Parser<L11Short> = anyString 3 |>> L11Short .>> pFSep

type State = 
    | State of string 

let pState : Parser<State> = anyString 2 |>> State .>> pRSep

type L11Shorty =
    | L11Shorty of L11Short * State

let pPartialL11 :Parser<L11Shorty> = 
    skipString "L11" >>. pFSep >>. tuple2 pL11Short pState |>> L11Shorty
*)   
