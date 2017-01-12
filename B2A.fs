module ElevatedWorlds.B2A

open ElevatedWorlds.Structures
open FParsec
open System
open System.Collections.Generic
open System.IO

//B2A: Transaction Set Purpose Code
type SetPurpCode =
    | Original
    | Cancellation
    | Change

let pSetPurpCode : Parser<SetPurpCode> =
    ((skipString "00" >>? preturn Original)
     <|> (skipString "01" >>? preturn Cancellation)
     <|> (skipString "04" >>? preturn Change)) .>> fsep

type AppType =
    | LoadTender

let pAppType : Parser<AppType option> =
    opt (skipString "LT" >>. preturn LoadTender)

type B2A = B2A of SetPurpCode * Option<AppType>

let pB2A : Parser<B2A> =
    skipString "B2A" >>. fsep >>. tuple2 pSetPurpCode pAppType |>> B2A .>> rsep
