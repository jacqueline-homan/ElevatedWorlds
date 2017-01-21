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
    (field' (skipString "00") (constant Original)
     <|> field' (skipString "01") (constant Cancellation)
     <|> field' (skipString "04") (constant Change)) 

type AppType =
    | LoadTender

let pAppType : Parser<AppType option> =
    optfield (skipString "LT") (constant LoadTender)

type B2A = B2A of SetPurpCode * AppType option

let pB2A = parse {
    let! p = fsep >>. pSetPurpCode
    let! a = fsep >>. pAppType
    let! _ = asep

    return B2A(p, a)
}

let pB2ARec = record "B2A" pB2A