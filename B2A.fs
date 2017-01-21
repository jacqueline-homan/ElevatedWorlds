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
    choice [field (skipString "00") (constant Original);
            field (skipString "01") (constant Cancellation);
            field (skipString "04") (constant Change);]

type AppType =
    | LoadTender

let pAppType : Parser<AppType option> =
    optfield (skipString "LT") (constant LoadTender)

type B2A = {
    pcode : SetPurpCode;
    atype :  Option<AppType>;
    }

let pB2A = parse {
    let! pc = pSetPurpCode
    let! at = pAppType

    return ({pcode = pc; atype = at;})
    }

let pB2ARec : Parser<B2A> = record "B2A" pB2A
