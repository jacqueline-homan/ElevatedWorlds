module ElevatedWorlds.EDI

open ElevatedWorlds.B2
open ElevatedWorlds.B2A
open ElevatedWorlds.GS
open ElevatedWorlds.IEA
open ElevatedWorlds.ISA
open ElevatedWorlds.N1
open ElevatedWorlds.NTE
open ElevatedWorlds.ST
open ElevatedWorlds.N1
open ElevatedWorlds.N3
open ElevatedWorlds.N4
open ElevatedWorlds.S5
open ElevatedWorlds.L11
open ElevatedWorlds.IEA
open FParsec

type EDI =
    | EDI of ISA * GS * ST * B2 * B2A * NTE * N1 * N3 * N4 * S5 * L11 list

let pEDI : Parser<EDI,_> = parse {
    let! a = pISARec
    let! b = pGSRec
    let! c = pSTRec
    let! d = pB2Rec
    let! e = pB2A
    let! f = pNTE
    let! g = pN1Rec
    let! h = pN3
    let! i = pN4Rec
    let! j = pS5
    let! k = many pL11
    return (EDI(a, b, c, d, e, f, g, h, i, j, k))
    }
