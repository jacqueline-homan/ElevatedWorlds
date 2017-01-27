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
open ElevatedWorlds.G62
open ElevatedWorlds.AT8
open FParsec

type EDI =
    | EDI of ISA * GS * ST * B2 * B2A * NTE * N1 * N3 * N4 * S5 * L11 list * G62 list * AT8

let pEDI : Parser<EDI,_> = parse {
    let! a = pISARec
    let! b = pGSRec
    let! c = pSTRec
    let! d = pB2Rec
    let! e = pB2ARec
    let! f = pNTERec
    let! g = pN1Rec
    let! h = pN3Rec
    let! i = pN4Rec
    let! j = pS5Rec
    let! k = many pL11Rec
    let! l = many pG62Rec
    let! m = pAT8Rec
    return (EDI(a, b, c, d, e, f, g, h, i, j, k, l, m))
    }
