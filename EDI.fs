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
open ElevatedWorlds.G61
open ElevatedWorlds.L5
open ElevatedWorlds.L3
open FParsec

type EDI =
    | EDI of ISA * GS * ST * B2 * B2A * NTE * N1 * N3 * N4 * S5 * L11 list *
    G62 list * AT8 * N1 * N3 * N4 * G61 * L5 * AT8 * L5 * AT8 * S5 * L11 list *
    G62 list * AT8 * N1 * N3 * N4 * G61 * L5 * AT8 * S5 * L11 list * G62 list *
    AT8 * N1 * N3 * N4 * G61 * L5 * AT8 * L3

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
    let! n = pN1Rec
    let! o = pN3Rec
    let! p = pN4Rec
    let! q = pG61Rec
    let! r = pL5Rec
    let! s = pAT8Rec
    let! t = pL5Rec
    let! u = pAT8Rec
    let! v = pS5Rec
    let! w = many pL11Rec
    let! x = many pG62Rec
    let! y = pAT8Rec
    let! z = pN1Rec
    let! aa = pN3Rec
    let! bb = pN4Rec
    let! cc = pG61Rec
    let! dd = pL5Rec
    let! ee = pAT8Rec
    let! ff = pS5Rec
    let! gg = many pL11Rec
    let! hh = many pG62Rec
    let! ii = pAT8Rec
    let! jj = pN1Rec
    let! kk = pN3Rec
    let! ll = pN4Rec
    let! mm = pG61Rec
    let! nn = pL5Rec
    let! oo = pAT8Rec
    let! pp = pL3Rec

    return (EDI(a, b, c, d, e, f, g, h, i, j, k, l , m, n, o, p, q, r, s, t, u, v,
                w, x, y, z, aa, bb, cc, dd, ee, ff, gg, hh, ii, jj, kk, ll, mm, nn,oo, pp))
    }
