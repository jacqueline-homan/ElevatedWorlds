module ElevatedWorlds.L3

open FParsec
open ElevatedWorlds.Structures
//Total Weight and Charges

//L3-01: Weight
type Weight = Weight of double 
let pwt : Parser<Weight> = 
    field pfloat Weight 

//L3-02: Weight Qualifier
type WtQual = 
    | GrossWt 

let pwqual : Parser<WtQual> =
    field (skipString "G" ) (constant GrossWt)

type WtInfo = 
    { weight : Weight
      wqual : WtQual}

let pinfo = parse {
    let! x = pwt
    let! y = pwqual

    return {
        weight = x
        wqual = y}
    }

//L3-03: Freight Rate
type FreightRate = FreightRate of double 

let pfrate : Parser<FreightRate> = 
    field pfloat FreightRate

//L3-04: Rate/Value Qualifier
type RateQual =
    | FlatRate

let prqual : Parser<RateQual> = 
    field (skipString "FR") (constant FlatRate)

type RateInfo = 
    { fRt : FreightRate
      rtQ : RateQual}

let prateInfo = parse {
    let! x = pfrate
    let! y = prqual

    return {
        fRt = x
        rtQ = y} 
    }

//L3-05: Charge
type Charge = Charge of double 

let pchg : Parser<Charge option> = 
    optfield pfloat Charge 

//L3-11: Lading Quantity
type LadingQty = LadingQty of int

let plqty : Parser<LadingQty option> =
    optfield' (manyMinMaxSatisfy 1 7 (isDigit) |>> int)  LadingQty 

type L3 = L3 of WtInfo * RateInfo * Charge option * LadingQty option

let pL3 = parse {
    let! wi = pinfo
    let! ri = prateInfo
    let! ch = pchg .>> weirdsep
    let! qt = plqty
    return L3(wi, ri, ch, qt)}

let pL3Rec = record "L3" pL3