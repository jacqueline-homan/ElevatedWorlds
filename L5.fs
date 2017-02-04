module ElevatedWorlds.L5

open FParsec
open ElevatedWorlds.Structures
//Loop 320
//L5-02: Lading Description
type LadingDesc =
    | LadingDesc of string

let pld : Parser<LadingDesc option> = 
    optfield (manyMinMaxSatisfy 1 50 isAsciiLetter) LadingDesc

//L5-03: Commodity Code
type CommCode = 
    | CommCode of string

let pcc : Parser<CommCode> = 
    field (manyMinMaxSatisfy 1 30 (isNoneOf "*~")) CommCode

//L5-04: Commodity Code Qualifier
type CCQ = 
    | NMFC

let pccq : Parser<CCQ> = 
    field (anyString 1) (constant NMFC)

//L5-05: Packing Code
type PkCode =
    | PkCode of string 

let pkc : Parser<PkCode option> = 
    optfield (manyMinMaxSatisfy 3 5 (isNoneOf "*~")) PkCode

//L5-06: Marks and Numbers
type Marks = 
    | Marks of string

let pmarks : Parser<Marks> = field (manyMinMaxSatisfy 1 48 (isNoneOf "*~")) Marks

//L5-07: Marks and Numbers Qualifier
type MarksQual = 
    | ShipmentID 

let pmqual : Parser<MarksQual option> = 
    optfield'(skipString "SI") (constant ShipmentID)

type L5 = L5 of LadingDesc option * CommCode * CCQ * PkCode option * Marks * MarksQual option

let pL5 = parse {
    let! ld = pld
    let! cc = pcc
    let! cq = pccq
    let! pk = pkc
    let! pm = pmarks
    let! pq = pmqual
    return L5(ld, cc, cq, pk, pm, pq)}

let pL5Rec = record "L5" pL5 


