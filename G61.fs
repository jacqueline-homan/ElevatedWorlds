module ElevatedWorlds.G61

open FParsec
open ElevatedWorlds.Structures
//Loop 310
// G61-01: The Contact Function Code
type CFC =
    | InfoContact 

let pCFC : Parser<CFC> = field (skipString "IC") (constant InfoContact)

//G61-02: Name
type Name = Name of string

let pName : Parser<Name> = field (manyMinMaxSatisfy 1 60 isAsciiLetter) Name

//G61-03: Communication Number Qualifier
type ComNoQual = 
    | Fax
    | Phone

let pComNoQual : Parser<ComNoQual> = 
    (tryField(skipString "FX") (constant Fax))
    <|>(tryField(skipString "TE") (constant Phone))
    <?> "ComNoQual"

//G61-04: Communication Number
type ComNo =
    | ComNo of string

let pComNo : Parser<ComNo> = field (manyMinMaxSatisfy 1 80 (isNoneOf "*~")) ComNo

//Communication number info
type ComInfo =
    { comNoQual : ComNoQual 
      comNo : ComNo}

let pcomInf = parse {
    let! x = pComNoQual
    let! y = pComNo 
    return {
        comNoQual = x
        comNo = y}
    }

type G61 = G61 of CFC * Name * ComInfo 

let pG61 = parse {
    let! c = pCFC
    let! n = pName
    let! ci = pcomInf
    return G61(c, n, ci)}

let pG61Rec = record "G61" pG61
