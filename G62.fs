module ElevatedWorlds.G62

open System
open System.IO
open FParsec
open ElevatedWorlds.Structures
//G62-01: The Date Qualifier
type DateQual =
    | ShipNotBefore
    | ShipNotLater
    | DelNotBefore
    | DelNotLater

let pdqual : Parser<DateQual> = 
    (field (skipString "37") (constant ShipNotBefore)
    <|> field (skipString "38") (constant ShipNotLater)
    <|> field (skipString "53") (constant DelNotBefore)
    <|> field (skipString "54") (constant ShipNotLater)
    ) <?> "DateQual"

//G62-02: Date
type Date = Date of string 
let pdate : Parser<Date, unit> = field (anyString 8) Date

//G62-03: The Time Qualifier
type TimeQual =
    | EarlReqPU
    | EarlReqDel
    | LatReqPU
    | LatReqDel 

let ptqual : Parser<TimeQual> =
    (field (skipString "I") (constant EarlReqPU)
    <|> field (skipString "G") (constant EarlReqDel)
    <|> field (skipString "K") (constant LatReqPU)
    <|> field (skipString "L") (constant LatReqDel)
    ) <?> "TimeQual"

//G62-04: Time
type Time = Time of string
let ptime : Parser<Time, unit> = field (manyMinMaxSatisfy 4 8 (fun c -> isDigit c)) Time

type G62 = G62 of DateQual * Date * TimeQual * Time

let pG62 = parse {
    let! dq = pdqual
    let! d = pdate
    let! tq = ptqual
    let! t = ptime
    return G62(dq, d, tq, t)}

let pG62Rec = record "G62" pG62 <?> "G62"