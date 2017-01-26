module ElevatedWorlds.G62

open System
open System.IO
open FParsec

//G62-01: The Date Qualifier
type DateQual =
    | ShipNotBefore
    | ShipNotLater
    | DelNotBefore
    | DelNotLater

//G62-02: The Date
type Date = Date of string


//G62-03: The Time Qualifier
type TimeQual =
    | EarlReqDel
    | EarlReqPU
    | LatReqPU
    | LatReqDel 

//G62-04
type Time = Time of string


type G62 = G62 of DateQual * Date * TimeQual * Time