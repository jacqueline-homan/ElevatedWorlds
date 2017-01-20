module ElevatedWorlds.GS

open ElevatedWorlds.Structures
open FParsec
open System
open System.Collections.Generic
open System.IO

// GS*SM*MGCTLYST*BLNJ*20160930*145316*1*X*004010~
// GS-01: Functional Identifier Code, 2 chars, "SM"
type FuncIdCode =
    | MotorCarrierLoadTender

let funcIdCode : Parser<FuncIdCode> =
    field (skipString "SM") (constant MotorCarrierLoadTender)

let routeCode = (manyMinMaxSatisfy 2 15 isUpper)

//GS-02: Application Sender Code, 2 chars
type ApsndrCode =
    | ApsndrCode of string

let sdr : Parser<ApsndrCode> = field routeCode ApsndrCode

//GS-03: Application Receiver Code
type AprecvrCode =
    | AprecvrCode of string

let rcvr : Parser<AprecvrCode> = field routeCode AprecvrCode

//Handling the relationship set between Sender and Receiver
type Routing =
    { apsdrCode : ApsndrCode
      aprecvrCode : AprecvrCode }

let routing = parse {
    let! s = sdr
    let! r = rcvr

    return { apsdrCode = s
             aprecvrCode = r }
    }

//GS-04, GS-05: Date of transaction
type TxnTimeStamp =
    | TxnTimeStamp of DateTime

let txnTimeStamp = field dateTime TxnTimeStamp

//GS-06: Group Control Number
type GrctrlNo =
    | GrctrlNo of string

let grctlNo = field (manyMinMaxSatisfy 1 9 isDigit) GrctrlNo

//GS-07: Responsible Agency Code
type RsagyCode =
    | AccredStdsCmteX12

let rsagyCode = field (skipString "X") (constant AccredStdsCmteX12)

//GS-08: Version/Release/Industry Indentifier Code
type VRIIcode =
    | DraftStds

let vRIIcode = field' (skipString "004010") (constant DraftStds)

type GS =
    | GS of FuncIdCode * Routing * TxnTimeStamp * GrctrlNo * RsagyCode * VRIIcode

let pGS = parse {
    let! fid = funcIdCode
    let! rtg = routing
    let! ts = txnTimeStamp
    let! gcn = grctlNo
    let! ragy = rsagyCode
    let! vrii = vRIIcode

    return GS(fid, rtg, ts, gcn, ragy, vrii)
    }

let pGSRec = record "GS" pGS