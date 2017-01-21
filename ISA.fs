//
//  Author:
//       evan <>
//
//  Copyright (c) 2016 evan
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
module ElevatedWorlds.ISA

open FParsec
open ElevatedWorlds.Structures
open System

// Parse a record like:
//
//     ISA*00*          *00*          *ZZ*MGCTLYST       *02*BLNJ           *160930*1453*U*00401*000000001*0*P*:
//

// ISA-01: The Authorization Qualifier
type AuthQual = AQNone

//  We only have one of these anyway
let authQual : Parser<AuthQual> = field (skipString "00") (constant AQNone)

//ISA-02: this is 10 chars, may be whitespace
type AuthInfo =
    | AuthInfo of string

let authInfo : Parser<AuthInfo> = field (anyString 10) AuthInfo

// The Authorization Qualifier and Info
type Auth =
    { authQual : AuthQual
      authInfo : AuthInfo }

// Construct an Auth from the parsed pAuthQual and pAuthInfo. The
// field separators have already been handled at this point so we just
// need to combine the results of the parsers into another parser.
let auth =
    pipe2 authQual authInfo (fun q i ->
        { authQual = q
          authInfo = i })

// The Security Info Qualifier
type SecQual = SQNone

//ISA-03
let secQual : Parser<SecQual> = field (skipString "00") (constant SQNone)

type SecInfo =
    | SecInfo of string

//ISA-04
let secInfo : Parser<SecInfo> = field (anyString 10) SecInfo

// The Security Info Qualifier and stuff
type Sec =
    { secQual : SecQual
      secInfo : SecInfo }

let sec =
    pipe2 secQual secInfo (fun q i ->
        { secQual = q
          secInfo = i })

// ISA-05: The Interchange ID Qualifier
type InterchangeID = InterchangeID of string

let interchgID : Parser<InterchangeID> = field (anyString 2) InterchangeID

// ISA-06: The Interchange Sender ID
type InterchgSndrID = InterchgSndrID of string

let interchgSndrId : Parser<InterchgSndrID> = field (anyString 15) InterchgSndrID

// ISA-07 is exactly the same as ISA-05 in the MG-EDI pdf manual
type InterchgIdQual = InterchgIdQual of string

let interchgIdQual : Parser<InterchgIdQual> = field (anyString 2) InterchgIdQual

//ISA-08: The Interchange Receiver ID
type InterchgRecvrID = InterchgRecvrID of string

let interchgRcvId : Parser<InterchgRecvrID> = field (anyString 15) InterchgRecvrID

//ISA-09/ISA-10: Interchange Date/Time
type InterchgDateTime = InterchgDateTime of DateTime

let interchgDateTime = field dateTime InterchgDateTime

//ISA-11: Interchange Control Standards Identifier
type InterchgCtrlStds = InterchgCtrlStds of string

let interchgCtrlStds<'T, 'u> = field (anyString 1) InterchgCtrlStds

//ISA-12: Interchange Control Version Number
type InterchgCtrlVerNo = InterchgCtrlVerNo of string

let interchgCtrlVerNo : Parser<InterchgCtrlVerNo> = field (anyString 5) InterchgCtrlVerNo

//ISA-13: Interchange Control Number
type InterchgCtrlNo = InterchgCtrlNo of string

let interchgCtrlNo : Parser<InterchgCtrlNo> = field (anyString 9) InterchgCtrlNo

//ISA-14: Acknowledgement Requested
type AckReq = AckReq of string

let ackReq : Parser<AckReq> = field (anyString 1) AckReq

//ISA-15: Usage Indicator
type UsageInd = 
    | UsageProd
    | UsageTest

let usageInd : Parser<UsageInd> = field ((attempt (skipChar 'P' >>% UsageProd)) <|> (attempt (skipChar 'T' >>% UsageTest))) id

type ISA =
    | ISA of Auth * Sec * InterchangeID * InterchgSndrID * InterchgIdQual * InterchgRecvrID * InterchgDateTime * InterchgCtrlStds * InterchgCtrlVerNo * InterchgCtrlNo * AckReq * UsageInd

let pISA = parse {
    let! a = auth
    let! b = sec
    let! c = interchgID
    let! d = interchgSndrId
    let! e = interchgIdQual
    let! f = interchgRcvId
    let! g = interchgDateTime
    let! h = interchgCtrlStds
    let! i = interchgCtrlVerNo
    let! j = interchgCtrlNo
    let! k = ackReq
    let! l = usageInd

    let! _ = elsep

    return ISA(a, b, c, d, e, f, g, h, i, j, k, l)
    }

let pISARec = record "ISA" pISA 
