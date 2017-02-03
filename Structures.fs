module ElevatedWorlds.Structures

open FParsec
open System

// Parse and EDI record value into a record type tag and then a sequence of fields
type Parser<'t> = Parser<'t, unit>

// The field separator
let fsep : Parser<_> = skipChar '*' <?> "Field Separator"

// The record delimiter
let rsep : Parser<_> = skipChar '~' <?> "Record Separator"

// Parse either separator
let asep =
    (attempt fsep) <|> (attempt rsep) <?> "Field or Record Separator"

// Parses an arbitrary string not containging separaters.
let pValue (m : int) (x : int): Parser<string> = manyMinMaxSatisfy m x (isNoneOf "*~")

// Parse ASCII letters
let pAsciiAlpha (m : int) (x : int) : Parser<string> = manyMinMaxSatisfy m x isAsciiLetter

//ISA-16: Component Element Separator. Since this is not a data structure, we only need a function.
let elsep : Parser<_> = skipChar ':' <?> "Semicolon"
let psep : Parser<_> = skipChar '.' <?> "Dot"
let nbr l = manyMinMaxSatisfy l l isDigit <?> "Number"

// DateTime parsing
let invInf = System.Globalization.DateTimeFormatInfo.InvariantInfo

// Just parse the field without the preceding separator.
let field' (p : Parser<_>) (c : _ -> 'v) : Parser<'v> = (p |>> c)

// Parse a field without a separator, returning a value, then convert
// the value inside the Parser to another value.
let field (p : Parser<_>) (c : _ -> 'v) : Parser<'v> = fsep >>. field' p c

// Try parsing a field, and reset if we fail..
let tryField (p : Parser<_>) (c : _ -> 'v) : Parser<'v> = (attempt (field p c))


//  let choiceField (p : Parser<_>)
// Parse an optional field.
let optfield (p : Parser<_>) (c :_ -> 'v) : Parser<'v option> =
    fsep >>. (opt (field' p c))

// parse an optional field with an optional preceding separator. This
// is for record-terminal fields.
let optfield' (p : Parser<_>) (c :_ -> 'v) : Parser<'v option> =
    (opt (fsep >>. field' p c))

// Parse a record. fs should be a series of field parsers yielding a
// tuple. This tuple is passed to the rc constructor. Then a record
// separator is parsed and thrown out.
let record (cd : string) fs =
    skipString cd >>. fs .>> rsep

// Return a value regardless of the parameter.
let constant (v : 'v) : (_ -> 'v) = fun _ -> v

// Try parsing a date format and just fail parsing if there's an exception
let tryDate (fmt : string) : Parser<DateTime, unit> =
    nbr (fmt.Length)
    >>= fun d ->
        try
            (preturn (DateTime.ParseExact(d, fmt, invInf)))
        with :? FormatException as ex ->
            fail
                (String.concat " "
                     [ "Could not parse date:"; d; "format:"; fmt ])

let date : Parser<DateTime> =
    (attempt (tryDate "yyyyMMdd")) <|> (attempt (tryDate "yyMMdd"))
    <?> "Date"

//let date2 : Parser<Date> = anyString 8 |>> Date .>> fsep
    //(attempt(tryDate "CCYYMMDD")) <|> (attempt(tryDate "yyyyMMDD"))

let time : Parser<DateTime> =
    (attempt (tryDate "HHmmss")) <|> (attempt (tryDate "HHmm")) <?> "Time"

// Parse a pair of fields containing a date and time into a value.
let dateTime : Parser<DateTime> =
    date
    >>= fun d ->
        fsep >>. time >>= fun t -> preturn (d.Add(t.TimeOfDay)) <?> "DateTime"
