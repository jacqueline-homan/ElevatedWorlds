module ElevatedWorlds.Structures

open FParsec
open System

// Parse and EDI record value into a record type tag and then a sequence of fields
type Parser<'t> = Parser<'t, unit>

// The field separator
let fsep : Parser<_> = skipChar '*' <?> "Field Separator"

// Optional fields are separated by two '*'
let oFSep : Parser<_> = skipString "**" <?> "Double Field Separator"

// The record delimiter
let rsep : Parser<_> = skipChar '~' <?> "Record Separator"

// Parse either separator
let asep =
    (attempt fsep) <|> (attempt rsep) <?> "Field or Record Separator"

//ISA-16: Component Element Separator. Since this is not a data structure, we only need a function.
let elsep : Parser<_> = skipChar ':' <?> "Semicolon"
let psep : Parser<_> = skipChar '.' <?> "Dot"
let nbr l = manyMinMaxSatisfy l l isDigit <?> "Number"

// DateTime parsing
let invInf = System.Globalization.DateTimeFormatInfo.InvariantInfo

// Parse a field without a separator, returning a value, then convert
// the value inside the Parser to another value.
let field' (p : Parser<_>) (c : _ -> 'v) : Parser<'v> = p |>> c

// Parse a field using field' and skip the separator after it.
let field (p : Parser<_>) (c : _ -> 'v) : Parser<'v> = field' p c .>> fsep

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
let time : Parser<DateTime> =
    (attempt (tryDate "HHmm")) <|> (attempt (tryDate "HHmmss")) <?> "Time"

// Parse a pair of fields containing a date and time into a value.
let dateTime (c : DateTime -> 'p) : Parser<'p> =
    date
    >>= fun d ->
        time >>= fun t -> preturn (d.Add(t.TimeOfDay) |> c) <?> "DateTime"