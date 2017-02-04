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


