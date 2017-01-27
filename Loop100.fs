module ElevatedWorlds.Loops

open FParsec

type Loop = 
    | Loop100
    | Loop300
    | Loop310
    | Loop320
    | Loop325
    | Loop330
    | Loop360
    | Loop365

type LoopType = LoopType of Loop