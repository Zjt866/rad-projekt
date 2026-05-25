module CountSketch

open HashFunctions

// Opgave 5

let MakckCoutSkt (t: int)(g: uint64 -> bigint): (uint64 -> uint64) * (uint64 -> int)=

    if t < 0 || t > 64 then
        invalidArg "t" "t should be between 0 and 64"

    let m = 1I <<< t
    let h (x: uint64) : uint64=
        let gx = g x
        uint64 (gx % m) // h(x) = g(x) mod m 
    
    let s(x: uint64)=
        let gx = g x 
        let topBit= gx >>> 88 // p = 2^89 - 1, så b = 89.
        // s(x) = 1 - 2 * floor(g(x) / 2^88)
        1-2 * int topBit
    (h, s)
    
// t, som bestemmer størrelsen af arrayet.

// h, som bestemmer hvilken counter vi bruger.

// s, som bestemmer plus eller minus.

// stream, som er en sekvens af par (x, d) 
// Opgave 6. Vi bygge count-Sketch arrayet c ud fra streem
let buildCountSketch (t: int) (h: uint64 -> uint64) (s: uint64 -> int) (stream: seq<uint64 * int>): bigint array= 
    if t < 0 || t > 30 then 
        invalidArg "t" "t should be between 0 and 30"
    
    let m = 1 <<< t


    // C[0], ..., C[m-1] starter på 0
    let C = Array.create m 0I

    // den løbe igennem streem
    for (x, d) in stream do
        let index = int (h x)
        let sign = s x

        // C[h(x)] <- C[h(x)] + s(x) * d
        C[index] <- C[index] + bigint (sign *d)

    C


// Beregner estimatet X = sum_y C[y]^2
let estimateSecondMoment (c: bigint array) : bigint =
    c 
    |> Array.sumBy (fun cy -> cy * cy)

