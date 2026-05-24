module CountSketch

open HashFunctions

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

    


