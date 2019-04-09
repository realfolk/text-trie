# text-trie benchmarks

We compare the per iteration/second timings of `Data.Trie.Text`,
`Data.Trie` with Text encoded to `UTF16`, and `Data.Trie` with `Char8`.

The associated values represent how many times faster
`Data.Trie.Text` is than the given usage of `Data.Trie`.

## fromListL: obverse

Timing method           | Data.Trie (UTF16)    | Data.Trie (Char8)
----------------------- | -------------------- | ---------------------
Per iteration: 269.202  | 1.5647654920840113   | 0.7017667030705567  
Per second   : 3714.68  | 1.5647675814570652   | 0.7017675107493974  

## fromListR: reverse

Timing method           | Data.Trie (UTF16)    | Data.Trie (Char8)
----------------------- | -------------------- | ---------------------
Per iteration: 366.425  | 0.9763143890291327   | 0.7708125810193082  
Per second   : 2729.07  | 0.9763136429981969   | 0.7708126795292204  

## fromListL: reverse

Timing method           | Data.Trie (UTF16)    | Data.Trie (Char8)
----------------------- | -------------------- | ---------------------
Per iteration: 44.477   | 3.4125727904310095   | 1.010792094790566   
Per second   : 22483.34 | 3.412533429663381    | 1.0107797492847368  

## fromListR: obverse sorted

Timing method           | Data.Trie (UTF16)    | Data.Trie (Char8)
----------------------- | -------------------- | ---------------------
Per iteration: 50.894   | 3.4557904664597006   | 0.908751522772822   
Per second   : 19648.77 | 3.45581034591925     | 0.908756941557027   

## fromListL: obverse sorted

Timing method           | Data.Trie (UTF16)    | Data.Trie (Char8)
----------------------- | -------------------- | ---------------------
Per iteration: 278.447  | 1.1829540271577714   | 0.6452502630662209  
Per second   : 3591.35  | 1.1829527787293472   | 0.6452496752505915  

## fromListR: reverse sorted

Timing method           | Data.Trie (UTF16)    | Data.Trie (Char8)
----------------------- | -------------------- | ---------------------
Per iteration: 52.973   | 3.3431181922866364   | 0.830819474071697   
Per second   : 18877.69 | 3.34314854038125     | 0.8308275623802073  

## fromListL: reverse sorted

Timing method           | Data.Trie (UTF16)    | Data.Trie (Char8)
----------------------- | -------------------- | ---------------------
Per iteration: 302.306  | 1.1726495669950316   | 0.5597871031339107  
Per second   : 3307.91  | 1.1726517966024785   | 0.5597869773219184  

