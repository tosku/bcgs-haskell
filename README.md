# bcgs 

## Description
Package for performing ground state calculations on the random bond Blume-Capel model.

## Benchmark
```
stack  bench --benchmark-arguments '--output bench/bench.html  +RTS -N1 -A1G' && open bench/bench.html
```

## Test
```
stack test
```

## Run
```
stack exec bcgs-exe -- jofile.json
```
where `jobfile.json` defines job's parameters
a sample job file for a grid `L=10` `d=2` is given below
```
{ "_l" : 10
, "_d"   : 2
, "_delta": 
  { "rangefrom": 0
  , "rangeto": 2
  , "rangestep": 0.1
  }
, "_disorder": 
  { "rangefrom": 1
  , "rangeto": 1
  , "rangestep": 0.1
  }
, "_disorderType" : "unimodal"
, "_realizations" : 100
, "_seedofSeeds" : 1409
, "_resultfile"    : "l10.json"
}
```
