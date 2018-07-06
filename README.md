# FiniteFloats.jl

#### Floats with neither Infinities nor NaNs.


----

#### Copyright ©2018 by Jeffrey Sarnoff.
####  This work is released under The MIT License.


-----

[![Build Status](https://travis-ci.org/JeffreySarnoff/FiniteFloats.jl.svg?branch=master)](https://travis-ci.org/JeffreySarnoff/FiniteFloats.jl)
----

## Use
```julia

using FiniteFloats

julia> a = sqrt(Finite64(2))
1.4142135623730951

julia> typeof(a)
Finite64

julia> b = Finite32(Inf32)
3.4028235f38

julia> b == typemax(Finite32)
true
```

## Exports

#### exported types

- Finite64, Finite32, Finite16

#### exported functions

In addition to the familiar functions that work with Float64, Float32, Float16,    
(comparisions, floating part decompositions, arithmetic, elementary functions)

- square, cube
- typemaxneg, typeminneg

----

## Examples
```julia
julia> Float64(0) * inv(Float64(0))
NaN

julia> Finite64(0) * inv(Finite64(0))
0.0
```
