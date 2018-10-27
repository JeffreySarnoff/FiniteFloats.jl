# FiniteFloats.jl

#### Floats with neither Infinities nor NaNs.


----

#### Copyright ©&thinsp;2018 by Jeffrey Sarnoff. &nbsp;&nbsp; This work is released under The MIT License.


-----

[![Build Status](https://travis-ci.org/JeffreySarnoff/FiniteFloats.jl.svg?branch=master)](https://travis-ci.org/JeffreySarnoff/FiniteFloats.jl)
----

### this is an experimental branch
#### the major difference
- underflow to zero is intercepted and replaced with a _tiny_ value

- floatmin(T) * floatmin(T) == floatmin(T)
- floatmax(T) * floatmax(T) == floatmax(T)

Generally, arithmetic that would underflow to zero or overflow to Inf is made to be saturating.
So, the only way to obtain zero within the domain for FiniteFloats as specified (Floats without
infinities and without NaNs [almost always]) is to set it explicitly or to multiply a value by zero.

There are two saturating values, colloquially: __huge__ and __tiny__.  _huge_ is positive and
as large a magnitude as is (a) inv_ersable without generating zero and (b) _. 
_tiny_ is positive and as small a magnitude as is (a) inv_ersable without generating a floating point
infinity and (b) _.

### Tiny and Huge


These are the definitions for _tiny_ and _huge_, parameterized by floating point type.
```
tiny(::Type{Float64}) = nextfloat(inv(prevfloat(Inf64)))
tiny(::Type{Float32}) = nextfloat(inv(prevfloat(Inf32)))
tiny(::Type{Float16}) = nextfloat(inv(prevfloat(Inf16)))

huge(::Type{Float64}) = inv(tiny(Float64))
huge(::Type{Float32}) = inv(tiny(Float32))
huge(::Type{Float16}) = inv(tiny(Float16))
```

`tiny(T)` is the positive value nearest zero(T) for which `inv(tiny(T))` is finite.
`huge(T)` is the positive value nearest T(Inf) for which `inv(huge(T)) == tiny(T)`.

A look at the definitions shows that there is exactly one non-zero floating point
value that is less than `tiny(T)`.  There are exactly seven (7) finite floating point
values of type `T` that are greater than `huge(T)`.  They are given by this expression:
`[nextfloat(huge(T), n) for n=1:7]` where `T` is one of `{Float64, Float32, Float16}`.

These exceptionally large and exceptionally small values may arise in the course of
computation. It is important to use inequality tests rather than test for equality
or doesnotequal when ascertaining whether to saturate a result or not.

----

__THIS IS REQUIRED OF THE IMPLEMENTATION__ 

It is _necessary_ that none of the exceptionally large or exceptionally small finite, nonzero values
be permitted to propogate.  The exceptionally large values (seven for each type) must be saturated
to _huge_ and the exceptionally small value (one for each type) must be saturated to _tiny_ when
generated.  To allow an exceptional finite value to be returned as a computational result
is to abrogate the logical consistancy of the implementation.

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

#### supported functions

In addition to the familiar functions that work with Float64, Float32, Float16,    
(comparisions, floating part decompositions, arithmetic, elementary functions)

-    square, cube

-    string, show, 
-    typemax, typemin, floatmax, floatmin
    
-    significand, exponent, precision
-    prevfloat, nextfloat, isequal, isless
    
-    (==), (!=), (<), (<=), (>=), (>)
-    (+), (-), (*), (/), (^)
    
-    inv, div, rem, fld, mod, cld

-    round, trunc, ceil, floor (single arg forms)
    
-    abs, signbit, copysign, flipsign, sign
-    frexp, ldexp, modf
    
-    min, max, minmax
-    clamp, sqrt, cbrt, hypot
    
-    exp, expm1, exp2, exp10
-    log, log1p, log2, log10
 
-    sin, cos, tan, csc, sec, cot
-    asin, acos, atan, acsc, asec, acot

-    sinh, cosh, tanh, csch, sech, coth,
-    asinh, acosh, atanh, acsch, asech, acoth


-    sind, cosd, tand, cscd, secd, cotd
-    asind, acosd, atand, acscd, asecd, acotd

-    rad2deg, deg2rad, mod2pi, rem2pi
-    sincos, sinc, sinpi, cospi


----

## Examples
```julia
julia> Float64(0) * inv(Float64(0))
NaN

julia> Finite64(0) * inv(Finite64(0))
0.0

julia> typemax(Finite64) == nextfloat(floatmax(Finite64)) == floatmax(Finite64)
true
```

Finite64|32|16 are saturating at ±floatmax(T) 
