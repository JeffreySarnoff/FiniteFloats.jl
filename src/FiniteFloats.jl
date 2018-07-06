module FiniteFloats

export Finite64, Finite32, Finite16,
       typemaxneg, typeminneg,
       square, cube 

import Base: hash, string, show, 
    promote_rule, convert, 
    typemax, typemin, realmax, realmin,
    significand, exponent, precision,
    prevfloat, nextfloat,
    (==), (!=), (<), (<=), (>=), (>), isequal, isless,
    (+), (-), (*), (/), (^),
    inv, div, rem, fld, mod, cld,
    round, trunc, ceil, floor,
    abs, signbit, copysign, flipsign, sign,
    frexp, ldexp, modf,
    min, max, minmax,
    clamp, sqrt, cbrt, hypot,
    exp, expm1, exp2, exp10,
    log, log1p, log2, log10,
    rad2deg, deg2rad, mod2pi, rem2pi,
    sin, cos, tan, csc, sec, cot,
    asin, acos, atan, acsc, asec, acot,
    sinh, cosh, tanh, csch, sech, coth,
    asinh, acosh, atanh, acsch, asech, acoth,
    sincos, sinc, sinpi, cospi,
    sind, cosd, tand, cscd, secd, cotd,
    asind, acosd, atand, acscd, asecd, acotd
        
    
    
primitive type Finite64 <: AbstractFloat 64 end
primitive type Finite32 <: AbstractFloat 32 end
primitive type Finite16 <: AbstractFloat 16 end

Base.typemax(::Type{Finite64}) = 1.7976931348623157e308     #  realmax(Float64)
Base.typemax(::Type{Finite32}) = 3.4028235f38               #  realmax(Float32)
Base.typemax(::Type{Finite16}) = Float16(6.55e4)            #  realmax(Float16)
typemaxneg(::Type{Finite64})   = -1.7976931348623157e308    # -realmax(Float64) 
typemaxneg(::Type{Finite32})   = -3.4028235f38              # -realmax(Float32)
typemaxneg(::Type{Finite16})   = Float16(-6.55e4)           # -realmax(Float16)

Base.typemin(::Type{Finite64}) = 2.2250738585072014e-308    #  realmin(Float64)
Base.typemin(::Type{Finite32}) = 1.1754944f-38              #  realmin(Float32)
Base.typemin(::Type{Finite16}) = Float16(6.104e-5)          #  realmin(Float16)
typeminneg(::Type{Finite64})   = -2.2250738585072014e-308   # -realmin(Float64) 
typeminneg(::Type{Finite32})   = -1.1754944f-38             # -realmin(Float32)
typeminneg(::Type{Finite16})   = Float16(-6.104e-5)         # -realmin(Float16)

Base.realmax(::Type{Finite64}) = Base.typemax(Finite64)
Base.realmax(::Type{Finite32}) = Base.typemax(Finite32)
Base.realmax(::Type{Finite16}) = Base.typemax(Finite16)
Base.realmin(::Type{Finite64}) = Base.typemin(Finite64)
Base.realmin(::Type{Finite32}) = Base.typemin(Finite32)
Base.realmin(::Type{Finite16}) = Base.typemin(Finite16)

# consts are used to accelerate replacement of infinities

const Finite64_maxpos = typemax(Finite64)
const Finite64_minpos = typemin(Finite64)
const Finite64_maxneg = typemaxneg(Finite64)
const Finite64_minneg = typeminneg(Finite64)

const Finite32_maxpos = typemax(Finite32)
const Finite32_minpos = typemin(Finite32)
const Finite32_maxneg = typemaxneg(Finite32)
const Finite32_minneg = typeminneg(Finite32)

const Finite16_maxpos = typemax(Finite16)
const Finite16_minpos = typemin(Finite16)
const Finite16_maxneg = typemaxneg(Finite16)
const Finite16_minneg = typeminneg(Finite16)





@inline function FiniteFloat64(x::Float64)
    isfinite(x) && return x
    if isinf(x)
       signbit(x) ? Finite64_maxneg : Finite64_maxpos
    else
       throw(DomainError("NaN encountered"))
    end
end

@inline function FiniteFloat32(x::Float32)
    isfinite(x) && return x
    if isinf(x)
       signbit(x) ? Finite32_maxneg : Finite32_maxpos
    else
       throw(DomainError("NaN32 encountered"))
    end
end

@inline function FiniteFloat16(x::Float16)
    isfinite(x) && return x
    if isinf(x)
       signbit(x) ? Finite16_maxneg : Finite16_maxpos
    else
       throw(DomainError("NaN16 encountered"))
    end
end


Finite64(x::Float64) = reinterpret(Finite64, FiniteFloat64(x))
Finite32(x::Float32) = reinterpret(Finite32, FiniteFloat32(x))
Finite16(x::Float16) = reinterpret(Finite16, FiniteFloat16(x))

Float64(x::Finite64) = reinterpret(Float64, x)
Float32(x::Finite32) = reinterpret(Float32, x)
Float16(x::Finite16) = reinterpret(Float16, x)

for O in ( :string,
           :prevfloat, :nextfloat,
           :inv, :abs, :sqrt, :cbrt,
           :exp, :expm1, :exp2, :exp10,
           :log, :log1p, :log2, :log10,
           :rad2deg, :deg2rad, :mod2pi, :rem2pi,
           :sin, :cos, :tan, :csc, :sec, :cot,
           :asin, :acos, :atan, :acsc, :asec, :acot,
           :sinh, :cosh, :tanh, :csch, :sech, :coth,
           :asinh, :acosh, :atanh, :acsch, :asech, :acoth,
           :sinc, :sinpi, :cospi,
           :sind, :cosd, :tand, :cscd, :secd, :cotd,
           :asind, :acosd, :atand, :acscd, :asecd, :acotd
          )       
    @eval begin
        $O(x::Finite64) = Finite64($O(Float64(x))) 
        $O(x::Finite32) = Finite32($O(Float32(x))) 
        $O(x::Finite16) = Finite16($O(Float16(x))) 
    end
end

for (T,F) in ( (:Finite64, :Float64), (:Finite32, :Float32), (:Finite16, :Float16) )
   @eval begin
       Base.String(x::$T) = String($F(x))
       $T(x::String) = $T(parse($F, x))
       Base.show(io::IO, x::$T) = show(io, $F(x))
       square(x::$T) = $T($F(x)*$F(x))
       cube(x::$T) = $T($F(x)*$F(x)*$F(x))              
   end
end



#=            
 div, rem, fld, mod, cld,
 round, trunc, ceil, floor,
 signbit, copysign, flipsign, sign,
 frexp, ldexp, modf,
 min, max, minmax,
 clamp, hypot,
 sincos
=#


Base.promote_rule(::Type{Float64}, :Type{Finite64}) = Finite64
Base.promote_rule(::Type{Float32}, :Type{Finite32}) = Finite32
Base.promote_rule(::Type{Float16}, :Type{Finite16}) = Finite16

Base.promote_rule(::Type{Finite64}, :Type{Finite32}) = Finite64
Base.promote_rule(::Type{Finite64}, :Type{Finite16}) = Finite64
Base.promote_rule(::Type{Finite32}, :Type{Finite16}) = Finite32

Finite64(x::Finite32) = Finite64(Float64(Float32(x)))
Finite64(x::Finite16) = Finite64(Float64(Float16(x)))
Finite32(x::Finite16) = Finite32(Float32(Float16(x)))

Base.promote_rule(::Type{Float32}, :Type{Finite64}) = Finite64
Base.promote_rule(::Type{Float16}, :Type{Finite64}) = Finite64
Base.promote_rule(::Type{Float16}, :Type{Finite32}) = Finite32
Base.promote_rule(::Type{Float64}, :Type{Finite32}) = Finite64
Base.promote_rule(::Type{Float64}, :Type{Finite16}) = Finite64
Base.promote_rule(::Type{Float32}, :Type{Finite16}) = Finite32


end # FiniteFloats
