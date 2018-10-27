abstract type AbstractFinite <: AbstractFloat end

primitive type Finite64 <: AbstractFinite 64 end
primitive type Finite32 <: AbstractFinite 32 end
primitive type Finite16 <: AbstractFinite 16 end


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


float(::Type{Finite64}) = Float64
float(::Type{Finite32}) = Float32
float(::Type{Finite16}) = Float16

finite(::Type{Float64}) = Finite64
finite(::Type{Float32}) = Finite32
finite(::Type{Float16}) = Finite16

finite(::Type{Int64}) = Finite64
finite(::Type{Int32}) = Finite32
finite(::Type{Int16}) = Finite16

signed(::Type{Finite64}) = Int64
signed(::Type{Finite32}) = Int32
signed(::Type{Finite16}) = Int16

finite(::Type{UInt64}) = Finite64
finite(::Type{UInt32}) = Finite32
finite(::Type{UInt16}) = Finite16

unsigned(::Type{Finite64}) = UInt64
unsigned(::Type{Finite32}) = UInt32
unsigned(::Type{Finite16}) = UInt16

Base.typemax(::Type{Finite64}) = Finite64(1.7976931348623157e308)     #  realmax(Float64)
Base.typemax(::Type{Finite32}) = Finite32(3.4028235f38)               #  realmax(Float32)
Base.typemax(::Type{Finite16}) = Finite16(Float16(6.55e4))            #  realmax(Float16)
typemaxneg(::Type{Finite64})   = Finite64(-1.7976931348623157e308)    # -realmax(Float64) 
typemaxneg(::Type{Finite32})   = Finite32(-3.4028235f38)              # -realmax(Float32)
typemaxneg(::Type{Finite16})   = Finite16(Float16(-6.55e4))           # -realmax(Float16)

Base.typemin(::Type{Finite64}) = Finite64(2.2250738585072014e-308)    #  realmin(Float64)
Base.typemin(::Type{Finite32}) = Finite32(1.1754944f-38)              #  realmin(Float32)
Base.typemin(::Type{Finite16}) = Finite16(Float16(6.104e-5))          #  realmin(Float16)
typeminneg(::Type{Finite64})   = Finite64(-2.2250738585072014e-308)   # -realmin(Float64) 
typeminneg(::Type{Finite32})   = Finite32(-1.1754944f-38)             # -realmin(Float32)
typeminneg(::Type{Finite16})   = Finite16(Float16(-6.104e-5))         # -realmin(Float16)

Base.floatmax(::Type{Finite64}) = Base.typemax(Finite64)
Base.floatmax(::Type{Finite32}) = Base.typemax(Finite32)
Base.floatmax(::Type{Finite16}) = Base.typemax(Finite16)
Base.floatmin(::Type{Finite64}) = Base.typemin(Finite64)
Base.floatmin(::Type{Finite32}) = Base.typemin(Finite32)
Base.floatmin(::Type{Finite16}) = Base.typemin(Finite16)

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

for O in ( :(-), :(+),
           :string,
           :sign,
           :prevfloat, :nextfloat,
           :round, :trunc, :ceil, :floor,
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
   end
end


for O in ( :flipsign, :copysign,
           :min, :max, 
           :(+), :(-), :(*), :(/), :(^),  
           :div, :rem, :fld, :mod, :cld,
           :hypot 
          )       
    @eval begin
        $O(x::Finite64, y::Finite64) = Finite64($O(Float64(x), Float64(y))) 
        $O(x::Finite32, y::Finite32) = Finite32($O(Float32(x), Float32(y))) 
        $O(x::Finite16, y::Finite16) = Finite16($O(Float16(x), Float16(y))) 
    end
end


#=
was 

for O in ( :flipsign, :copysign,
           :min, :max, 
           :(+), :(-), :(*), :(/), :(^),  
           :div, :rem, :fld, :mod, :cld,
           :hypot 
          )       
    @eval begin
        $O(x::Finite64, y::Finite64) = Finite64($O(Float64(x), Float64(y))) 
        $O(x::Finite32, y::Finite32) = Finite32($O(Float32(x), Float32(y))) 
        $O(x::Finite16, y::Finite16) = Finite16($O(Float16(x), Float16(y))) 
    end
end

To obtain saturating underflow, `*`, `/`, `inv`, `^`, `hypot` must be intercepted.
There may be other operations to intercept, `square`, `cube` come to mind.
=#

tiny(::Type{Float64}) = nextfloat(inv(prevfloat(Inf64)))
tiny(::Type{Float32}) = nextfloat(inv(prevfloat(Inf32)))
tiny(::Type{Float16}) = nextfloat(inv(prevfloat(Inf16)))

huge(::Type{Float64}) = inv(tiny(Float64))
huge(::Type{Float32}) = inv(tiny(Float32))
huge(::Type{Float16}) = inv(tiny(Float16))


#=
    Both pure magnitude (preceeding) and signed magnitude (following)
    versions of `tiny` and `huge` are defined.  Both are made available
    in an effort to facillitate future exploration.
=#

tinypos(::Type{Finite64}) = Finite64(nextfloat(inv(prevfloat(Inf64))))
tinypos(::Type{Finite32}) = Finite32(nextfloat(inv(prevfloat(Inf32))))
tinypos(::Type{Finite16}) = Finite16(nextfloat(inv(prevfloat(Inf16))))

hugepos(::Type{Finite64}) = Finite64(inv(tiny(Float64)))
hugepos(::Type{Finite32}) = Finite32(inv(tiny(Float32)))
hugepos(::Type{Finite16}) = Finite16(inv(tiny(Float16)))

tinyneg(::Type{Finite64}) = -tinypos(Finite64)
tinyneg(::Type{Finite32}) = -tinypos(Finite32)
tinyneg(::Type{Finite16}) = -tinypos(Finite16)

hugeneg(::Type{Finite64}) = -hugepos(Finite64)
hugeneg(::Type{Finite32}) = -hugepos(Finite32)
hugeneg(::Type{Finite16}) = -hugepos(Finite16)


#=
    These values are used in arithmetically critical paths.
    We define `const`s to gain a few units of throughput
    when computing at the advancing cusp.
=#

const TinyPos64 = tinypos(Finite64)
const TinyPos32 = tinypos(Finite32)
const TinyPos16 = tinypos(Finite16)

const HugePos64 = hugepos(Finite64)
const HugePos32 = hugepos(Finite32)
const HugePos16 = hugepos(Finite16)

const TinyNeg64 = tinyneg(Finite64)
const TinyNeg32 = tinyneg(Finite32)
const TinyNeg16 = tinyneg(Finite16)

const HugeNeg64 = hugeneg(Finite64)
const HugeNeg32 = hugeneg(Finite32)
const HugeNeg16 = hugeneg(Finite16)


for (T,P,N) in ( (:Finite64, :TinyPos64, :TinyNeg64),
                 (:Finite64, :TinyPos64, :TinyNeg64),
                 (:Finite64, :TinyPos64, :TinyNeg64) )
  @eval begin
    @inline saturate_tiny(computed::$T, signbit::Bool=false)
        !iszero(computed) ? computed : ifelse(signbit, $N, $P)
  end
end

for (T,P,N) in ( (:Finite64, :HugePos64, :HugeNeg64),
                 (:Finite64, :HugePos64, :HugeNeg64),
                 (:Finite64, :HugePos64, :HugeNeg64) )
  @eval begin
    @inline saturate_huge(computed::$T, signbit::Bool=false)
        !iszero(computed) ? computed : ifelse(signbit, $N, $P)
  end
end

macro saturating(fp, T)
    quote
      begin
        local absfp = abs($fp)
        return tiny($T) <= absfp <= huge($T) ?
                                         $fp :
                   ( absfp > huge($T)        ? 
                     copysign(huge($T), $fp) : 
                     copysign(tiny($T), $fp)   )
      end
    end
end

for (T,F) in ( (:Finite64, :Float64), (:Finite32, :Float32), (:Finite16, :Float16) )
   @eval begin
       function square(x::$T)
            fp = $F(x)*$F(x)
            fp = @saturating(fp,$T)
            return $T(fp)    
       end
        
       function cube(x::$T)
            fp = $F(x)*$F(x)*$F(x)
            fp = @saturating(fp,$T)
            return $T(fp)
       end
   end
end

for O in ( :(+), :(-), :(*), :(/), :(^),  
           :hypot 
         )       
    @eval begin
        function $O(x::Finite64, y::Finite64)
            fp = $O(Float64(x), Float64(y))
            fp = @saturating(fp, Float64)
            return Finite64(fp)
        end
        
        function $O(x::Finite32, y::Finite32)
            fp = $O(Float32(x), Float32(y))
            fp = @saturating(fp, Float32)
            return Finite32(fp)
        end
        
        function $O(x::Finite16, y::Finite16)
            fp = $O(Float16(x), Float16(y))
            fp = @saturating(fp, Float16)
            return Finite16(fp)
        end
    end
end



for O in ( :flipsign, :copysign,
           :min, :max, 
           :div, :rem, :fld, :mod, :cld
         )       
    @eval begin
        $O(x::Finite64, y::Finite64) = Finite64($O(Float64(x), Float64(y))) 
        $O(x::Finite32, y::Finite32) = Finite32($O(Float32(x), Float32(y))) 
        $O(x::Finite16, y::Finite16) = Finite16($O(Float16(x), Float16(y))) 
    end
end

for O in ( :(==), :(!=),
           :(<), :(<=), :(>=), :(>),  
           :isequal, :isless
          )       
    @eval begin
        $O(x::Finite64, y::Finite64) = $O(Float64(x), Float64(y)) 
        $O(x::Finite32, y::Finite32) = $O(Float32(x), Float32(y)) 
        $O(x::Finite16, y::Finite16) = $O(Float16(x), Float16(y)) 
    end
end

signbit(x::Finite64) = signbit(Float64(x))
signbit(x::Finite32) = signbit(Float32(x))
signbit(x::Finite16) = signbit(Float16(x))

for O in ( :minmax, :modf )       
    @eval begin
        $O(x::Finite64, y::Finite64) = Finite64.($O(Float64(x), Float64(y))) 
        $O(x::Finite32, y::Finite32) = Finite32.($O(Float32(x), Float32(y))) 
        $O(x::Finite16, y::Finite16) = Finite16.($O(Float16(x), Float16(y)))
    end
end

frexp(x::Finite64) = map((a,b)->(Finite64(a), b), frexp(Float64(x))...,)
frexp(x::Finite32) = map((a,b)->(Finite32(a), b), frexp(Float32(x))...,)
frexp(x::Finite16) = map((a,b)->(Finite16(a), b), frexp(Float16(x))...,)

ldexp(x::Finite64, y::Int) = Finite64(ldexp(Float64(x), y))
ldexp(x::Finite32, y::Int) = Finite32(ldexp(Float32(x), y))
ldexp(x::Finite16, y::Int) = Finite16(ldexp(Float16(x), y))

sincos(x::Finite64) = map((a,b)->(Finite64(a), Finite64(b)), sincos(Float64(x))...,)
sincos(x::Finite32) = map((a,b)->(Finite32(a), Finite32(b)), sincos(Float32(x))...,)
sincos(x::Finite16) = map((a,b)->(Finite16(a), Finite16(b)), sincos(Float16(x))...,)

clamp(x::T, lo::T, hi::T) where {T<:Finite64} = Finite64(clamp(Float64(x), Float64(lo), Float64(hi)))

for (T,F) in ( (:Finite64, :Float64), (:Finite32, :Float32), (:Finite16, :Float16) )
   @eval begin
       clamp(x::$T, lo::$T, hi::$T) = $T( clamp($F(x), $F(lo), $F(hi)) )
   end
end



Base.promote_rule(::Type{Float64}, ::Type{Finite64}) = Finite64
Base.promote_rule(::Type{Float32}, ::Type{Finite32}) = Finite32
Base.promote_rule(::Type{Float16}, ::Type{Finite16}) = Finite16

Base.promote_rule(::Type{Finite64}, ::Type{Finite32}) = Finite64
Base.promote_rule(::Type{Finite64}, ::Type{Finite16}) = Finite64
Base.promote_rule(::Type{Finite32}, ::Type{Finite16}) = Finite32

Finite64(x::Finite32) = Finite64(Float64(Float32(x)))
Finite64(x::Finite16) = Finite64(Float64(Float16(x)))
Finite32(x::Finite64) = Finite32(Float32(Float64(x)))
Finite32(x::Finite16) = Finite32(Float32(Float16(x)))
Finite16(x::Finite64) = Finite16(Float16(Float64(x)))
Finite16(x::Finite16) = Finite16(Float16(Float16(x)))

Base.promote_rule(::Type{Float32}, ::Type{Finite64}) = Finite64
Base.promote_rule(::Type{Float16}, ::Type{Finite64}) = Finite64
Base.promote_rule(::Type{Float16}, ::Type{Finite32}) = Finite32
Base.promote_rule(::Type{Float64}, ::Type{Finite32}) = Finite64
Base.promote_rule(::Type{Float64}, ::Type{Finite16}) = Finite64
Base.promote_rule(::Type{Float32}, ::Type{Finite16}) = Finite32

Finite64(x::Float32) = Finite64(Float64(x))
Finite64(x::Float16) = Finite64(Float64(x))
Finite32(x::Float64) = Finite32(Float32(x))
Finite32(x::Float16) = Finite32(Float32(x))
Finite16(x::Float64) = Finite16(Float16(x))
Finite16(x::Float32) = Finite16(Float16(x))

Base.promote_rule(::Type{Int64}, ::Type{Finite64}) = Finite64
Base.promote_rule(::Type{Int32}, ::Type{Finite32}) = Finite32
Base.promote_rule(::Type{Int16}, ::Type{Finite16}) = Finite16

Base.promote_rule(::Type{Int32}, ::Type{Finite64}) = Finite64
Base.promote_rule(::Type{Int16}, ::Type{Finite64}) = Finite64
Base.promote_rule(::Type{Int16}, ::Type{Finite32}) = Finite32
Base.promote_rule(::Type{Int64}, ::Type{Finite32}) = Finite64
Base.promote_rule(::Type{Int64}, ::Type{Finite16}) = Finite64
Base.promote_rule(::Type{Int32}, ::Type{Finite16}) = Finite32

Finite64(x::Int64) = Finite64(Float64(x))
Finite64(x::Int32) = Finite64(Float64(x))
Finite64(x::Int16) = Finite64(Float64(x))
Finite32(x::Int64) = Finite32(Float32(x))
Finite32(x::Int32) = Finite32(Float32(x))
Finite32(x::Int16) = Finite32(Float32(x))
Finite16(x::Int64) = Finite16(Float16(x))
Finite16(x::Int32) = Finite16(Float16(x))
Finite16(x::Int16) = Finite16(Float16(x))

Int64(x::Finite64) = Int64(Float64(x))
Int64(x::Finite32) = Int64(Float64(Float32(x)))
Int64(x::Finite16) = Int64(Float64(Float16(x)))
Int32(x::Finite64) = Int32(Float64(x))
Int32(x::Finite32) = Int32(Float32(x))
Int32(x::Finite16) = Int32(Float16(x))
Int16(x::Finite64) = Int16(Float64(x))
Int16(x::Finite32) = Int16(Float32(x))
Int16(x::Finite16) = Int16(Float16(x))
