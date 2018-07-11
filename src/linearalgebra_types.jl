for O in (:Adjoint, :Transpose,:Cholesky, :Hessenberg, :LDLt, :LQ, :LU, :QR)
    @eval LinearAlgebra.$O(::Type{T}) where {T} = LinearAlgebra.$O{T, Array{T,1}}
end

for O in (:BunchKaufman, :GeneralizedSVD, :Schur)
    @eval LinearAlgebra.$O(::Type{T}) where {T} = LinearAlgebra.$O{T, Array{T,2}}
end


for (O,F) in ( (:Adjoint, :adjoint), (:Transpose, :transpose),
               (:BunchKaufman, :bunchkaufman), (:Cholesky, :cholesky), (:Hessenberg, :hessenberg),
               (:GeneralizedSVD, :svd), (:LDLt, :ldlt), (:LQ, :lq), (:LU, :lu), (:QR, :qr), (:Schur, :schur))
    @eval begin
        LinearAlgebra.$F(x::A) where {N, T<:AbstractFinite, A<:AbstractArray{T,N}} =
            T.(LinearAlgebra.$F(float(T).(x)))     
    end
end
