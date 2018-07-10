for F in (:diag, :diagm, :expm, :exmp!, :logdet, :norm, :normalize, :normalize!, :pinv, :qr, :transpose)
   @eval $F(x::AbstractVector{T}) where {T<:AbstractFinite} = T.($F(float(T).(x)))
end

for F in (:adjoint, :diag, :diagm, :expm, :exmp!, :logdet, :norm, :normalize, :normalize!,
          :nullspace, :pinv, :qr, :transpose)
   @eval $F(x::Vector{T}) where {T<:AbstractFinite} = T.($F(float(T).(x)))
end

for F in (:adjoint, :bunchkaufman, :cond, :condskeel, :det, :diag, :diagind,
          :eigmax, :eigmin, :eigvecs, :expm, :expm!,
          :isdiag, :ishermitian, :isposdef, :isposdef!, :issymmetric, :istril, :istriu,
          :logabsdet, :logdet, :lu, :norm, :opnorm, :qr, :rank, :svdvals,
          :tr, :transpose, :tril, :tril!, :triu, :triu!)
   @eval $F(x::AbstractMatrix{T}) where {T<:AbstractFinite} = T.($F(float(T).(x)))
end
          
for F in (:bunchkaufman, :cholesky, :cholesky!, :cond, :condskeel,
          :det, :diag, :diagind, :eigen, :eigmax, :eigmin, :eigvals, :eigvecs,
          :expm, :expm!, :factorize, :hessenberg, :isdiag, :ishermitian,
          :isposdef, :isposdef!, :issymmetric, :istril, :istriu, :logabsdet,
          :logdet, :lu, :lu!, :norm, :nullspace, :opnorm, :pinv, :qr, :qr!,
          :rank, :schur, :svd, :svdvals, :tr, :transpose, :tril, :tril!, :triu, :triu!)
   @eval $F(x::Matrix{T}) where {T<:AbstractFinite} = T.($F(float(T).(x)))
end

for F in (:adjoint, :expm, :exmp!, :logdet, :norm, :transpose)
   @eval $F(x::AbstractArray{T}) where {T<:AbstractFinite} = T.($F(float(T).(x)))
end

for F in (:adjoint, :expm, :exmp!, :logdet, :norm, :transpose)
   @eval $F(x::Array{T}) where {T<:AbstractFinite} = T.($F(float(T).(x)))
end
