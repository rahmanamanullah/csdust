CDECK  ID>, RNF.
      FUNCTION RNF ( M, N )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNF ( M, N )                            Ch.Walck    841102
C.
C.    Pseudorandom number from Fisher-Snedecor F-distribution with M
C.    and N degrees of freedom.
C.
C.                 m/2   n/2                 m/2-1
C.                m     n    Gamma((m+n)/2) F
C.    F(F:m,n) =  -----------------------------------
C.                                            (m+n)/2
C.                Gamma(m/2) Gamma(n/2) (m+nF)
C.
C.    Variable:          F > 0           (real)
C.    Parameters:        M > 0 and N > 0 (integers)
C.
C.    Expectation value: E(F) = N/(N-2)                  for   N > 2
C.    Variance:          V(F) = 2*N**2 * (M+N-2)
C.                              / N / (N-2)**2 / (N-4)   for   N > 4
C.
C.----------------------------------------------------------------------
      RNF = ( RNCHSQ(M)/FLOAT(M) ) / ( RNCHSQ(N)/FLOAT(N) )
      RETURN
      END
