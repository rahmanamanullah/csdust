CDECK  ID>, RNNCF.
      FUNCTION RNNCF ( M, N, LAMBDA )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNNCF ( M, N, LAMBDA )                  Ch.Walck    961124
C.
C.    Pseudorandom number from a non-central F-distribution with M
C.    and N degrees of freedom and non-central parameter LAMBDA.
C.
C.    Variable:          F > 0           (real)
C.    Parameters:        M > 0 and N > 0 (integers)
C.                       LAMBDA >=0
C.
C.----------------------------------------------------------------------
      REAL LAMBDA
      RNNCF = ( RNNCX2(M,LAMBDA)/FLOAT(M) ) / ( RNCHSQ(N)/FLOAT(N) )
      RETURN
      END
