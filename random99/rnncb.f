CDECK  ID>, RNNCB.
      FUNCTION RNNCB ( M, N, LAMBDA )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNNCB ( M, N, LAMBDA )                  Ch.Walck    981102
C.
C.    Pseudorandom number from a non-central Beta-distribution with M
C.    and N degrees of freedom and non-central parameter LAMBDA (i.e.
C.    with parameters p=m/2 and q=n/2 and lambda).
C.
C.    Variable:          0 < x < 1       (real)
C.    Parameters:        M > 0 and N > 0 (integers)
C.                       LAMBDA >=0
C.
C.----------------------------------------------------------------------
      REAL LAMBDA
      X2M = RNNCX2(M,LAMBDA)
      X2N = RNCHSQ(N)
      RNNCB = X2M / ( X2M + X2N )
      RETURN
      END
