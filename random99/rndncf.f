CDECK  ID>, RNDNCF.
      FUNCTION RNDNCF ( M, N, MLAM, NLAM )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNDNCF ( M, N, MLAM, NLAM )             Ch.Walck    961124
C.
C.    Pseudorandom number from a doubly non-central F-distribution with 
C.    M and N degrees of freedom and non-central parameters MLAM, NLAM
C.    (the first corresponding to the non-central chi-square distribution
C.    with M degrees of freedom in the nominator).
C.
C.    Variable:          F > 0           (real)
C.    Parameters:        M > 0 and N > 0 (integers)
C.                       MLAM >=0
C.                       NLAM >=0
C.
C.----------------------------------------------------------------------
      REAL MLAM, NLAM
      RNDNCF = ( RNNCX2(M,MLAM)/FLOAT(M) ) / ( RNNCX2(N,NLAM)/FLOAT(N) )
      RETURN
      END
