CDECK  ID>, RNNCX2.
      FUNCTION RNNCX2 ( NDF, LAMBDA )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNNCX2 ( NDF, LAMBDA )                  Ch.Walck    961124
C.
C.    Pseudorandom number from a non-central chisquared distribution 
C.    with NDF degrees of freedom and non-central parameter LAMBDA.
C.
C.    Variable:          X      >=0  (real)
C.    Parameters:        NDF    > 0  (integer)
C.                       LAMBDA >=0  (real)
C.
C.----------------------------------------------------------------------
      REAL LAMBDA, MU
      MU = SQRT(LAMBDA/FLOAT(NDF))
      R  = 0.0
      DO I = 1, NDF
         R = R + RNNRMT(MU,1.0)**2
      END DO
      RNNCX2 = R
      RETURN
      END
