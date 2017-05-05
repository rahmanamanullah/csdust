CDECK  ID>, RNDNCT.
      FUNCTION RNDNCT ( NDF, DELTA, LAMBDA )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNDNCT ( NDF, DELTA, LAMBDA )           Ch.Walck    981025
C.
C.    Pseudorandom number from a doubly non-central t-distribution with 
C.    NDF degrees of freedom and non-central parameters DELTA and LAMBDA.
C.
C.    Variable:          T         (real)
C.    Parameters:        NDF > 0   (integer)
C.                       DELTA     (real)
C.                       LAMBDA >0 (real)
C.
C.----------------------------------------------------------------------
      REAL LAMBDA
      RNDNCT = RNNRMT(DELTA,1.0) / SQRT(RNNSX2(NDF,LAMBDA)/FLOAT(NDF))
      RETURN
      END
