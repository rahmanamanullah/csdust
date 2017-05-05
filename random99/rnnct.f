CDECK  ID>, RNNCT.
      FUNCTION RNNCT ( NDF, DELTA )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNNCT ( NDF, DELTA )                    Ch.Walck    961124
C.
C.    Pseudorandom number from a non-central t-distribution with NDF 
C.    degrees of freedom and non-central parameter DELTA.
C.
C.    Variable:          T       (real)
C.    Parameters:        NDF > 0 (integer)
C.                       DELTA   (real)
C.
C.----------------------------------------------------------------------
      RNNCT = RNNRMT(DELTA,1.0) / SQRT(RNCHSQ(NDF)/FLOAT(NDF))
      RETURN
      END
