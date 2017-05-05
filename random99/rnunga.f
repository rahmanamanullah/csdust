CDECK  ID>, RNUNGA.
      FUNCTION RNUNGA ( D, S )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNUNGA ( D, S )                         Ch.Walck    841102
C.
C.    Pseudorandom number from distribution with flat top between
C.    -D and D and Gaussian tails with width S on each side.
C.    Used e.g. for rapidity distributions by UA5.
C.
C.    F(X) = A * EXP(-((X+D)/S)**2/2) / SQRT(2*PI) / S   for      X < -D
C.         = 1 / (2*D)                                   for -D < X <  D
C.         = A * EXP(-((X-D)/S)**2/2) / SQRT(2*PI) / S   for  D < X
C.
C.    where A = SQRT(PI/2) * S / D = CNST * S / D = area of Gaussian
C.    tails if uniform part normalized to have area 1.
C.
C.    Variable:   X     (real)
C.    Parameters: D >=0 (real)
C.                S >0  (real)
C.
C.----------------------------------------------------------------------
C                     SQRT(2/PI)
      PARAMETER (CNST=1.25331414)
      IF ( D .EQ. 0.0 ) THEN
         R = RNNRMT ( 0.0, S )
      ELSE
         PRUN = 1.0 / ( 1.0 + CNST * S / D )
         IF ( RNUNIF(DUM) .LE. PRUN ) THEN
            R = D * ( 2.0 * RNUNIF(PRUN) - 1.0 )
         ELSE
            R = RNNRMT ( 0.0, S )
            R = R + SIGN ( D, R )
         END IF
      END IF
      RNUNGA = R
      RETURN
      END
