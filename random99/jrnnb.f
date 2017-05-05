CDECK  ID>, JRNNB.
      SUBROUTINE JRNNB ( N, CUM, AVN, K )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE JRNNB ( N, CUM, AVN , K )             Ch.Walck    860911
C.
C.    Prepare cumulative vector for negative binomial distribution with
C.    Parameters N, M and NS so that pseudorandom numbers can be effi-
C.    ciently obtained with function IRNFNC
C.
C.    CUM should be dimensioned to at least N words in the calling
C.    routine (includes 5 status words plus cumulative vector).
C.
C.----------------------------------------------------------------------
      DIMENSION CUM(N)
      REAL K
      P = 1.0 / ( 1.0 + AVN/K )
      Q = 1.0 - P
      CUM(1) = P**K
      DO 1 I = 2, N-5
         CUM(I) = CUM(I-1) * Q * ( FLOAT(I) + K - 2.0 ) / FLOAT(I-1)
    1 CONTINUE
      CALL IRNHIS ( CUM, N-5, 0, CUM )
      NBIN = CUM(1)
C
C     If probabilities still nonzero at end of vector print warning
C
      IF ( NBIN .EQ. N-5 ) THEN
         TAIL = 1.0 - CUM(NBIN+5)
         IF ( TAIL .GE. 0.000001 ) WRITE (6,1001) TAIL
 1001    FORMAT (' JRNNB - excluded tail of negative binomial contains '
     +          ,'the fraction',E12.3)
      END IF
      RETURN
      END
