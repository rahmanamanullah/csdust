CDECK  ID>, JRNPOI.
      SUBROUTINE JRNPOI ( N, CUM, MU )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE JRNPOI ( N, CUM, MU )                 Ch.Walck    860911
C.
C.    Prepare cumulative vector for Poisson distribution with
C.    parameter MU so that pseudorandom numbers can be effi-
C.    ciently obtained with function IRNFNC
C.
C.    CUM should be dimensioned to at least N words in the calling
C.    routine (includes 5 status words plus cumulative vector).
C.
C.----------------------------------------------------------------------
      DIMENSION CUM(N)
      REAL MU
      CUM(1) = EXP(-MU)
      DO 1 I = 2, N-5
         CUM(I) = CUM(I-1) * MU / FLOAT(I-1)
    1 CONTINUE
      CALL IRNHIS ( CUM, N-5, 0, CUM )
      RETURN
      END
