CDECK  ID>, JRNBIN.
      SUBROUTINE JRNBIN ( N, P, CUM )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE JRNBIN ( N, P, CUM )                  Ch.Walck    860813
C.
C.    Prepare cumulative vector for binomial distribution with para-
C.    meters N and P so that pseudorandom numbers can be efficiently
C.    obtained with function IRNFNC
C.
C.    CUM should be dimensioned to at least N+6 in the calling routine
C.
C.----------------------------------------------------------------------
      DIMENSION CUM(*)
      Q = 1.0 - P
      CUM(1) = Q**N
      DO 1 I = 1, N
         CUM(I+1) = CUM(I)*FLOAT(N+1-I)*P/Q/FLOAT(I)
    1 CONTINUE
      CALL IRNHIS ( CUM, N+1, 0, CUM )
      RETURN
      END
