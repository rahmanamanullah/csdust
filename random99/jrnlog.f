CDECK  ID>, JRNLOG.
      SUBROUTINE JRNLOG ( N, CUM, P )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE JRNLOG ( N, CUM, P )                  Ch.Walck    860918
C.
C.    Prepare cumulative vector for logarithmic distribution with
C.    parameter P so that pseudorandom numbers can be efficiently
C.    obtained with function IRNFNC.
C.
C.    CUM should be dimensioned to at least N in the calling routine.
C.
C.----------------------------------------------------------------------
      DIMENSION CUM(N)
      Q = 1.0 - P
      ALPHA = 1.0 / LOG(P)
      R = - ALPHA * Q
      CUM(1) = R
      DO 1 I = 2, N-5
         R = R * Q
         CUM(I) = R / FLOAT(I)
    1 CONTINUE
      CALL IRNHIS ( CUM, N-5, 1, CUM )
      RETURN
      END
