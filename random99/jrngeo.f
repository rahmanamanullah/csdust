CDECK  ID>, JRNGEO.
      SUBROUTINE JRNGEO ( N, CUM, P )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE JRNGEO ( N, CUM, P )                  Ch.Walck    860918
C.
C.    Prepare cumulative vector for geometric distribution with
C.    parameter P so that pseudorandom numbers can be efficiently
C.    obtained with function IRNFNC.
C.
C.    CUM should be dimensioned to at least N in the calling routine
C.
C.----------------------------------------------------------------------
      DIMENSION CUM(N)
      Q = 1.0 - P
      CUM(1) = P
      DO 1 I = 2, N-5
         CUM(I) = CUM(I-1) * Q
    1 CONTINUE
      CALL IRNHIS ( CUM, N-5, 1, CUM )
      RETURN
      END
