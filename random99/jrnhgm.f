CDECK  ID>, JRNHGM.
      SUBROUTINE JRNHGM ( N, M, NS, CUM )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE JRNHGM ( N, M, NS, CUM )              Ch.Walck    860813
C.
C.    Prepare cumulative vector for hypergeometric distribution with
C.    parameters N, M and NS so that pseudorandom numbers can be effi-
C.    ciently obtained with function IRNFNC
C.
C.    CUM should be dimensioned to at least NS+6 in the calling routine
C.
C.----------------------------------------------------------------------
      DIMENSION CUM(*)
C
C     Calculate lower and upper value (normally 0 and NS)
C
      IRL = MAX(0,NS-N+M)
      IRU = MIN(NS,M)
C
C     Calculate probability for lowest possible value (IRL)
C
      P = 1.0
      DO 1 I = 1, NS-IRL
         P = P * FLOAT(N-M+1-I) / FLOAT(N+1-I)
    1 CONTINUE
      DO 2 I = 1, IRL
         P = P * FLOAT(M+1-I)   / FLOAT(N-NS+1+IRL-I)
    2 CONTINUE
      DO 3 I = 1, IRL
         P = P * FLOAT(NS+1-I)  / FLOAT(IRL+1-I)
    3 CONTINUE
C
C     Calculate probabilities by recursive formula
C
      CUM(1) = P
      DO 4 IR = IRL+1, IRU
         K = IR - IRL
         CUM(K+1) = CUM(K) * FLOAT((NS-IR+1)*(M-IR+1))
     +                     / FLOAT((N-M-NS+IR)*IR)
    4 CONTINUE
      CALL IRNHIS ( CUM, K+1, IRL, CUM )
      RETURN
      END
