CDECK  ID>, IRNMNL.
      SUBROUTINE IRNMNL ( NCH, N, P, IRN )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE IRNMNL ( NCH, N, P, IRN )             Ch.Walck    841115
C.
C.    Pseudorandom number from multinomial distribution.
C.
C.    P(R(1),R(2),...R(NCH)) = N! * P(1)**R(1) * P(2)**R(2) * .....
C.                       * P(NCH)**R(NCH) / R(1)! / R(2)! .... / R(NCH)!
C.
C.    Variables:          0 <= R(I) <=N  (integers)      I=1,2,.....,NCH
C.    Parameters:         N > 0          (integer)
C.                        NCH > 0        (integer)
C.                        0 <= P(I) <= 1 (real)
C.    Constraints:        N=SUM(R(I)) and SUM(P(I))=1
C.
C.    Expectation values: E(R(I)) = N*P(I)
C.    Variances:          V(R(I)) = N*P(I)*(1-P(I))
C.    Covariances:        COV(R(I),R(J)) = -N*P(I)*P(J) for I.NE.J
C.
C.    This distribution gives the probability of exactly R(I) outcomes
C.    of type I in N independent trials, where the probability of
C.    outcome I in a single trial is P(I), I=1,2,.....,NCH.
C.
C.----------------------------------------------------------------------
      DIMENSION P(NCH), IRN(NCH)
C
      DO 10 I = 1, NCH
         IRN(I) = 0
   10 CONTINUE
C
      DO 30 I = 1, N
         R = RNUNIF(I)
         J = 0
         S = 0.0
   20    J = J + 1
         S = S + P(J)
         IF ( R.GT.S .AND. J.LT.NCH ) GO TO 20
         IRN(J) = IRN(J) + 1
   30 CONTINUE
      RETURN
      END
