CDECK  ID>, RNMULN.
      SUBROUTINE RNMULN ( X, S, N, R )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE RNMULN ( X, S, N , R )                Ch.Walck    841008
C.
C.    Generate random numbers from a multinormal distribution with
C.    means in vector X(N) and variance matrix in V (packed symmetric
C.    matrix with N*(N+1)/2 elements). Before calling this routine
C.    one should find a matrix S which is a lower triangular packed
C.    (N*(N+1)/2 elements) matrix satisfying V = S * ST. This is
C.    done e.g. by CALL TRCHLU ( V, S, N ).
C.
C.    Resulting random numbers are returned in vector R(N).
C.
C.    Dimension of RN below limit the routine to N less or equal to 50.
C.
C.----------------------------------------------------------------------
      DIMENSION R(*), RN(50), X(*), S(*)
C--
C--   Generate N independent random numbers from N(0,1)-distribution
C--
C     WARNING: This loop could cause compiler optimization problems
      DO 10 I = 1, N
         RN(I) = RNNRMT ( 0.0, 1.0 )
   10 CONTINUE
C--
C--   Transform into random numbers with variance matrix V
C--
      CALL TRLA ( S, RN, R, N, 1 )
C--
C--   Shift to correct means
C--
      DO 20 I = 1, N
         R(I) = R(I) + X(I)
   20 CONTINUE
      RETURN
      END
