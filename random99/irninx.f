CDECK  ID>, IRNINX.
      SUBROUTINE IRNINX ( INDEX, N, M )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE IRNINX ( INDEX, N, M )                Ch.Walck    861018
C.
C.    Give, in vector INDEX(N), M random indices in the range 1 to N.
C.
C.----------------------------------------------------------------------
      DIMENSION INDEX(N)
      DO 1 I = 1, N
    1 INDEX(I) = I
      K = 1
    2 L = IRNUNI(K,N)
      ISV      = INDEX(L)
      INDEX(L) = INDEX(K)
      INDEX(K) = ISV
      K = K + 1
      IF ( K .LE. M ) GO TO 2
      RETURN
      END
