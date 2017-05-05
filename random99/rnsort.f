CDECK  ID>, RNSORT.
      SUBROUTINE RNSORT ( A, B, N, M )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE RNSORT ( A, B, N, M )                 Ch.Walck    861019
C.
C.    Give, in vector B(N), M random elements from A(N). For M=N the
C.    elements of B is a random permutation of the elements in A.
C.    Even if M<N the vector B should be dimensioned to at least N in
C.    the calling program.
C.
C.----------------------------------------------------------------------
      DIMENSION A(N), B(N)
      DO 1 I = 1, N
    1 B(I) = A(I)
      K = 1
    2 L = IRNUNI(K,N)
      SV   = B(L)
      B(L) = B(K)
      B(K) = SV
      K = K + 1
      IF ( K .LE. M ) GO TO 2
      RETURN
      END
