CDECK  ID>, RNPRE.
      SUBROUTINE RNPRE ( FUNC, NCUM, XMIN, XMAX, CUM )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE RNPRE ( FUNC, NCUM, XMIN, XMAX, CUM ) Ch.Walck    1980/81
C.
C.    Initialize random number generation according to function
C.    (random numbers accessed by RNFUNC or RNFNC).
C.
C.    FUNC - Function according to which random numbers
C.           are to be generated
C.           (should be declared external in calling routine)
C.    NCUM - No of elements in reference vector
C.    XMIN - Minimum x value
C.    XMAX - Maximum x value
C.    CUM  - Reference vector of dimension NCUM
C.           (1) = No of elements in cumulative vector
C.           (2) = Lower edge for cumulative vector
C.           (3) = Upper edge for cumulative vector
C.           (4) = Bin width
C.           (5) - (NCUM) = Cumulative vector
C.
C.    External references: FUNC  (user supplied)
C.                         VCOPY (patch RNUTIL)
C.
C.----------------------------------------------------------------------
      DIMENSION CUM(*)
      N      = NCUM - 4
      CUM(1) = N
      CUM(2) = XMIN
      CUM(3) = XMAX
      DX     = (XMAX-XMIN) / (N-1)
      CUM(4) = DX
      CUM(5) = 0.0
      DO 101 I = 2, N
      X  = XMIN + (I-2) * DX
      YY = 0.0
      DO 100 J = 1, 10
  100 YY = YY + (FUNC(X+(J-1)*DX/10.0) + FUNC(X+J*DX/10.0)) * DX / 20.0
  101 CUM(I+4) = YY + CUM(I+3)
C
      DO 102 I = 2, N
  102 IF ( CUM(I+4) .NE. 0.0 ) GO TO 103
  103 KMIN = I + 3
      DO 104 I = 1, N
  104 IF ( CUM(N-I+5) .NE. CUM(N-I+4) ) GO TO 105
  105 KMAX = N - I + 5
      IF ( KMIN .EQ. 5 .AND. KMAX .EQ. N+4 ) RETURN
      IF ( KMIN .NE. 5 ) CALL VCOPY ( CUM(KMIN), CUM(5), KMAX-KMIN+1 )
      CUM(1) = KMAX - KMIN + 1
      CUM(2) = XMIN + (KMIN-5) * DX
      CUM(3) = XMIN + (KMAX-5) * DX
      RETURN
      END
