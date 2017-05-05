CDECK  ID>, IRNPRE.
      SUBROUTINE IRNPRE ( FUNC, NCUM, IMIN, CUM )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE IRNPRE ( FUNC, NCUM, IMIN, CUM )      Ch.Walck    860807
C.
C.    Initialize random number generation for a distribution in a
C.    discrete variable known in the form of a function
C.    (random numbers accessed by function IRNFNC)
C.
C.    FUNC - Function according to which random numbers are to be
C.           generated (should be declared external in calling routine)
C.    NCUM - No of elements in reference vector
C.    IMIN - Minimum x value
C.    CUM  - Reference vector of dimension NCUM
C.           (1) = No of elements in cumulative vector
C.           (2) = Lower edge for cumulative vector = IMIN
C.           (3) = Upper edge for cumulative vector = IMIN+N-1
C.           (4) = Bin width = 1
C.           (5) - (NCUM) = Cumulative vector
C.
C.    External references: FUNC  (user supplied)
C.                         VCOPY (patch RNUTIL)
C.
C.----------------------------------------------------------------------
      DIMENSION CUM(*)
C
C     Fill cumulative vector with 4 status words at start
C
      N      = NCUM - 4
      CUM(1) = FLOAT(N)    + 0.1
      CUM(2) = FLOAT(IMIN) + 0.1
      IF ( IMIN .LT. 0 ) CUM(2) = FLOAT(IMIN) - 0.1
      CUM(3) = FLOAT(IMIN+N-1) + 0.1
      CUM(4) = 1.1
      CUM(5) = 0.0
      DO 101 I = 2 , N
         CUM(I+4) = FUNC(I-2+IMIN) + CUM(I+3)
  101 CONTINUE
C
C     Repack if zeros at start or max at end of cumulative vector
C
      DO 102 I = 2 , N
  102 IF ( CUM(I+4) .NE. 0.0 ) GO TO 103
  103 KMIN = I + 3
      DO 104 I = 1 , N
  104 IF ( CUM(N-I+5) .NE. CUM(N-I+4) ) GO TO 105
  105 KMAX = N - I + 5
      IF ( KMIN .EQ. 5 .AND. KMAX .EQ. N+4 ) RETURN
      IF ( KMIN .NE. 5 ) CALL VCOPY(CUM(KMIN),CUM(5),KMAX-KMIN+1)
      CUM(1) = FLOAT(KMAX-KMIN+1) + 0.1
      CUM(2) = FLOAT(IMIN+KMIN-5) + 0.1
      CUM(3) = FLOAT(IMIN+KMAX-6) + 0.1
      RETURN
      END
