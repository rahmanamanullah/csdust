CDECK  ID>, IRNHIS.
      SUBROUTINE IRNHIS  ( HIS, NBIN, IMIN, CUM )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE IRNHIS ( HIS, NBIN, IMIN, CUM )       Ch.Walck    860807
C.
C.    Initialize random number generation for a distribution in a
C.    discrete variable known in the form of a histogram/vector
C.    (random numbers accessed by function IRNFNC)
C.
C.    HIS  - Histogram vector of dimension NBIN
C.    NBIN - No of bins in histogram
C.    IMIN - Lower edge of histogram (bin width assumed to be 1)
C.    CUM  - Reference vector of dimension NBIN+5
C.           (may be identical to HIS vector)
C.           (1) = No of elements in cumulative vector
C.           (2) = Lower edge for cumulative vector
C.           (3) = Upper edge for cumulative vector
C.           (4) = Bin width = 1
C.           (5) - (NBIN+5) = Cumulative vector
C.
C.    External references: VCOPY (patch RNUTIL)
C.
C.----------------------------------------------------------------------
      DIMENSION HIS(*), CUM(*)
      CALL VCOPY ( HIS, CUM(6), NBIN )
      N = NBIN + 1
C
C     Fill cumulative vector (with 4 status words at start)
C
C     0.1 added to assure correct integer when converted back
      CUM(1) = FLOAT(N)    + 0.1
      CUM(2) = FLOAT(IMIN) + 0.1
      IF ( IMIN .LT. 0 ) CUM(2) = FLOAT(IMIN) - 0.1
      CUM(3) = FLOAT(IMIN+NBIN-1) + 0.1
      CUM(4) = 1.1
      CUM(5) = 0.0
      DO 101 I = 2 , N
  101 CUM(I+4) = CUM(I+4) + CUM(I+3)
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
