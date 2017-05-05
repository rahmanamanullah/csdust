CDECK  ID>, RNHIS.
      SUBROUTINE RNHIS  ( HIS, NBIN, XMIN, DX, CUM )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE RNHIS ( HIS, NBIN, XMIN, DX, CUM )    Ch.Walck    1980/81
C.
C.    Initialize random number generation according to histogram
C.    (random numbers accessed by function RNFUNC or RNFNC)
C.
C.    HIS  - Histogram vector of dimension NBIN
C.    NBIN - No of bins in histogram
C.    XMIN - Lower edge of histogram
C.    DX   - Bin width
C.    CUM  - Reference vector of dimension NBIN+5
C.           (may be identical to HIS vector)
C.           (1) = No of elements in cumulative vector
C.           (2) = Lower edge for cumulative vector
C.           (3) = Upper edge for cumulative vector
C.           (4) = Bin width
C.           (5) - (NBIN+5) = Cumulative vector
C.
C.    External references: VCOPY (patch RNUTIL)
C.
C.----------------------------------------------------------------------
      DIMENSION HIS(*), CUM(*)
      CALL VCOPY ( HIS, CUM(6), NBIN )
      N = NBIN + 1
      CUM(1) = N
      CUM(2) = XMIN
      CUM(3) = XMIN + NBIN * DX
      CUM(4) = DX
      CUM(5) = 0.0
      DO 101 I = 2, N
  101 CUM(I+4) = CUM(I+4) + CUM(I+3)
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
