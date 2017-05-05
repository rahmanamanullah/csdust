CDECK  ID>, RNFNC.
      FUNCTION RNFNC ( CUM )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNFNC ( CUM )                           Ch.Walck    1980/81
C.
C.    Give random number according to any histogram or function
C.    (initialization by call to subroutine RNHIS or RNPRE).
C.    This routine uses a sequential search technique and is
C.    now surpassed by subroutine RNFUNC which using a binary
C.    search technique is much faster (unless very short vector).
C.
C.    External references: RNUNIF
C.
C.----------------------------------------------------------------------
      DIMENSION CUM(*)
      NCUM = CUM(1)
      R = RNUNIF(DUM) * CUM(NCUM+4)
      DO 103 I = 2 , NCUM
  103 IF ( R .LE. CUM(I+4) ) GO TO 104
      RNFNC = CUM(3)
      RETURN
  104 RNFNC = CUM(2) + CUM(4) * ((I-2)+(R-CUM(I+3))/(CUM(I+4)-CUM(I+3)))
      RETURN
      END
