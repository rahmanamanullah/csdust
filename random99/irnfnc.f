CDECK  ID>, IRNFNC.
      FUNCTION IRNFNC ( CUM )
C-----------------------------------------------------------------------
C
C...       FUNCTION IRNFNC ( CUM )                          Ch.Walck    860807
C.
C.    Give random number for any distribution in a discrete variable
C.    according to histogram/vector or function (initialized by call
C.    to subroutine IRNHIS or IRNPRE).
C.
C.----------------------------------------------------------------------
      DIMENSION CUM(*)
      IMIN = 1
      IMAX = CUM(1)
      R = RNUNIF(DUM) * CUM(IMAX+4)
C     Binary search
  100 M = ( IMIN + IMAX ) / 2
      IF ( R .GT. CUM(M+4) ) THEN
         IMIN = M
      ELSE
         IMAX = M
      END IF
      IF ( IMAX-IMIN .GT. 1 ) GO TO 100
C
      IRNFNC = IFIX(CUM(2)) + IMIN - 1
      RETURN
      END
