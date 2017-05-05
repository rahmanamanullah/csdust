CDECK  ID>, RNFUNC.
      FUNCTION RNFUNC ( CUM )
C-----------------------------------------------------------------------
C
C...       FUNCTION RNFUNC ( CUM )                          Ch.Walck    841105
C.                                                          simplified  860812
C.
C.    Give random number according to any histogram or function
C.    (initialization by call to subroutine RNHIS or RNPRE).
C.    This routine is a faster version of RNFNC using a binary
C.    search technique.
C.
C.----------------------------------------------------------------------
      DIMENSION CUM(*)
      IMIN = 1
      IMAX = CUM(1)
      R = RNUNIF(DUM) * CUM(IMAX+4)
  100 M = ( IMIN + IMAX ) / 2
      IF ( R .GT. CUM(M+4) ) THEN
         IMIN = M
      ELSE
         IMAX = M
      END IF
      IF ( IMAX-IMIN .GT. 1 ) GO TO 100
C
      RNFUNC = ( R - CUM(IMIN+4) ) / ( CUM(IMAX+4) - CUM(IMIN+4) )
     +         * CUM(4) + FLOAT(IMIN-1) * CUM(4) + CUM(2)
      RETURN
      END
