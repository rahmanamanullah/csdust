CDECK  ID>, RNUNPK.
      SUBROUTINE RNUNPK ( ISEED, I, J, K, L )
*
*     Unpack the four initial seeds I, J, K and L from the packed
*     word ISEED.
*                                                    Ch.Walck 1989-12-21
*
      IF ( ISEED.LE.0 .OR. ISEED.GT.921350143 ) GO TO 999
      I = MOD(ISEED/169/176/176,176) + 2
      J = MOD(ISEED/169/176,176) + 2
      K = MOD(ISEED/169,176) + 2
      L = MOD(ISEED,169)
      RETURN
*
*     Abort run if ISEED value is outside valid range
*
  999 WRITE (6,1000) I, J, K, L
 1000 FORMAT (//' RNUNPK: error - illegal start seed',4I4//)
      STOP
      END
