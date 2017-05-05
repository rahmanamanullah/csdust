CDECK  ID>, RNPACK.
      SUBROUTINE RNPACK ( I, J, K, L, ISEED )
*
*     Pack the four initial seeds I, J, K and L into one integer ISEED.
*     The allowed values for I, J and K are between 2 and 177 and for
*     L between 0 and 168.
*                                                    Ch.Walck 1989-12-21
*                                             minor bug fixed 1991-01-14
*
      IF ( I.LE.1 .OR. I.GE.178 ) GO TO 999
      IF ( J.LE.1 .OR. J.GE.178 ) GO TO 999
      IF ( K.LE.1 .OR. K.GE.178 ) GO TO 999
      IF ( L.LT.0 .OR. L.GE.169 ) GO TO 999
*
*     Pack seeds densely into ISEED using bases 176, 176, 176 and 169
*     for the four seeds, respectively.
*
      ISEED = 169 * ( 176 * ( 176*(I-2) + (J-2) ) + (K-2) ) + L
      RETURN
*
*     Abort run if illegal values are used in the call
*
  999 WRITE (6,1000) I, J, K, L
 1000 FORMAT (//' RNPACK: error - illegal start seeds',4I12//)
      STOP
      END
