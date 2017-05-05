CDECK  ID>, RNUNIV.
      SUBROUTINE RNUNIV ( RNVEC, LEN )
*
*     Universal pseudorandom number generator by George Marsaglia & Arif
*     Zaman as described in the report FSU-SCRI-87-50, Florida State
*     University, 1987 (later on published together with a third author,
*     Wai Wan Tsang, in Stat. & Prob. Lett. 9 (1990) 35-39).
*
*     This routine returns LEN uniform pseudorandom numbers between
*     zero and one, with end-points excluded, in vector RNVEC.
*
*     Initialize by a call to RNINIT and make use of, if wanted,
*     the utilities to pack/unpack seeds or use a clock start.
*
*                                                    Ch.Walck 1989-12-21
*
      COMMON /RNCMMN/ LAG1, LAG2, U(97), C, CD, CM
      DIMENSION RNVEC(*)
*
*     Floating point arithmetics version
*
      DO 100 I = 1, LEN
    1    UNI = U(LAG1) - U(LAG2)
         IF ( UNI .LT. 0.0 ) UNI = UNI + 1.0
         U(LAG1) = UNI
         LAG1 = LAG1 - 1
         IF ( LAG1 .EQ. 0 ) LAG1 = 97
         LAG2 = LAG2 - 1
         IF ( LAG2 .EQ. 0 ) LAG2 = 97
         C = C - CD
         IF ( C .LT. 0.0 ) C = C + CM
         UNI = UNI - C
         IF ( UNI .LT. 0.0 ) UNI = UNI + 1.0
*        Avoid returning zero
         IF ( UNI .EQ. 0.0 ) GO TO 1
         RNVEC(I) = UNI
  100 CONTINUE
      RETURN
      END
