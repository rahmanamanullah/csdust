CDECK  ID>, RNCLCK.
      SUBROUTINE RNCLCK ( ISEED )
*
*            Clock start of random number generator
*
*     Calculate number of seconds from 1 January 1989 modulus 921350144
*     which may be used as start seed for the sequence. This implies
*     that one avoids using the same start sequence in any simulation
*     unless two jobs are ran in parallel during the same second!
*     The code previously used the CERN library routine DATIMH to
*     obtain the system date and time. This, however, is a unnecessary
*     detour in achieving this. A new subroutine RNDATE has been
*     supplied which directly uses system functions for date and time
*     on most computers.
*
*     Working in several laboratories one may, as has been done for
*     DELPHI, construct a value from a lab-code and the number of
*     seconds past during the current year.
*
*                                                    Ch.Walck 1989-12-21
*                                                     revised 1990-09-22
*                                                     bug fix 1994-09-05
*
*                                                    Ra.Amanullah
*                                                     revised 2010-08-19
      SAVE IMD
      DIMENSION IMD(12)
      DATA IMD / 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 /
*
*     Get system date and time
*
      CALL RNDATE ( JY, JM, JD, IH, IM, IS )
*
*     Calculate number of days in previous years from 1 January 1989
*
      IY = 1900 + JY
      IF ( JY .LT. 89 ) IY = 2000 + JY
      IDY = 0
      IF ( IY .GT. 1989 ) THEN
         DO 100 I = 1989, IY-1
            JDY = 365
*           Code valid until 2100 which is not a leap-year (2000 is! 940905)
            IF ( MOD(I,4).EQ.0 ) JDY = 366
            IDY = IDY + JDY
  100    CONTINUE
      END IF
*
*     Number of days from 1 January until 1st current month
*     (treat leap-years as well)
*
      LY = 0
*     Code valid until 2100 which is not a leap-year (2000 is! 940905)
      IF ( MOD(JY,4).EQ.0 ) LY = 1
      ISHIFT = IMD(JM)
      IF ( JM .GT. 2 ) ISHIFT = IMD(JM) + LY
*
*     Seconds since January 1st 1989 at 00:00:00 (=0)
*
      ISEED = IS + 60 * ( IM + 60 * ( IH + 24 * ( JD-1+ISHIFT+IDY ) ) )
*
*     Bring packed seed into range (this modulus will zero the ISEED at
*     18:35:44 on March 13 2018 and at 13:11:28 on May 24 2047, ... )
*
      ISEED = MOD(ISEED,921350144)

*
*     Not sure how it happens but sometimes ISEED is negative.  We just use
*     the absolute value. / Rahman 2010-08-19
*
      IF ( ISEED .LT. 0. ) THEN
        ISEED = -ISEED
      ENDIF
*
      WRITE (6,1003) JD, JM, JY, IH, IM, IS, ISEED
 1003 FORMAT (/' RNCLCK: date and time is',I3,'-',I2.2,'-',I4,I3.2,':'
     +       ,I2.2,':',I2.2,', clock start at seed',I10/)
      RETURN
      END
