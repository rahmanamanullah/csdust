CDECK  ID>, DCCLCK.
      SUBROUTINE DCCLCK ( ISEED )
*
*     Decode seed packed by RNCLCK to give a time when the run was
*     initialized by this routine. Normally not needed but sometimes
*     nice to find date when a run whose start clock seed has been
*     stored was made. Gives answer in the interval from 1989-01-01
*     at 00:00:00 (ISEED=0) to 2018-03-13 18:35:43 (ISEED=921350143).
*     (Provisional routine)
*                                                    Ch.Walck 1994-09-05
*
      SAVE IMD
      DIMENSION IMD(12)
      DATA IMD / 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
     +         , 365 /
*
      IS  = MOD ( ISEED,      60 )
      IM  = MOD ( ISEED/60,   60 )
      IH  = MOD ( ISEED/3600, 24 )
      JD  = ISEED/3600/24
      JY  = 1989
      JDY = 365
    1 IF ( JD .GE. JDY ) THEN
         JY  = JY + 1
         JD  = JD - JDY
         JDY = 365
         IF ( MOD(JY,4) .EQ. 0 ) JDY = 366
         GO TO 1
      END IF
      JD = JD + 1
      LY = 0
      IF ( MOD(JY,4) .EQ. 0 ) LY = 1
      DO I = 1, 12 
         ISHIFT = IMD(I)
         IF ( I .GT. 1 ) ISHIFT = IMD(I) + LY
         IF ( JD .LE. ISHIFT ) THEN
            JM = I
            IF ( I .GT. 1 ) JD = JD - JSHIFT
            GO TO 2
         END IF
         JSHIFT = ISHIFT
      END DO
*
*     Store also/instead time in character variable of length 19 !?
*
    2 PRINT 1000, ISEED, JY, JM, JD, IH, IM, IS
 1000 FORMAT (' Seed ',I10,' => ',I4,'-',I2.2,'-',I2.2,1x,i2.2,':',i2.2
     +       ,':',i2.2)
      RETURN
      END
