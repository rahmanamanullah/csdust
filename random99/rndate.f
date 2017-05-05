CDECK  ID>, RNDATE.
      SUBROUTINE RNDATE ( JY, JM, JD, IH, IM, IS )
*
*     Routine to give current date and time. This routine replaces the
*     old DATIMH routine from the CERN library which was an unnecessary
*     detour in achieving this information. The routine is not fully
*     tested on all computers but hopefully works anyway. If any problem
*     is encountered the self-tag DATIMH activates the old code.
*
*          JY - Year (modulus 100)        IH - Hours
*          JM - Month                     IM - minutes
*          JD - Date                      IS - seconds
*
*                                                    Ch.Walck 1990-09-22
*
*     System calls on MacIntosh
*
      CHARACTER*10 CTIME
      INTEGER TARRAY(3)
*     CALL TIME  ( CTIME )
*     CALL IDATE ( JM, JD, JY )
      CALL DATE_AND_TIME(TIME=CTIME)
      CALL IDATE ( TARRAY )
      JM = TARRAY(1)
      JD = TARRAY(2)
      JY = TARRAY(3)
      READ (CTIME,1001) IH, IM, IS
 1001 FORMAT (I2,I2,I2)
* 1001 FORMAT (I2,1X,I2,1X,I2)
      RETURN
      END
