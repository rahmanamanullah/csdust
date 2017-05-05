CDECK  ID>, RNREAD.
      SUBROUTINE RNREAD ( LUN, LBL1, LBL2 )
*
*     Read start values for random number generator from unit LUN.
*     The event searched for has labels LBL1 and LBL2. The file
*     has normally been created by successive calls to RNWRIT.
*     If requested entry not found the run is aborted.
*
*     Note that this is only an example. Some computers may require
*     OPEN statements etc. Also it is easy for the user to include
*     the common block RNCMMN and take care of this problem there.
*
*                                                    Ch.Walck 1989-12-21
*
      COMMON /RNCMMN/ LAG1, LAG2, U(97), C, CD, CM
*
      CD =  7654321.0 / 16777216.0
      CM = 16777213.0 / 16777216.0
    1 READ(LUN,END=999) LBLA, LBLB, LAG1, LAG2, (U(I),I=1,97), C
      IF ( LBLA.NE.LBL1 .OR. LBLB.NE.LBL2 ) GO TO 1
      RETURN
  999 WRITE (6,1000)
 1000 FORMAT (//' RNREAD: error! Start values for event not found!'//)
      STOP
      END
