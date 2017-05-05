CDECK  ID>, RNWRIT.
      SUBROUTINE RNWRIT ( LUN, LBL1, LBL2 )
*
*     Store seeds for random number generation on unit LUN.
*     The entry is identified by the labels LBL1 and LBL2.
*     The labels are typically run and event number but may be any
*     pair of identifiers unique enough to be used for restart
*     purposes using subroutine RNREAD.
*
*     Note that this is only an example. Some computers may require
*     OPEN statements etc. Also it is easy for the user to include
*     the common block RNCMMN and take care of this problem there.
*
*                                                    Ch.Walck 1989-12-21
*
      COMMON /RNCMMN/ LAG1, LAG2, U(97), C, CD, CM
*
      WRITE (LUN) LBL1, LBL2, LAG1, LAG2, (U(I),I=1,97), C
      RETURN
      END
