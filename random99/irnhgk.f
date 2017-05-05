CDECK  ID>, IRNHGK.
      SUBROUTINE IRNHGK ( NPICK, NTOT, NTYPE, NGROUP, IR )
C-----------------------------------------------------------------------
C
C...       SUBROUTINE IRNHGK (NPICK,NTOT,NTYPE,NGROUP,IR)   Ch.Walck    860804
C.
C.    Generate a pseudorandom number from a generalized hypergeometric
C.    distribution. Note the limitation for the number of NGROUP put to
C.    100 by default in the parameter statement below. If this routine
C.    is needed for a larger number of groups change this accordingly.
C.
C.    NPICK elements are to be picked without replacement out of NTOT
C.    elements which contains NGROUP groups with NTYPE(I) elements each
C.    (sum NTYPE(I)=NTOT). The generalized hypergeometric distribution
C.    gives the probability that exactly IR(I) (I=1-NGROUP) elements
C.    of type I are picked (cf also function IRNHGM below).
C.
C.                             NGROUP   NTYPE(I)      NTOT
C.    P(IR;NTOT,NPICK,NTYPE) = PRODUCT (        )  / (     )
C.                              I = 1     IR(I)       NPICK
C.
C.----------------------------------------------------------------------
      PARAMETER (MAXGRP=100)
      DIMENSION IR(NGROUP), MTYPE(MAXGRP), NTYPE(NGROUP)
C
C        Initialize counters:
C        IR(I)    = No of picked elements of each type
C        MTYPE(I) = No of remaining elements of type I
C        MTOT     = No of remaining elements totally
C        MPICK    = No of picked elements
C
      DO 1 I = 1, NGROUP
         IR(I) = 0
         MTYPE(I) = NTYPE(I)
    1 CONTINUE
      MTOT  = NTOT
      MPICK = 0
C
C        Select one of the remaining elements
C
    2 K = IRNUNI ( 1, MTOT )
C
C        Find out which group this element is from (I), increment
C        IR(I) and MPICK and decrement MTYPE(I) and MTOT.
C
      NCUM = 0
      DO 3 I = 1, NGROUP
         NCUM = NCUM + MTYPE(I)
         IF ( K .LE. NCUM ) THEN
            IR(I)    = IR(I)    + 1
            MTYPE(I) = MTYPE(I) - 1
            MTOT     = MTOT     - 1
            MPICK    = MPICK    + 1
C
C              Finish if NPICK elements picked
C
            IF ( MPICK .EQ. NPICK ) GO TO 4
            GO TO 2
         END IF
    3 CONTINUE
    4 RETURN
      END
