C
C Copyright (C) 2021 United States Government as represented by the Administrator
C of the National Aeronautics and Space Administration. No copyright is claimed
C in the United States under Title 17, U.S. Code. All Other Rights Reserved.
C
C SPDX-License-Identifier: NASA-1.3
C

C Adapted from
C https://ccmc.gsfc.nasa.gov/pub/modelweb/geomagnetic/igrf/fortran_code/bilcal.for

        SUBROUTINE IGRF(LON, LAT, HEIGHT, YEAR, XL, BBX)
            REAL        LON, LAT, HEIGHT, YEAR, XL, BBX
            LOGICAL     VAL

            CALL INITIZE
            CALL FELDCOF(YEAR, DIMO)
            CALL FELDG(LAT, LON, HEIGHT, BNORTH, BEAST, BDOWN, BABS)
            CALL SHELLG(LAT, LON, HEIGHT, DIMO, XL, ICODE, BAB1)
            BEQU = DIMO / (XL * XL * XL)
            IF(ICODE.EQ.1) THEN
                BDEL = 1.E-3
                CALL FINDB0(0.05, BDEL, VAL, BEQ, RR0)
                IF(VAL) BEQU = BEQ
            ENDIF
            BBX = BABS / BEQU
        END

C Adapted from
C https://ccmc.gsfc.nasa.gov/pub/modelweb/radiation_belt/radbelt/fortran_code/radbelt.for

        SUBROUTINE AEP8(E, L, BB0, IMNAME, FLUX)
            REAL        L, BB0
            DIMENSION   MAP(20000), IHEAD(8), EE(1)
            CHARACTER*10  	NAME, MNAME(4)
            DATA MNAME  /'ae8min.asc','ae8max.asc','ap8min.asc',
     &                   'ap8max.asc'/

            IUAEAP = 15
            NAME = MNAME(IMNAME)

            OPEN(IUAEAP,FILE=NAME,STATUS='OLD',IOSTAT=IERR,ERR=1822,
     &           FORM='FORMATTED')
            READ(IUAEAP,1301) IHEAD
            NMAP=IHEAD(8)
            READ(IUAEAP,1301) (MAP(I),I=1,NMAP)
1301	    FORMAT(1X,12I6)
        
1822        CLOSE(IUAEAP)
            IF (IER .NE. 0) STOP

            EE(1) = E
            CALL TRARA1(IHEAD,MAP,L,BB0,E,FLUX,1)
            IF(FLUX.GT.0.0) FLUX=10.**FLUX
        END
