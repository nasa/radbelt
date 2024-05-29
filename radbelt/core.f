C
C Copyright (C) 2021 United States Government as represented by 
C the Administrator of the National Aeronautics and Space Administration.
C No copyright is claimed in the United States under Title 17, U.S. Code.
C All Other Rights Reserved.
C
C SPDX-License-Identifier: NASA-1.3
C

C Adapted from
C https://ccmc.gsfc.nasa.gov/pub/modelweb/geomagnetic/igrf/fortran_code/bilcal.for

        SUBROUTINE IGRF(LON, LAT, HEIGHT, YEAR, XL, BBX)
            REAL        LON, LAT, HEIGHT, YEAR, XL, BBX
            LOGICAL     VAL
            REAL        DIMO, BNORTH, BEAST, BDOWN, BABS, BEQ, BDEL, RR0
            INTEGER     ICODE

            ! Initialize the IGRF model
            CALL INITIZE

            ! Get field coefficients for the given year
            CALL FELDCOF(YEAR, DIMO)

            ! Compute the geomagnetic field components
            CALL FELDG(LAT, LON, HEIGHT, BNORTH, BEAST, BDOWN, BABS)

            ! Calculate the L-shell parameter
            CALL SHELLG(LAT, LON, HEIGHT, DIMO, XL, ICODE, BAB1)

            ! Compute the magnetic field equatorial value
            BEQ = DIMO / (XL * XL * XL)

            ! Adjust BEQ if ICODE equals 1
            IF (ICODE .EQ. 1) THEN
                BDEL = 1.E-3
                CALL FINDB0(0.05, BDEL, VAL, BEQ, RR0)
                IF (VAL) BEQ = BEQ
            END IF

            ! Calculate the ratio of BABS to BEQ
            BBX = BABS / BEQ
        END SUBROUTINE IGRF

C Adapted from
C https://ccmc.gsfc.nasa.gov/pub/modelweb/radiation_belt/radbelt/fortran_code/radbelt.for

        SUBROUTINE AEP8(E, L, BB0, IMNAME, FLUX)
            REAL        E, L, BB0, FLUX
            INTEGER     IERR, NMAP, IUAEAP
            INTEGER     MAP(20000), IHEAD(8)
            CHARACTER*10 NAME, MNAME(4)
            REAL        EE(1)
            DATA MNAME  /'ae8min.asc', 'ae8max.asc', 'ap8min.asc', 'ap8max.asc'/

            ! Map the IMNAME to the corresponding file name
            IUAEAP = 15
            NAME = MNAME(IMNAME)

            ! Open the file containing the AE-8/AP-8 radiation belt data
            OPEN(IUAEAP, FILE=NAME, STATUS='OLD', IOSTAT=IERR, ERR=1822, FORM='FORMATTED')
            READ(IUAEAP, 1301) IHEAD

            ! Get the number of map points from the file header
            NMAP = IHEAD(8)
            READ(IUAEAP, 1301) (MAP(I), I=1, NMAP)

1301        FORMAT(1X, 12I6)

1822        CLOSE(IUAEAP)
            IF (IERR .NE. 0) STOP

            ! Set the energy level for flux calculation
            EE(1) = E

            ! Compute the flux using the TRARA1 subroutine
            CALL TRARA1(IHEAD, MAP, L, BB0, E, FLUX, 1)
            IF (FLUX .GT. 0.0) FLUX = 10.**FLUX
        END SUBROUTINE AEP8
