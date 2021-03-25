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
