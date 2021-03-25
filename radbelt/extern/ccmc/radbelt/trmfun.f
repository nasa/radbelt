C TRMFUN.FOR	1987
C
C********************************************************************
C*************** SUBROUTINES, FUNCTIONS *****************************
C********************************************************************
C******************* TRARA1, TRARA2 *********************************
C********************************************************************
C
      SUBROUTINE TRARA1(DESCR,MAP,FL,BB0,E,F,N)                         
C***********************************************************************
C*** TRARA1 FINDS PARTICLE FLUXES FOR GIVEN ENERGIES, MAGNETIC FIELD *** 
C*** STRENGTH AND L-VALUE. FUNCTION TRARA2 IS USED TO INTERPOLATE IN ***
C*** B-L-SPACE.                                                      ***
C***   INPUT: DESCR(8)   HEADER OF SPECIFIED TRAPPED RADITION MODEL  ***
C***          MAP(...)   MAP OF TRAPPED RADITION MODEL               ***
C***                     (DESCR AND MAP ARE EXPLAINED AT THE BEGIN   ***
C***                     OF THE MAIN PROGRAM MODEL)                  ***
C***          N          NUMBER OF ENERGIES                          ***
C***          E(N)       ARRAY OF ENERGIES IN MEV                    ***
C***          FL         L-VALUE                                     ***
C***          BB0        =B/B0  MAGNETIC FIELD STRENGTH NORMALIZED   ***
C***                     TO FIELD STRENGTH AT MAGNETIC EQUATOR       ***
C***  OUTPUT: F(N)       DECADIC LOGARITHM OF INTEGRAL FLUXES IN     ***
C***                     PARTICLES/(CM*CM*SEC)                       ***
C***********************************************************************
      LOGICAL S0,S1,S2                                                  
      DIMENSION E(N),F(N),MAP(1)                               
      INTEGER DESCR(8)
      COMMON/TRA2/FISTEP
      DATA F1,F2/1.001,1.002/  
C
      FISTEP=DESCR(7)/DESCR(2)
      ESCALE=DESCR(4)
      FSCALE=DESCR(7)
      XNL=AMIN1(15.6,ABS(FL))
      NL=XNL*DESCR(5)                                 
      IF(BB0.LT.1.) BB0=1.                                               
      NB=(BB0-1.)*DESCR(6)     
C                                                                       
C I2 IS THE NUMBER OF ELEMENTS IN THE FLUX MAP FOR THE FIRST ENERGY.  
C I3 IS THE INDEX OF THE LAST ELEMENT OF THE SECOND ENERGY MAP.       
C L3 IS THE LENGTH OF THE MAP FOR THE THIRD ENERGY.                   
C E1 IS THE ENERGY OF THE FIRST ENERGY MAP (UNSCALED)                 
C E2 IS THE ENERGY OF THE SECOND ENERGY MAP (UNSCALED)                
C
      I1=0                                                              
      I2=MAP(1)                                                         
      I3=I2+MAP(I2+1)                                                   
      L3=MAP(I3+1)                                                      
      E1=MAP(I1+2)/ESCALE
      E2=MAP(I2+2)/ESCALE
C
C S0, S1, S2 ARE LOGICAL VARIABLES WHICH INDICATE WHETHER THE FLUX FOR 
C A PARTICULAR E, B, L POINT HAS ALREADY BEEN FOUND IN A PREVIOUS CALL  
C TO FUNCTION TRARA2. IF NOT, S.. =.TRUE.
C
      S1=.TRUE.                                                         
      S2=.TRUE.               
C 
C			ENERGY LOOP
C 
      DO 3 IE=1,N                                                       
C
C FOR EACH ENERGY E(I) FIND THE SUCCESSIVE ENERGIES E0,E1,E2 IN 
C MODEL MAP, WHICH OBEY  E0 < E1 < E(I) < E2 . 
C
  1   IF((E(IE).LE.E2).OR.(L3.EQ.0)) GOTO 2                                   
      	I0=I1                                                             
      	I1=I2                                                             
      	I2=I3                                                             
      	I3=I3+L3                                                          
      	L3=MAP(I3+1)                                                      
      	E0=E1                                                             
      	E1=E2                                                             
      	E2=MAP(I2+2)/ESCALE
      	S0=S1                                                             
      	S1=S2                                                             
      	S2=.TRUE.                                                         
      	F0=F1                                                             
      	F1=F2                                 
      	GOTO 1     
C
C CALL TRARA2 TO INTERPOLATE THE FLUX-MAPS FOR E1,E2 IN L-B/B0-
C SPACE TO FIND FLUXES F1,F2 [IF THEY HAVE NOT ALREADY BEEN 
C CALCULATED FOR A PREVIOUS E(I)].
C
  2   IF(S1) F1=TRARA2(MAP(I1+3),NL,NB)/FSCALE
      IF(S2) F2=TRARA2(MAP(I2+3),NL,NB)/FSCALE
      S1=.FALSE.                                                        
      S2=.FALSE.                                                        
C
C FINALLY, INTERPOLATE IN ENERGY.
C
      F(IE)=F1+(F2-F1)*(E(IE)-E1)/(E2-E1)                               
      IF(F2.GT.0.0) GOTO 3
      IF(I1.EQ.0)   GOTO 3
C                                                                       
C --------- SPECIAL INTERPOLATION ---------------------------------
C IF THE FLUX FOR THE SECOND ENERGY CANNOT BE FOUND (I.E. F2=0.0),
C AND THE ZEROTH ENERGY MAP HAS BEEN DEFINED (I.E. I1 NOT EQUAL 0), 
C THEN INTERPOLATE USING THE FLUX MAPS FOR THE ZEROTH AND FIRST 
C ENERGY AND CHOOSE THE MINIMUM OF THIS INTERPOLATIONS AND THE
C INTERPOLATION THAT WAS DONE WITH F2=0. 
C                                                                       
      IF(S0) F0=TRARA2(MAP(I0+3),NL,NB)/FSCALE
      S0=.FALSE.                                                        
      F(IE)=AMIN1(F(IE),F0+(F1-F0)*(E(IE)-E0)/(E1-E0))                  
C
C THE LOGARITHMIC FLUX IS ALWAYS KEPT GREATER OR EQUAL ZERO.
C
  3   F(IE)=AMAX1(F(IE),0.)                                             
      RETURN                                                            
      END                                                               
C
      FUNCTION TRARA2(MAP,IL,IB)                                        
C*****************************************************************
C***  TRARA2 INTERPOLATES LINEARLY IN L-B/B0-MAP TO OBTAIN     ***
C***  THE LOGARITHM OF INTEGRAL FLUX AT GIVEN L AND B/B0.      ***
C***    INPUT: MAP(..) IS SUB-MAP (FOR SPECIFIC ENERGY) OF     ***
C***                   TRAPPED RADIATION MODEL MAP             ***
C***           IL      SCALED L-VALUE                          ***
C***           IB      SCALED B/B0-1                           ***
C***   OUTPUT: TRARA2  SCALED LOGARITHM OF PARTICLE FLUX       ***
C*****************************************************************
C***  SEE MAIN PROGRAM 'MODEL' FOR EXPLANATION OF MAP FORMAT   ***
C***  SCALING FACTORS.                                         ***
C***  THE STEPSIZE FOR THE PARAMETERIZATION OF THE LOGARITHM   ***
C***  OF FLUX IS OBTAINED FROM 'COMMON/TRA2/'.                 ***
C*****************************************************************
      DIMENSION MAP(1)
      COMMON/TRA2/FISTEP
      FNL=IL                                                            
      FNB=IB                                                            
      ITIME=0                                                           
      I2=0                          
C
C FIND CONSECUTIVE SUB-SUB-MAPS FOR SCALED L-VALUES LS1,LS2, 
C WITH IL LESS OR EQUAL LS2.  L1,L2 ARE LENGTHS OF SUB-SUB-MAPS. 
C I1,I2 ARE INDECES OF FIRST ELEMENTS MINUS 1.
C
  1   L2=MAP(I2+1)  
      IF(MAP(I2+2).GT.IL) GOTO 2  
	I1=I2                                                             
    	L1=L2
      	I2=I2+L2
      	GOTO 1                                                           
  2   CONTINUE    
C  
C IF SUB-SUB-MAPS ARE EMPTY, I. E. LENGTH LESS 4, THAN TRARA2=0
C       
      IF((L1.LT.4).AND.(L2.LT.4)) GOTO 50                                   
C
C IF FLOG2 LESS FLOG1, THAN LS2 FIRST MAP AND LS1 SECOND MAP
C
      IF(MAP(I2+3).GT.MAP(I1+3))  GOTO 10
    5 	KT=I1                                                             
      	I1=I2                                                             
      	I2=KT                                                             
      	KT=L1                                                             
      	L1=L2                                                             
      	L2=KT                                                             
C
C DETERMINE INTERPOLATE IN SCALED L-VALUE
C 
  10  FLL1=MAP(I1+2)
      FLL2=MAP(I2+2)
      DFL=(FNL-FLL1)/(FLL2-FLL1)                                        
      FLOG1=MAP(I1+3)                                                   
      FLOG2=MAP(I2+3)                                                   
      FKB1=0.                                                           
      FKB2=0.                                                           
      IF(L1.LT.4) GOTO 32
C
C B/B0 LOOP
C                                               
      DO 17 J2=4,L2                                                     
 	FINCR2=MAP(I2+J2)                                                 
      	IF(FKB2+FINCR2.GT.FNB) GOTO 23                                    
      	FKB2=FKB2+FINCR2                                                  
   17 FLOG2=FLOG2-FISTEP                                                
      ITIME=ITIME+1                                                     
      IF(ITIME.EQ.1)GO TO 5                                             
      GO TO 50                                                          
   23 IF(ITIME.EQ.1)GO TO 30                                            
      IF(J2.EQ.4)GO TO 28                                               
      SL2=FLOG2/FKB2                                                    
      DO 27 J1=4,L1                                                     
      	FINCR1=MAP(I1+J1)                                                 
      	FKB1=FKB1+FINCR1                                                  
      	FLOG1=FLOG1-FISTEP                                                
      	FKBJ1=((FLOG1/FISTEP)*FINCR1+FKB1)/((FINCR1/FISTEP)*SL2+1.)       
      	IF(FKBJ1.LE.FKB1) GOTO 31                                         
   27 CONTINUE                                                          
      IF(FKBJ1.LE.FKB2) GOTO 50                                        
   31 IF(FKBJ1.LE.FKB2) GOTO 29                                         
      FKB1=0.                                                           
   30 FKB2=0.                                                           
   32 J2=4                                                              
      FINCR2=MAP(I2+J2)                                                 
      FLOG2=MAP(I2+3)                                                   
      FLOG1=MAP(I1+3)                                                   
   28 FLOGM=FLOG1+(FLOG2-FLOG1)*DFL                                     
      FKBM=0.                                                           
      FKB2=FKB2+FINCR2                                                  
      FLOG2=FLOG2-FISTEP                                                
      SL2=FLOG2/FKB2                                                    
      IF(L1.LT.4) GOTO 35                                               
      J1=4                                                              
      FINCR1=MAP(I1+J1)                                                 
      FKB1=FKB1+FINCR1                                                  
      FLOG1=FLOG1-FISTEP                                                
      SL1=FLOG1/FKB1                                                    
      GOTO 15                                                          
   29 FKBM=FKBJ1+(FKB2-FKBJ1)*DFL                                       
      FLOGM=FKBM*SL2                                                    
      FLOG2=FLOG2-FISTEP                                                
      FKB2=FKB2+FINCR2                                                  
      SL1=FLOG1/FKB1                                                    
      SL2=FLOG2/FKB2                                                    
   15 IF(SL1.LT.SL2) GOTO 20                                            
      	FKBJ2=((FLOG2/FISTEP)*FINCR2+FKB2)/((FINCR2/FISTEP)*SL1+1.)       
      	FKB=FKB1+(FKBJ2-FKB1)*DFL                                         
      	FLOG=FKB*SL1                                                      
      	IF(FKB.GE.FNB) GOTO 60                                            
      	FKBM=FKB                                                          
      	FLOGM=FLOG                                                        
      	IF(J1.GE.L1) GOTO 50                                            
      	J1=J1+1                                                           
      	FINCR1=MAP(I1+J1)                                                 
      	FLOG1=FLOG1-FISTEP                                                
      	FKB1=FKB1+FINCR1                                                  
      	SL1=FLOG1/FKB1                                                    
      	GOTO 15                                                          
   20 FKBJ1=((FLOG1/FISTEP)*FINCR1+FKB1)/((FINCR1/FISTEP)*SL2+1.)       
      FKB=FKBJ1+(FKB2-FKBJ1)*DFL                                        
      FLOG=FKB*SL2                                                      
      IF(FKB.GE.FNB) GOTO 60                                            
      	FKBM=FKB                                                          
      	FLOGM=FLOG                                                        
      	IF(J2.GE.L2) GOTO 50                                             
      	J2=J2+1                                                           
      	FINCR2=MAP(I2+J2)                                                 
      	FLOG2=FLOG2-FISTEP                                                
      	FKB2=FKB2+FINCR2                                                  
      	SL2=FLOG2/FKB2                                                    
      	GOTO 15  
   35 	FINCR1=0.                                                         
      	SL1=-900000.                                                      
      	GOTO 20                                                          
   60 IF(FKB.LT.FKBM+1.E-10) GOTO 50                                    
      TRARA2=FLOGM+(FLOG-FLOGM)*((FNB-FKBM)/(FKB-FKBM))                 
      TRARA2=AMAX1(TRARA2,0.)                                           
      RETURN                                                            
   50 TRARA2=0.                                                         
      RETURN                                                            
      END
