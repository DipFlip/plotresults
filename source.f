C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C                    S   T   A   N   T   O   N       1                  
C                                                                       
C  MONTE CARLO NEUTRON COUNTER EFFICIENCY PROGRAM                       
C  MAIN ROUTINE                                                         
C     IN THIS ROUTINE WE READ IN ALL INPUT DATA NECESSARY TO            
C     CALCULATE A NEUTRON EFFICIENCY FOR A PARTICULAR                   
C     SCINTILLATOR GEOMETRY                                             
C                                                                       
C     WE THEN CALL FOLNUT TO PROPAGATE A NEUTRON THROUGH THE            
C     SCINTILLATOR AND PRODUCE THE LIGHT DEPOSITED BY ANY CHARGED       
C     PARTICLES PRODUCED IN THE PROPAGATION PROCESS                     
C                                                                       
C     THE MAIN ROUTINE THEN BINS THE LIGHT OUTPUT OF FOLNUT AS          
C     REQUIRED BY INPUT PARAMETERS PREVIOUSLY READ                      
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C     THIS IS THE KENT STATE UNIVERSITY VERSION OF STANTON              
C                                    (REF.  NIM 161 (1979) PP.439-447)  
C                                                                       
C.                                                                      
C.    ARE STATEMENTS FROM THE OLD VERSION WHICH WERE REMOVED THIS WAY   
C.                                                                      
C                                                          P.G.79/08/08 
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C                                                                       
      DIMENSION X(3),C(3),EL(9),S(8),XI(3),CI(3),COMMNT(20)             
      DIMENSION LT(400,9),LCON(9),LHI(9),ELOPE(3),EFF(400)              
      COMMON /SCINP/ PA,PB,PC,PR,PS,PT                                  
      COMMON /SCINA/ AG,AH,AO,AP,AQ,AU                                  
      COMMON /CRBANG/ ED(32),AD(32,8)                                   
      COMMON /SIGDAT/ EDAT(128),SDAT(128,7)                             
      COMMON /SWBLK/ NSW(6),NDUMP,MDUMP                                 
      COMMON /FIRSC/ NFS(6,5,2)                                         
      COMMON /SCIN/ DHYD,DCARB,IGEO,XB,YB,ZB                            
      COMMON /TIMING/ TBIAS,DTIM,DPOS,NTIM(100),NPOS(100)               
      COMMON /RANGEN/ KSEED          
      character infile*30,outfile*30,eventfile*30
c
777	format(a)
	write(6,*)' Enter input file'
	read(5,777)infile
	open(51,file=infile,status='old')
	write(6,*)' Enter output file'
	read(5,777)outfile
	open(61,file=outfile,status='new')
C  INPUT SENSE SWITCH SETTINGS, AND MAXIMUM NUMBER OF EVENTS TO DUMP    
C   NSW(I) , THE 'SENSE SWITCHES' CONTROL HOW MUCH OUTPUT IS DUMPED     
      WRITE (61,54)                                                      
   54 FORMAT(10H1DATA DECK     )                                        
      READ(51,55) COMMNT                                                 
      WRITE (61,56) COMMNT                                               
   55 FORMAT (20A4)                                                     
   56 FORMAT (1H0/1X,20A4)                                              
      READ(51,9001)NSW,MDUMP                                             
      WRITE(61,9002) NSW,MDUMP
	if(mdump.gt.0)then
	  write(6,*)' Enter event file'
	  read(5,777)eventfile
	  open(11,file=eventfile,form='unformatted',status='new')
        endif
 9001 FORMAT (6I1,I4)                                                   
 9002 FORMAT (1X,6I1,I4)                                                
C  READ IN BIAS AND BIN WIDTH INFORMATION FOR TIMING AND POSITION STUDY 
      READ (51,55) COMMNT                                                
      IF(NSW(2).NE.0) WRITE (61,56) COMMNT                               
      READ(51,4) TBIAS, DTIM, DPOS                                       
      IF(NSW(2).NE.0) WRITE(61,7) TBIAS,DTIM,DPOS                        
      READ(51,55)COMMNT                                                  
      IF(NSW(2).NE.0) WRITE(61,56) COMMNT                                
      READ(51,5) (ELOPE(I),I=2,3)                                        
    5 FORMAT(3F6.2,2I2)                                                 
      ELOPE(1) = 0.                                                     
      IF(NSW(2).NE.0) WRITE(61,605) (ELOPE(I),I=2,3)                     
C                                                                       
C  READ IN CROSS SECTION DATA                                           
C  EDAT(I) CONTAINS THE ENERGY (IN MEV) FOR THE ITH DATA POINT          
C  SDAT(I,J) CONTAINS TOTAL CROSS-SECTION (IN BARNS) FOR REACTION       
C  CHANNEL J FOR ITH DATA POINT                                         
C                                                                       
C  REACTION CHANNELS ARE AS FOLLOWS                                     
C    J=1  NP ELASTIC SCATTERING                                         
C    J=2  NC NONDIFFRACTIVE ELASTIC SCATTERING                          
C    J=3  N+C--N+C+GAMMA                                                
C    J=4  N+C--ALPHA+9BE                                                
C    J=5  N+C--N+3 ALPHAS                                               
C    J=6  N+C--(N+P+11B) AND (2N+11C) OR (P+12B) AND (2N+11C)           
C    J=7  NC DIFFRACTIVE ELASTIC SCATTERING                             
C. NOTE--THE LABELING OF REACTION CHANNELS J IN THE ARRAY S(J) RETURNED 
C. BY SUBROUTINE SIGTOT IS SOMEWHAT DIFFERENT.  SEE LISTING OF SIGTOT.  
C                                                                       
C. (UNUSED ELEMENTS OF EDAT ARE INITIALIZED TO 1.0E7 FOR BINARY SEARCH) 
      DO 2 I=1,128                                                      
    2 EDAT(I)=1.0E7                                                     
      READ (51,55) COMMNT                                                
      IF(NSW(2).NE.0) WRITE(61,56) COMMNT                                
      READ (51,1) N                                                      
    1 FORMAT (I3)                                                       
      READ (51,55) COMMNT                                                
      IF(NSW(2).NE.0) WRITE (61,1) N                                     
      IF(NSW(2).NE.0) WRITE(61,56) COMMNT                                
C  (N CROSS-SECTION CARDS FOLLOW)                                       
C                                                                       
      DO 1331 I=1,N                                                     
 1331 READ (51,3) EDAT(I), (SDAT(I,J),J=1,7)                             
603   FORMAT(16F5.3)                                                    
605   FORMAT(1X,16F8.3)                                                 
    3 FORMAT (8F5.3)                                                    
      IF(NSW(2).NE.0) WRITE(61,1334) (EDAT(I),(SDAT(I,J),J=1,7),I=1,N)   
 1334 FORMAT (F8.2,5X,7F7.3)                                            
C  READ IN COEFFICIENT ARRAY FOR NC NON DIFFRACTIVE ELASTIC SCATTERING  
C  ED(I) CONTAINS ENERGY FOR ITH DATA POINT                             
C  AD(I,J),J=1 TO 5, CONTAINS JTH SHAPE PARAMETER FOR ITH DATA POINT    
      READ (51,55) COMMNT                                                
      IF(NSW(2).NE.0) WRITE(61,56) COMMNT                                
      READ(51,1) M                                                       
      READ (51,55) COMMNT                                                
      IF(NSW(2).NE.0) WRITE (61,1) M                                     
      IF(NSW(2).NE.0) WRITE(61,56) COMMNT                                
C  (M DATA CARDS FOLLOW)                                                
      DO 1349 J=1,32                                                    
 1349 ED(J)=1.0E7                                                       
      DO 1350 I=1,M                                                     
      READ (51,4) ED(I),(AD(I,J),J=1,5)                                  
C  AD(I,J),J=6 TO 8, CONTAINS JTH AREA PARAMETER FOR ITH DATA POINT     
      AD(I,6)=2.*AD(I,1)                                                
      AD(I,7)=(1.-AD(I,3))*(AD(I,2)-AD(I,1))*.5                         
      IF (AD(I,7) .LT. 0.) AD(I,7)=0.                                   
      AD(I,8)=(1.+AD(I,5))*(AD(I,4)-AD(I,1))*.5                         
      IF (AD(I,8) .LT. 0.)  AD(I,8)=0.                                  
 1350 IF(NSW(2).NE.0) WRITE(61,4)  ED(I), (AD(I,J),J=1,8)                
C  READ SCINTILLATOR BOUNDARIES AND 1 PHOTOELECTRON LEVELS              
      READ (51,55) COMMNT                                                
      WRITE (61,56) COMMNT                                               
      READ (51,55) COMMNT                                                
      WRITE (61,56) COMMNT                                               
      READ(51,5006)KSEED                                                 
      WRITE(61,5007)KSEED                                                
 5006 FORMAT(I9)                                                        
 5007 FORMAT(1X,I9)                                                     
      READ (51,55) COMMNT                                                
      WRITE (61,56) COMMNT                                               
      READ (51,4) RHO, COMP                                              
      WRITE (61,7) RHO, COMP                                             
    4 FORMAT(10F8.3)                                                    
    7 FORMAT(1X,10F8.3)                                                 
      DCARB=.6025*2.54*RHO/(12.+COMP)                                   
      DHYD=DCARB*COMP                                                   
      READ(51,55) COMMNT                                                 
      WRITE(61,56) COMMNT                                                
      READ(51,104) PA,PB,PC,PR,PS,PT                                     
C    READ COEFFICIENTS FOR LIGHT RESPONSE FUNCTIONS FOR PROTONS         
      WRITE(61,107) PA,PB,PC,PR,PS,PT                                    
      READ(51,55) COMMNT                                                 
      WRITE(61,56) COMMNT                                                
      READ(51,104) AG,AH,AO,AP,AQ,AU                                     
C    READ COEFFICIENTS FOR LIGHT RESPONSE FUNCTIONS FOR ALPHAS          
      WRITE(61,107) AG,AH,AO,AP,AQ,AU                                    
  104 FORMAT(6F12.8)                                                    
  107 FORMAT(1X,6F12.8)                                                 
      READ(51,55) COMMNT                                                 
      WRITE(61,56) COMMNT                                                
      READ(51,5) XB,YB,ZB,IGEO,IRANP                                     
      WRITE(61,5) XB,YB,ZB,IGEO,IRANP                                    
C   IGEO DEFINES SHAPE OF SCINTILLATOR                                  
C         = 0 RECTANGULAR                                               
C         = 1 CYLINDER VIEWED ON TOP                                    
C         = 2 CYLINDER VIEWED ON CURVED SIDE                            
C                                                                       
C INITIALIZE HISTOGRAM ARRAYS AND COUNTERS                              
      READ(51,55) COMMNT                                                 
      IF(NSW(2).NE.0) WRITE(61,56) COMMNT                                
      READ(51,55) COMMNT                                                 
      IF(NSW(2).NE.0) WRITE(61,56) COMMNT                                
      READ(51,55) COMMNT                                                 
      IF(NSW(2).NE.0) WRITE(61,56) COMMNT                                

C*********** Start Working **************

 1000 DO 10 I=1,9                                                       
      DO 9 J=1,400                                                      
    9 LT(J,I)=0                                                         
      LCON(I)=0                                                         
   10 LHI(I)=0                                                          
      NDET=0                                                            
      NDUMP=0                                                           
      IN=0                                                              
C  ZERO TIMING AND POSITION ARRAYS                                      
      DO 11 I=1,100                                                     
      NTIM(I)=0                                                         
   11 NPOS(I)=0                                                         
      DO 12 I=1,5                                                       
      DO 12 J=1,6                                                       
      NFS(J,I,1)=0                                                      
   12 NFS(J,I,2)=0                                                      
 4100 READ(51,15,END=9999) (XI(I),I=1,3),(CI(I),I=1,3),E1,DE,NEV,BINW    
c
	if(mdump.gt.0)write(11)e1,xi,ci,xb,yb,zb
c
      write(*,*)'Start processing energy:',E1
   15 FORMAT(3F5.2,3F7.4,2F8.3,I8,F8.3)                                 
      IF(DE .LE. 0.) DE=0.                                              
C     DE IS THE DESIRED BINWIDTH CENTERED ABOUT E1                      
 2000 IN=IN+1                                                           
      DO 30 I=1,3                                                       
      X(I)=XI(I)                                                        
   30 C(I)=CI(I)                                                        
      IF (IRANP .NE. 1) GO TO 2006                                      
      IF (IGEO-1) 2002,2004,2005                                        
 2002 X(2)=YB*(1.-2.*UNIRND(Y))                                         
      X(1)=XB*(1.-2.*UNIRND(Y))                                         
      GO TO 2006                                                        
C  IF CYLINDRICAL GEOMETRY, CHOOSE RANDOM RADIUS FROM LINEAR DISTRIBUTIO
 2004 R=XB*SQRT(UNIRND(Y))                                              
      PHI=6.2832*UNIRND(Y)                                              
      X(1)=R*COS(PHI)                                                   
      X(2)=R*SIN(PHI)                                                   
      GOTO 2006                                                         
 2005 X(3) = ZB*UNIRND(Y)                                               
      X(1) = XB*(1.-2.*UNIRND(Y))                                       
      X(2) = SQRT(YB*YB-X(1)*X(1))                                      
 2006 CONTINUE                                                          
      E=E1+DE*(.5-UNIRND(Y))                                            
      DO 40 I=1,9                                                       
   40 EL(I)=0.                                                          
C  CALL SUBROUTINE FOLNUT, WHICH FOLLOWS THE NEUTRON UNTIL IT EITHER    
C  ESCAPES FROM THE SCINTILLATOR OR DROPS BELOW 0.1 MEV IN ENERGY       
      CALL FOLNUT(X,C,E,.1,EL,MS,time)                                       
C                                                                       
      IF(EL(7) .LE. 0.) GO TO 46                                        
c
      if(mdump.gt.0)write(11) ndet,x,c,e,el,ms,time
c
      NDET = NDET + 1                                                   
      DO 44 J=2,3                                                       
      K=J+6                                                             
      SIG=SQRT(EL(7)/ELOPE(J)+.5)*ELOPE(J)                              
C---GAUSS-ROUTINE REPLACED BELOW                                        
C    APPROXIMATION TO NORMAL DISTRIBUTION USING CENTRAL                 
C    LIMIT THEOREM AND UNIFORM RANDOM NUMBER GENERATOR                  
      GAUSSB = -6.0000                                                  
      DO 9998 IGAUSS = 1,12                                             
 9998 GAUSSB = GAUSSB+UNIRND(Y)                                         
   44 EL(K)=EL(7)+SIG*GAUSSB                                            
C--- GAUSS-ROUTINE REPLACED ABOVE                                       
   46 DO 50 I=1,9                                                       
      IF (EL(I) .LE. 0.) GO TO 50                                       
      L=IFIX(EL(I)/BINW)+1                                              
      IF(L.GT. 400) L=400                                               
      LT(L,I)=LT(L,I)+1                                                 
      LCON(I)=LCON(I)+1                                                 
      LHI(I)=MAX0(LHI(I),L)                                             
   50 CONTINUE                                                          
C.    IF (NSW(5) .EQ. 1) WRITE(61,51) IN,NDET                            
C. 51 FORMAT (2I8)                                                      
      NSW(3)=NSW(3)+1                                                   
      IF(IN .GE. NEV) GO TO 3000                                        
C.    IF (N1 .EQ. 1) GO TO 3000                                         
      GO TO 2000                                                        
 3000 CONTINUE                                                          
      XBB=2.*XB                                                         
      YBB=2.*YB                                                         
      I = IGEO+1                                                        
      GOTO (3002,3004,3004),I                                           
 3002 WRITE(61,3003) ZB,XBB,YBB                                          
       GO TO 3006                                                       
 3004 WRITE(61,3005) ZB,XB                                               
 3003 FORMAT(52H1RECTANGULAR SCINTILLATOR WITH DIMENSIONS (INCHES)     
     1,  F8.3,8H DEEP BY ,F8.3,4H BY  ,F8.3,' (Z,X,Y-DIM)')              
 3005 FORMAT(52H1CYLINDRICAL SCINTILLATOR WITH DIMENSIONS (INCHES)      
     1,  F8.3,8H DEEP BY , F8.3,10H IN RADIUS   )                       
 3006 CONTINUE                                                          
      WRITE(61,56) COMMNT                                                
      WRITE(61,57) RHO, COMP                                             
   57 FORMAT (9H0DENSITY= , F7.3,9H GM/CM**3 ,5X,10HH/C RATIO= ,F7.3)   
      WRITE(61,300) IN,E1,DE,(XI(I),I=1,3),(CI(I),I=1,3)                 
  300 FORMAT(1H0,I7,20H NEUTRONS OF ENERGY ,F6.2,6H BINW ,F6.2,' MEV'// 
     1 20H INCIDENT AT X,Y,Z= ,3F7.2, 7H INCHES/                        
     2 20H DIRECTION COSINES= ,3F7.4 /)                                 
      IF (IRANP  .EQ. 1) WRITE (61,301)                                  
  301 FORMAT (30H ** RANDOM INITIAL POSITION **   )                     
      WRITE(61,3101) KSEED,NDET                                          
 3101 FORMAT('0LAST SEED NUMBER ',I12/                                  
     *  ' TOTAL NUMBER OF NEUTRONS DETECTED ',I10)                      
      IF(NSW(1).EQ.0) GOTO 3362                                         
      IF(NSW(1).EQ.1) GOTO 3360                                         
      WRITE (61,3007)                                                    
      WRITE (61,3008)                                                    
 3007 FORMAT (36H DIFFERENTIAL PULSE HEIGHT SPECTRA     /)              
 3008 FORMAT (40H LIGHT OUTPUT IN MEV ELECTRON EQUIVALENT   )           
      DO 350 I=1,6                                                      
      NC=LHI(I)                                                         
      WRITE (61,302) I,BINW,LCON(I)                                      
      IF (LCON(I) .EQ. 0) GO TO 350                                     
  302 FORMAT(  9H0CHANNEL ,I3,5H ONLY  ,5X,11H BIN WIDTH ,F5.3,6X,I8,   
     1  8H EVENTS    )                                                  
      EN=LCON(I)                                                        
      DO 351 J=1,NC                                                     
      EFF(J)=EN/FLOAT(IN)                                               
351   EN=EN-FLOAT(LT(J,I))                                              
      WRITE(61,304)(LT(L,I),L=1,NC)                                      
      WRITE(61,3009)                                                     
      WRITE(61,310)(EFF(J),J=1,NC)                                       
  350 CONTINUE                                                          
  304 FORMAT  (1H ,10I5)                                                
      DO 360 I=1,3                                                      
      K=I+6                                                             
      WRITE (61,306) ELOPE(I),BINW,LCON(K)                               
      NC=LHI(K)                                                         
  306 FORMAT (33H0LIGHT OUTPUT FROM ALL CHANNELS , ,5X,                   
     1 28H ONE PHOTOELECTRON LEVEL AT , F7.3,5H MEV /                   
     2 11H BIN WIDTH ,F5.3,5H MEV , 6X,I7, 8H EVENTS  )                 
  360 WRITE (61,304) (LT(L,K),L=1,NC)                                    
      WRITE (61,3009)                                                    
 3009 FORMAT (1H0//,30H INTEGRAL PULSE HEIGHT SPECTRA   /)              
      WRITE (61,3008)                                                    
 3360 I1 = 1                                                            
      I2 = 3                                                            
      GOTO 3365                                                         
 3362 I1 = 2                                                            
      I2 = I1                                                           
 3365 DO 380 I=I1,I2                                                    
      K = I+6                                                           
      NC=LHI(K)                                                         
      WRITE(61,308) ELOPE(I),BINW                                        
  308 FORMAT (1H0/30H INTEGRAL EFFICIENCY,ONE P.E.= ,F7.3,5H MEV ,3X,   
     1 12H BIN WIDTH , ,F5.3,5H MEV )                                     
      WRITE(61,309)                                                      
  309 FORMAT (43H NOTE  FIRST BIN CORRESPONDS TO ZERO BIAS.   /)        
      EN=LCON(K)                                                        
      DO 370 J=1,NC                                                     
      EFF(J)=EN/FLOAT(IN)                                               
  370 EN=EN-FLOAT(LT(J,K))                                              
  380 WRITE(61,310) (EFF(J),J=1,NC )                                     
  310 FORMAT (1H ,10F8.4)                                               
      IF(NSW(1).LT.2) GOTO 1000                                         
      WRITE(61,400)                                                      
  400 FORMAT (20H0FIRST SCATTER DATA   )                                
      WRITE(61,3010)                                                     
 3010 FORMAT(7H0 BIAS ,6X,35HCH.1  CH.2  CH.3  CH.4  CH.5  CH.6  )      
  420 FORMAT (1H0,F6.2,6X,6I6)                                          
      BIAS=.125                                                         
      DO 410 I=1,5                                                      
      BIAS=BIAS*2.                                                      
  410 WRITE (61,420) BIAS,(NFS(J,I,1),J=1,6)                             
C  OUTPUT SECOND SCATTER INFORAMATION                                   
      WRITE (61,500)                                                     
  500 FORMAT (21H0SECOND SCATTER DATA     )                             
       WRITE (61,3010)                                                   
       BIAS=.125                                                        
       DO 510 I=1,5                                                     
       BIAS=BIAS*2.                                                     
  510 WRITE (61,420) BIAS,(NFS(J,I,2),J=1,6)                             
C  OUTPUT TIMING AND POSITION INFORMATION                               
       WRITE(61,520) TBIAS                                               
  520 FORMAT ( 60H0HISTOGRAMS OF TIME DELAY AND X-COORD. DRIFT BEFORE BI
     1AS OF     ,F5.3, 17H MEV IS ATTAINED    )                         
       WRITE (61,521) DTIM                                               
  521 FORMAT (8H0TIMING  ,F5.3,10H NSEC/BIN   )                         
       WRITE(61,522) NTIM                                                
  522 FORMAT (1X,2(10I5,5X))                                            
      WRITE (61,523) DPOS                                                
  523 FORMAT (10H0POSITION ,F5.3,10H INCH/BIN  )                        
      WRITE (61,522) NPOS                                                
      GO TO 1000                                                        
 9999 CALL EXIT                                                         
	close(11)
	close(61)
	close(51)
      END                                                               
      SUBROUTINE FOLNUT(X,C,E,EMIN,EL,NS,time)
C-----------------------------------------------------------------------
C                                                                       
C     FOLNUT IS CALLED ONCE PER INCIDENT NEUTRON TO FOLLOW THE          
C     NEUTRON THROUGH THE SCINTILLATOR AND VARIOUS REACTIONS            
C     ESSENTIALLY IT RETURNS ONLY EL ,THE LIGHT PRODUCED                
C                                                                       
C     MANY SUBROUTINES THAT WERE PREVIOUSLY CODED AS SUBROUTINES        
C     AND WERE CALLED FROM WITHIN FOLNUT HAVE BEEN RECODED AS           
C     PART OF FOLNUT. THIS ELIMINATES MANY JUMPS TO SUBROUTINES         
C     AND MUCH MEMORY SWAPPING AND OVERLAYING ON SOME COMPUTERS         
C     THE RESULTING CODE EXECUTES FASTER                                
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C  GIVEN A NEUTRON OF ENERGY E AT POSITION X(I),I=1 TO 3,               
C  AND MOVING ALONG DIRECTION COSINES C(I),I=1 TO 3,                    
C  FOLNUT FOLLOWS THE NEUTRON THROUGH SUCCESSIVE INTERACTIONS           
C  UNTIL IT EITHER ESCAPES THE INTERACTION VOLUME, OR IS ABSORBED,      
C  OR UNTIL ITS ENERGY DROPS BELOW EMIN, OR MORE THAN 10 SCATTERS OCCUR.
C  FOLNUT RETURNS EL(L),L=1 TO 7, AN ARRAY CONTAINING THE LIGHT OUTPUT  
C  IN EACH OF THE SIX REACTION CHANNELS (SEE BELOW), AND THE TOTAL      
C  LIGHT OUTPUT (L=7). LIGHT OUTPUT IS EXPRESSED IN MEV ELECTRON EQUIVAL
C  FOLNUT ALSO RETURNS NS, THE NUMBER OF INTERACTIONS DURING THE HISTORY
C  FOLNUT ALSO PERFORMS THE BOOKKEEPING FOR THE STUDY OF CHANNEL        
C  DISTRIBUTIONS OF FIRST AND SECOND INTERACTIONS, AND THE STUDIES      
C  OF TIME DELAY AND POSITION DRIFT  (SEE BELOW).                       
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
      DIMENSION X1S(3),C1S(3)                                           
      DIMENSION X(3),C(3),EL(7),S(8),XG(3),CG(3),NF(5)                  
      COMMON /COEF/ C1,C2,C3,V                                          
      COMMON /SIGDAT/ EDAT(128),SDAT(128,7)                             
      COMMON /FIRSC/ NFS(6,5,2)                                         
      COMMON /SCIN/ DHYD,DCARB,IGEO,XB,YB,ZB                            
      COMMON /SWBLK/ NSW(6),NDUMP,MDUMP                                 
      COMMON /TIMING/ TBIAS,DTIM,DPOS,NTIM(100),NPOS(100)               
      COMMON /CRBANG/ ED(32),AD(32,8)                                   
      COMMON /ENG/ U                                                    
      EXTERNAL ENGDIS                                                   
      EXTERNAL ANGDIS                                                   
      DATA RM1,RM2,RM3,RM4,RM2A/939.55,11178.,3728.3,8394.6,11182.4/    
      DATA QQ,RM5,RM6/-7.26,1878.5,10255./                              
C  INITIALIZE                                                           
      L=7                                                               
      NSMAX=10                                                          
      NS=0                                                              
      TIME=0.                                                           
      ICOUNT=0                                                          
      X0=X(1)                                                           
      DO 4 I=1,L                                                        
    4 EL(I)=0.                                                          
C  PREPARE TO PRINT INDIVIDUAL HISTORIES, IF DESIRED.                   
      NDUMP=NDUMP+1                                                     
      IF (NDUMP .GT. MDUMP) GO TO 2000                                  
      IF (NDUMP .EQ. 1) WRITE (61,9)                                     
    9 FORMAT( 31H1 DUMP OF INDIVIDUAL HISTORIES  ,/,                    
     1  56H THE FOLLOWING VARIABLES ARE DUMPED AT EACH INTERACTION  ,/, 
     2 37H (SEE FOLNUT LISTING FOR DEFINITION)  ,/,                     
     356H E,EL(7),MS,ELT,(X(I),I=1,3),(C(I),I=1,3),COSL,PHI,TIME    )   
      WRITE (61,11) NDUMP,E                                              
   11 FORMAT(7H0EVENT ,I5,5X,14HINITIAL ENERGY  ,F8.3,4H MEV  )         
C  FIND TOTAL CROSS SECTIONS                                            
 2000 CONTINUE                                                          
      IF (E .LT. EMIN) RETURN                                           
C   START OF SIGTOT  -------------------------------------------------- 
C     COMMON SIGDAT ADDED FOR SIGTOT ROUTINE                            
C  PURPOSE--TO RETURN THE ARRAY S(J)  CONTAINING THE TOTAL CROSS-SECTION
C  (IN BARNS) AT ENERGY E FOR EACH REACTION CHANNEL J.                  
      LSIG = IBNSH(E,EDAT,128)                                          
      DEL = (E-EDAT(LSIG))/(EDAT(LSIG+1)-EDAT(LSIG))                    
      DO 10010 I = 1,7                                                  
10010 S(I) = SDAT(LSIG,I)+(SDAT(LSIG+1,I)-SDAT(LSIG,I))*DEL             
      S(8) = S(7)                                                       
      S(7) = S(3)+S(4)+S(5)+S(6)                                        
C   END OF SIGTOT ------------------------------------------------------
       ELT=0.                                                           
C  FIND DISTANCE TO NEXT SCATTER.                                       
C  SHH AND SCC ARE THE TOTAL CROSS-SECTIONS IN HYDROGEN AND CARBON.     
C DHYD AND DCARB ARE THE NUMBER OF H AND C NUCLEI PER BARN-INCH         
C  PASSED THROUGH COMMON BLOCK  /SCIN/                                  
      SHH=S(1)                                                          
      SCC=S(2)+S(7)+S(8)                                                
      PATH=1./(DHYD*SHH+DCARB*SCC)                                      
      D=-PATH*ALOG(UNIRND(Y))                                           
C  PROPAGATE NEUTRON A DISTANCE D.                                      
      DO 20 I=1,3                                                       
   20 X(I)=X(I)+C(I)*D                                                  
C  IS NEUTRON STILL IN BOUNDS?                                          
      IF(INBNDS(X) .LE. 0) RETURN                                       
C  THE FUNCTION INBNDS(X)=1 IF VECTOR X IS INSIDE THE SCINTILLATOR,     
C                        =0 IF X IS OUTSIDE.                            
      NS=NS+1                                                           
C  METER THE ELAPSED TIME (NANOSECONDS) SINCE NEUTRON ENTERED SCINT.    
      TIME=TIME+D*1.84/SQRT(E)                                          
      IF(NS .GT. NSMAX) RETURN                                          
C  DECIDE WHETHER CARBON INTERACTION OCCURRED.                          
      IF(DCARB*SCC*PATH .GT. UNIRND(Y)) GO TO 200                       
C  ELASTIC SCATTER FROM HYDROGEN OCCURRED (REACTION CHANNEL 1)          
  100 MS=1                                                              
C   START OF NPNP ROUTINE --------------------------------------------- 
C   THE SCATTERING IS ASSUMED ISOTROPIC IN THE CM BELOW 30 MEV.ABOVE    
C   30 MEV THE ANGULAR DISTRIBUTION BECOMES INCREASINGLY PARABOLIC.     
C   COMMON COEF ADDED                                                   
      IF(E.GT.30.) GOTO 10110                                           
      COSCM = -1.+2.*UNIRND(Y)                                          
 9105 CALL KINNR(RM1,RM1,RM1,RM1,E,COSCM,VB,WCM,PCM,COSL,W)             
      COSLP = COS(1.5703-ACOS(COSL))                                   
      ELT = EEQUIV(E-W,X,C,COSLP,1)                                     
      GOTO 9120                                                         
C   ABOVE 30 MEV THE ANGULAR DISTRIBUTION IS GIVEN BY ANGDIS,WITH       
C   THE COEFFICIENTS C,D,F AND U (COMMON/COEF/).RTNI INVERTS ANGDIS.    
10110 R = E/30.                                                         
      C1 = 3./(3.+R)                                                    
      C2 = R*C1/3.                                                      
      C3 = 0.                                                           
      V = UNIRND(Y)                                                     
      CALL RTNI(COSCM,FN,DV,ANGDIS,.5,.001,20,IER)                      
      IF(UNIRND(Y).GT.0.5) COSCM = -COSCM                               
      GOTO 9105                                                         
 9120 CONTINUE                                                          
C   END OF NPNP ROUTINE ------------------------------------------------
C  NPNP RETURNS THE NEW NEUTRON ENERGY W, THE COSINE OF THE LAB         
C  SCATTERING ANGLE COSL, AND THE LIGHT OUTPUT ELT.                     
      GO TO 1000                                                        
C  CARBON INTERACTION OCCURED.  DECIDE WHETHER ELASTIC OR INELASTIC.    
  200 IF (S(7)/SCC .GT. UNIRND(Y)) GO TO 300                            
C  ELASTIC SCATTER FROM CARBON OCCURRED (REACTION CHANNEL 2)            
      MS=2                                                              
C   START OF NCEL ROUTINE ----------------------------------------------
C   COMMON/CRBANG/ ABOVE                                                
C   DECIDE WETHER DIFFRACTION PATTERN OCCURED                           
      IF(S(8)/(S(2)+S(8)).GE.UNIRND(Y)) GOTO 10350                      
C   INTERPOLATE IN TABLE                                                
      LC = IBNSH(E,ED,32)                                               
      DEL = (E-ED(LC))/(ED(LC+1)-ED(LC))                                
      DO 10302 I=1,3                                                    
      CG(I)=AD(LC,I+5)+(AD(LC+1,I+5)-AD(LC,I+5))*DEL                    
10302 IF(CG(I) .LT. 0.) CG(I)=0.                                        
      AT=CG(1)+CG(2)+CG(3)                                              
C   CHOOSE PORTION OF ANGULAR DISTRIBUTION                              
      IF(CG(1)/AT .GE. UNIRND(Y)) GO TO 9310                            
      IF(CG(2)/(CG(2)+CG(3)) .GE. UNIRND(Y)) GO TO 10320                
      GO TO 10330                                                       
C  RECTANGLE (ISOTROPIC)                                                
 9310 COSCM=-1.+2.*UNIRND(Y)                                            
      GO TO 10311                                                       
C  FORWARD TRIANGLE                                                     
10320 X1=AD(LC,3)+(AD(LC+1,3)-AD(LC,3))*DEL                             
      COSCM=X1+(1.-X1)*SQRT(UNIRND(Y))                                  
      GO TO 10311                                                       
C  BACKWARD TRIANGLE                                                    
10330 X1=AD(LC,5)+(AD(LC+1,5)-AD(LC,5))*DEL                             
      COSCM=X1-(X1+1.)*SQRT(UNIRND(Y))                                  
C  CALL THE KINEMATICS SUBROUTINE KINNR.                                
10311 IF(COSCM .GT. 1.) COSCM=1.000000                                  
      IF(COSCM .LT.-1.) COSCM=-1.000000                                 
      CALL KINNR(RM1,RM2,RM1,RM2,E,COSCM,VB,WCM,PCM,COSL,W)             
C  LIGHT OUTPUT FROM CARBON                                             
      ELT=0.017*(E-W)                                                   
      GOTO 9333                                                         
C  DIFFRACTION ELASTIC SCATTERING.  EXPONENTIAL FORWARD PEAK, WITH WIDTH
C  INVERSELY PROPORTIONAL TO THE TOTAL INELASTIC CROSS SECTION, AND     
C  TO THE NEUTRON ENERGY E.                                             
10350 COSCM=1.+ALOG(1.-UNIRND(Y))/1.17/S(7)/E                           
      GO TO 10311                                                       
 9333 CONTINUE                                                          
C   END OF NCEL ROUTINE ------------------------------------------------
C  W,COSL, ELT ARE DIFINED UNDER NPNP ABOVE                             
      GO TO 1000                                                        
C  INELASTIC CARBON INTERACTION OCCURRED.  CHOOSE WHICH.                
  300 IF (S(3)/S(7) .LE. UNIRND(Y)) GO TO 400                           
C  N+C--N+C+GAMMA OCCURRED (REACION CHANNEL 3)                          
       MS=3                                                             
C   START OF NCNCGM ROUTINE ------------------------------------------- 
C  SIMULATE THE REACTION N+C--N+C+GAMMA                                 
C  FIND THE GAMMA ANGLE.  THE ANGULAR DISTRIBUTION (TAKEN FROM 14 MEV   
C  DATA) IS GIVEN BY ANGDIS, WHICH IS INVERTED BY RTNI.                 
C  C1,C2,C3, AND V ARE COEFFICIENTS FOR ANGDIS.                         
      V = UNIRND(Y)                                                     
      C1 = .7444                                                        
      C2 = .4342                                                        
      C3 = -.1786                                                       
      CALL RTNI(CSG,F,D,ANGDIS,.5,.001,10,IER)                          
      IF(IER .NE. 0) WRITE(61,9111) IER,CSG,F,D                          
 9111 FORMAT (4H ***,I3,3E12.4)                                         
      IF(UNIRND(Y) .GT. .5) CSG = -CSG                                  
C  FOLLOW THE GAMMA                                                     
C  SCATTR FINDS THD DIRECTION COSINES CG(I) OF THE GAMMA FROM THE       
C  INITIAL NEUTRON DIRECTION COSINES C(I) AND THE LAB COSINE CSG OF THE 
C  GAMMA EMISSION ANGLE AND A UNIFORMLY-DISTRIBUTIED AZIMUTH ANGLE PHI  
      DO 9150 IM = 1,3                                                  
 9150 XG(IM) = X(IM)                                                    
      EMM  = 0.1                                                        
      PHI = 6.2832*UNIRND(Y)                                            
      EGM  = 4.43                                                       
      CALL SCATTR(C,CSG,PHI,CG)                                         
      CALL FOLGAM(XG,CG,EGM,EMM,ELT)                                    
C  DETERMINE THE NEUTRON ANGLE.  THE ANGULAR DISTRIBUTION IS TAKEN FROM 
C  10 MEV PP DATA.                                                      
C  C1,C2,C3, AND V ARE COEFFICIENTS FOR ANGDIS.                         
      C1 = .5357                                                        
      C2 = .8929                                                        
      C3 = -.4826                                                       
      V = UNIRND(Y)                                                     
      CALL RTNI (CSN,F,D,ANGDIS,.5,.001,10,IERR)                        
      IF(IERR .NE. 0) WRITE(61,9111) IERR,CSN,F,D                        
      IF(UNIRND(Y) .GT. .5) CSN = -CSN                                  
C  TRANSFORM NEUTRON TO LAB COORDINATES                                 
C  KINNR IS THE KINEMATICS SUBROUTINE                                   
      CALL KINNR(RM1,RM2,RM1,RM2A,E,CSN,VB,WCM,PCM,COSL,W)              
C   END OF NCNCGM ROUTINE --------------------------------------------  
      GO TO 1000                                                        
  400 SR=S(4)+S(5)+S(6)                                                 
      U=UNIRND(Y)                                                       
      IF (S(4)/SR .LE. U) GO TO 500                                     
C  N+C--ALPHA+9BE    OCCURRED (REACTION CHANNEL 4)                      
      MS=4                                                              
C   START OF NALPHA ROUTINE-------------------------------------------- 
C  SIMULATE THE REACTION N+C--ALPHA+9BE                                 
C  ISOTROPIC TWO-BODY REACTION ASSUMED.                                 
      CNAL = -1.+2.*UNIRND(Y)                                           
      CALL KINNR(RM1,RM2,RM3,RM4,E,CNAL,VB,WCM,PCM,COSL,T)              
      ELT = EEQUIV(T,X,C,COSL,4)                                        
C   END OF NALPHA ROUTINE---------------------------------------------- 
      GO TO 950                                                         
  500 IF((S(4)+S(5))/SR .LE. U) GO TO 600                               
C  N+C--N+THREE ALPHAS OCCURRED (REACTION CHANNEL 5)                    
      MS=5                                                              
C   START OF NN3AL ROUTINE--------------------------------------------- 
C  SIMULATES THE REACTION N+C--N+THREE ALPHAS                           
C  PHASE SPACE ENERGY DISTRIBUTION OF OUTGOING NEUTRON ASSUMED.         
C      DATA EMN, EMC,QQ/940.,11280.,-7.26/                              
C     COMMON/ENG/U                                                      
C     EXTERNAL ENGDIS                                                   
      WCM=E*.923+QQ                                                     
      IF(WCM) 2,2,15004                                                 
    2 W=E                                                               
      COSL=1.                                                           
      GOTO 9500                                                         
C  MAXIMUM NEUTRON ENERGY IN CM                                         
15004 TM=.923*WCM                                                       
C  SIMULATE PHASE SPACE DISTRIBUTION FOR OUTGOING NEUTRON               
      U=UNIRND(Y)                                                       
      X3=.01                                                            
      IF(U .LE. .01) GO TO 3                                            
      CALL RTNI(X3,FN,DV,ENGDIS,.5,.001,20,IER)                         
      IF(IER .NE. 0) WRITE (61,15001) IER,X3,FN,DV                       
15001 FORMAT ( 8HERRNN3AL,I3,3E12.4)                                    
    3 T=TM*X3                                                           
      IF(T .LE. 0.) T=0.                                                
C  FIND THE LAB ENERGY EN AND COSINE COSL OF THE LAB ASCATTERING ANGLE  
C  FOR THE OUTGOING NEUTRON                                             
      PCM=SQRT(2.*RM1*T)                                                
      VB=SQRT(2.*RM1*E)/(RM1+RM2)                                       
C  ISOTROPIC CM ANGULAR DISTRIBUTION OF THE NEUTRON IS ASSUMED.         
      COSCM=-1.+2.*UNIRND(Y)                                            
      W=T+PCM*VB*COSCM+.5*RM1*VB**2                                     
      IF(W .LE. 0.) W=0.                                                
      PN=SQRT(2.*RM1*W)                                                 
      COSL=(PCM*COSCM+RM1*VB)/PN                                        
      TR=E-W+QQ                                                         
      DO 15 I=1,2                                                       
      TA=TR*UNIRND(Y)                                                   
      ELT = EEQUIV(TA,X,C,UNIRND(Y),4) + ELT                            
   15 TR=TR-TA                                                          
      ELT = EEQUIV(TR,X,C,UNIRND(Y),4) + ELT                            
 9500 CONTINUE                                                          
C   END OF NN3AL ROUTINE----------------------------------------------- 
      GO TO 1000                                                        
C  N+C--P+12B  OCCURRED (REACTION CHANNEL 6)                            
  600 MS=6                                                              
C                                                                       
C     NOTE THAT THE N2N CROSS SECTION IS GROUPED OR ICLUDED IN          
C     THE CODE AS PART OF THE NPB CROSS SECTIONS.                       
C                                                                       
C     TO DETERMINE WHEN AN N2N OCCURS THE MONTE CARLO METHOD IS         
C     USED ALONG WITH AN EMPIRICAL EXPRESSION WHICH SIMPLY REPRESENTS   
C     THE SPARCE EXPERIMENTAL N2N CROSS SECTION DATA AVAILABLE          
C                                                                       
C                                                                       
      IF(E.LE.20.3) GOTO 9221                                           
C   CHOOSE NPB OR N2N                                                   
      DUMMY = .022*(1.-EXP((20.3-E)/7.))/S(6)                           
C    THE CHOICE BETWEEN NPB AND N2N IS MADE IN THE FOLLOWING IF-        
C    STATEMENT USING THE EMPIRICAL EXPRESION ABOVE WHICH GIVES THE      
C    N2N CROSS SECTION IN BARNS ABOVE 20.3 MEV (THRESHOLD).             
      IF(DUMMY.GT.UNIRND(Y)) GOTO 10222                                 
C   START OF NPB ROUTINE----------------------------------------------- 
 9221 DUMM1 = 10239.05+15.95                                            
 9224 CALL KINNR(RM1,RM2,RM5,DUMM1,E,SQRT(UNIRND(Y)),VB,WCM,PCM,COSLP,E)
      W = E*UNIRND(Y)                                                   
C    PROTON ENERGY UNIFORMLY DISTRIBUTED BETWEEN 0 AND MAX VALUE        
C    IF RESCATTERED NEUTRON ASSUME SAME SCATTERING ANGLE AS PROTON      
      COSL = COSLP                                                      
      ELT = EEQUIV(W,X,C,COSLP,1)                                       
C    APPROX. PCT N+C=N+P+B FOR RESCATTERED NEUTRON                      
      IF(UNIRND(Y).LE.0.1) GOTO 9220                                    
      W = E-W                                                           
C   GIVE NEUTRON ENERGY NOT GIVEN TO PROTON                             
      GOTO 1000                                                         
C   EEQUIV RETURNS LIGHT OUTPUT FOR PROTON OF ENERGY EN                 
C  (NO NEUTRON RELEASED)                                                
 9220 CONTINUE                                                          
C   END OF NPB ROUTINE--------------------------------------------------
      GOTO 950                                                          
C   START OF N2N ROUTINE----------------------------------------------- 
C    TREAT N2N SAME AS NPB KINEMATICALLY EXCEPT FOR THRESHOLD           
10222 CALL KINNR(RM1,RM2,1882.8,RM6,E,SQRT(UNIRND(Y)),VB,WCM,PCM,COSL,E)
      CALL SCATTR(C,COSL,6.2832*UNIRND(Y),C1S)                          
      DO 9223 I =1,3                                                    
 9223 X1S(I) = X(I)                                                     
      W = E*UNIRND(Y)                                                   
      E = E-W                                                           
      CALL SECNUT(X1S,C1S,E,.1,EL,NS1S)                                 
C    FOLLOW ONE NEUTRON WITH SECNUT AND THE OTHER STAYS IN FOLNUT       
C   END OF N2N ROUTINE------------------------------------------------- 
  950 W=0.                                                              
      COSL=1.                                                           
C  INCREMENT THE ELEMENT MS=1 TO 6 OF THE LIGHT OUTPUT ARRAY ELT        
C  CORRESPONDING TO THE CHANNEL OF INTERACTION, AND ELT(7),THE TOTAL    
C  LIGHT OUTPUT                                                         
 1000 EL(MS)=EL(MS)+ELT                                                 
      EL(7)=EL(7)+ELT                                                   
      E=W                                                               
      PHI=6.2832*UNIRND(Y)                                              
      CALL SCATTR(C,COSL,PHI,C)                                         
      IF(NSW(1).LT.2) GOTO 1110                                         
      IF(NS-2) 800,900,1100                                             
  800 MC=MS                                                             
      DO 802 I=1,5                                                      
  802 NF(I)=1                                                           
  900 BIAS=.125                                                         
      DO 910 I=1,5                                                      
      BIAS=BIAS*2.                                                      
      IF(NF(I))910,910,902                                              
  902 IF (ELT-BIAS) 910,905,905                                         
  905 NFS(MC,I,NS)=NFS(MC,I,NS)+1                                       
      NF(I)=0                                                           
  910 CONTINUE                                                          
 1100 IF (ICOUNT  .EQ. 1) GO TO 1110                                    
C  CHECK WHETHER TIMING BIAS IS EXCEEDED.  IF SO, SET FLAG AND INCREMENT
C  ARRAYS.                                                              
      IF(ELT-TBIAS) 1110,1105,1105                                      
 1105 ICOUNT=1                                                          
      JT=TIME/DTIM+1.                                                   
      IF(JT .GT. 100) JT=100                                            
      IF (JT .LT. 1) JT=1                                               
      JP=(X(1)-X0)/DPOS+50.                                             
      IF(JP .GT. 100) JP=100                                            
      IF(JP .LT. 1) JP=1                                                
      NTIM(JT)=NTIM(JT)+1                                               
      NPOS(JP)=NPOS(JP)+1                                               
C  PRINT DETAILED INFORMATION FOR THIS EVENT                            
 1110 IF (NDUMP .GE. MDUMP) GO TO 2000                                  
      WRITE (61,1) E,EL(L),MS,ELT,(X(I),I=1,3), (C(I),I=1,3),COSL,PHI    
     1 ,TIME                                                            
    1 FORMAT(F6.1,3X,F6.2,I3,F6.2,3X,3F6.2,3X,3F6.2,3X,3F7.4)           
      GO TO 2000                                                        
      END                                                               
      SUBROUTINE SECNUT(X,C,E,EMIN,EL,NS)                               
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C     THIS SUBROUTINE IS IDENTICAL TO FOLNUT, EXCEPT FOR A              
C     MODIFICATION TO NPB ROUTINE. IT IS CALLED ONLY FROM               
C     THE N2N ROUTINE WITHIN FOLNUT TO FOLLOW THE PATH OF               
C     THE SECOND NEUTRON THROUGH THE SCINTILLATOR.                      
C                                                                       
C     ON SOME MACHINES THAT SUPPORT RECURSIVE CALLS TO SUBROUTINES      
C     THIS SUBROUTINE CAN BE REPLACED BY A CALL TO FOLNUT WITHIN        
C     FOLNUT ITSELF, HOWEVER CODING IN THIS FASHION ALLOWS THE          
C     PROGRAM TO BE RUN ON ANY MACHINE EASILY.                          
C                                                                       
C------------------------------------------------------------------     
C                                                                       
      DIMENSION X(3),C(3),EL(7),S(8),XG(3),CG(3),NF(5)                  
      COMMON /COEF/ C1,C2,C3,V                                          
      COMMON /SIGDAT/ EDAT(128),SDAT(128,7)                             
      COMMON /FIRSC/ NFS(6,5,2)                                         
      COMMON /SCIN/ DHYD,DCARB,IGEO,XB,YB,ZB                            
      COMMON /SWBLK/ NSW(6),NDUMP,MDUMP                                 
      COMMON /TIMING/ TBIAS,DTIM,DPOS,NTIM(100),NPOS(100)               
      COMMON /CRBANG/ ED(32),AD(32,8)                                   
      COMMON /ENG/ U                                                    
      EXTERNAL ENGDIS                                                   
      EXTERNAL ANGDIS                                                   
      DATA RM1,RM2,RM3,RM4,RM2A/939.55,11178.,3728.3,8394.6,11182.4/    
      DATA QQ,RM5,RM6/-7.26,1878.5,10255./                              
C  INITIALIZE                                                           
      L=7                                                               
      NSMAX=10                                                          
      NS=0                                                              
      TIME=0.                                                           
      ICOUNT=0                                                          
      X0=X(1)                                                           
C  PREPARE TO PRINT INDIVIDUAL HISTORIES, IF DESIRED.                   
      NDUMP=NDUMP+1                                                     
      IF (NDUMP .GT. MDUMP) GO TO 2000                                  
      WRITE (61,11) NDUMP,E                                              
   11 FORMAT(7H0EVENT ,I5,5X,14HINITIAL ENERGY  ,F8.3,4H MEV  )         
C  FIND TOTAL CROSS SECTIONS                                            
 2000 CONTINUE                                                          
      IF (E .LT. EMIN) RETURN                                           
C   START OF SIGTOT  -------------------------------------------------- 
C     COMMON SIGDAT ADDED FOR SIGTOT ROUTINE                            
C  PURPOSE--TO RETURN THE ARRAY S(J)  CONTAINING THE TOTAL CROSS-SECTION
C  (IN BARNS) AT ENERGY E FOR EACH REACTION CHANNEL J.                  
      LSIG = IBNSH(E,EDAT,128)                                          
      DEL = (E-EDAT(LSIG))/(EDAT(LSIG+1)-EDAT(LSIG))                    
      DO 10010 I = 1,7                                                  
10010 S(I) = SDAT(LSIG,I)+(SDAT(LSIG+1,I)-SDAT(LSIG,I))*DEL             
      S(8) = S(7)                                                       
      S(7) = S(3)+S(4)+S(5)+S(6)                                        
C   END OF SIGTOT ------------------------------------------------------
       ELT=0.                                                           
C  FIND DISTANCE TO NEXT SCATTER.                                       
C  SHH AND SCC ARE THE TOTAL CROSS-SECTIONS IN HYDROGEN AND CARBON.     
C DHYD AND DCARB ARE THE NUMBER OF H AND C NUCLEI PER BARN-INCH         
C  PASSED THROUGH COMMON BLOCK  /SCIN/                                  
      SHH=S(1)                                                          
      SCC=S(2)+S(7)+S(8)                                                
      PATH=1./(DHYD*SHH+DCARB*SCC)                                      
      D=-PATH*ALOG(UNIRND(Y))                                           
C  PROPAGATE NEUTRON A DISTANCE D.                                      
      DO 20 I=1,3                                                       
   20 X(I)=X(I)+C(I)*D                                                  
C  IS NEUTRON STILL IN BOUNDS?                                          
      IF(INBNDS(X) .LE. 0) RETURN                                       
C  THE FUNCTION INBNDS(X)=1 IF VECTOR X IS INSIDE THE SCINTILLATOR,     
C                        =0 IF X IS OUTSIDE.                            
      NS=NS+1                                                           
C  METER THE ELAPSED TIME (NANOSECONDS) SINCE NEUTRON ENTERED SCINT.    
      TIME=TIME+D*1.84/SQRT(E)                                          
      IF(NS .GT. NSMAX) RETURN                                          
C  DECIDE WHETHER CARBON INTERACTION OCCURRED.                          
      IF(DCARB*SCC*PATH .GT. UNIRND(Y)) GO TO 200                       
C  ELASTIC SCATTER FROM HYDROGEN OCCURRED (REACTION CHANNEL 1)          
  100 MS=1                                                              
C   START OF NPNP ROUTINE --------------------------------------------- 
C   THE SCATTERING IS ASSUMED ISOTROPIC IN THE CM BELOW 30 MEV.ABOVE    
C   30 MEV THE ANGULAR DISTRIBUTION BECOMES INCREASINGLY PARABOLIC.     
C   COMMON COEF ADDED                                                   
      IF(E.GT.30.) GOTO 10110                                           
      COSCM = -1.+2.*UNIRND(Y)                                          
 9105 CALL KINNR(RM1,RM1,RM1,RM1,E,COSCM,VB,WCM,PCM,COSL,W)             
      COSLP = COS(1.5703-ACOS(COSL))                                   
      ELT = EEQUIV(E-W,X,C,COSLP,1)                                     
      GOTO 9120                                                         
C   ABOVE 30 MEV THE ANGULAR DISTRIBUTION IS GIVEN BY ANGDIS,WITH       
C   THE COEFFICIENTS C,D,F AND U (COMMON/COEF/).RTNI INVERTS ANGDIS.    
10110 R = E/30.                                                         
      C1 = 3./(3.+R)                                                    
      C2 = R*C1/3.                                                      
      C3 = 0.                                                           
      V = UNIRND(Y)                                                     
      CALL RTNI(COSCM,FN,DV,ANGDIS,.5,.001,20,IER)                      
      IF(UNIRND(Y).GT.0.5) COSCM = -COSCM                               
      GOTO 9105                                                         
 9120 CONTINUE                                                          
C   END OF NPNP ROUTINE ------------------------------------------------
C  NPNP RETURNS THE NEW NEUTRON ENERGY W, THE COSINE OF THE LAB         
C  SCATTERING ANGLE COSL, AND THE LIGHT OUTPUT ELT.                     
      GO TO 1000                                                        
C  CARBON INTERACTION OCCURED.  DECIDE WHETHER ELASTIC OR INELASTIC.    
  200 IF (S(7)/SCC .GT. UNIRND(Y)) GO TO 300                            
C  ELASTIC SCATTER FROM CARBON OCCURRED (REACTION CHANNEL 2)            
      MS=2                                                              
C   START OF NCEL ROUTINE ----------------------------------------------
C   COMMON/CRBANG/ ABOVE                                                
C   DECIDE WETHER DIFFRACTION PATTERN OCCURED                           
      IF(S(8)/(S(2)+S(8)).GE.UNIRND(Y)) GOTO 10350                      
C   INTERPOLATE IN TABLE                                                
      LC = IBNSH(E,ED,32)                                               
      DEL = (E-ED(LC))/(ED(LC+1)-ED(LC))                                
      DO 10302 I=1,3                                                    
      CG(I)=AD(LC,I+5)+(AD(LC+1,I+5)-AD(LC,I+5))*DEL                    
10302 IF(CG(I) .LT. 0.) CG(I)=0.                                        
      AT=CG(1)+CG(2)+CG(3)                                              
C   CHOOSE PORTION OF ANGULAR DISTRIBUTION                              
      IF(CG(1)/AT .GE. UNIRND(Y)) GO TO 9310                            
      IF(CG(2)/(CG(2)+CG(3)) .GE. UNIRND(Y)) GO TO 10320                
      GO TO 10330                                                       
C  RECTANGLE (ISOTROPIC)                                                
 9310 COSCM=-1.+2.*UNIRND(Y)                                            
      GO TO 10311                                                       
C  FORWARD TRIANGLE                                                     
10320 X1=AD(LC,3)+(AD(LC+1,3)-AD(LC,3))*DEL                             
      COSCM=X1+(1.-X1)*SQRT(UNIRND(Y))                                  
      GO TO 10311                                                       
C  BACKWARD TRIANGLE                                                    
10330 X1=AD(LC,5)+(AD(LC+1,5)-AD(LC,5))*DEL                             
      COSCM=X1-(X1+1.)*SQRT(UNIRND(Y))                                  
C  CALL THE KINEMATICS SUBROUTINE KINNR.                                
10311 IF(COSCM .GT. 1.) COSCM=1.000000                                  
      IF(COSCM .LT.-1.) COSCM=-1.000000                                 
      CALL KINNR(RM1,RM2,RM1,RM2,E,COSCM,VB,WCM,PCM,COSL,W)             
C  LIGHT OUTPUT FROM CARBON                                             
      ELT=0.017*(E-W)                                                   
      GOTO 9333                                                         
C  DIFFRACTION ELASTIC SCATTERING.  EXPONENTIAL FORWARD PEAK, WITH WIDTH
C  INVERSELY PROPORTIONAL TO THE TOTAL INELASTIC CROSS SECTION, AND     
C  TO THE NEUTRON ENERGY E.                                             
10350 COSCM=1.+ALOG(1.-UNIRND(Y))/1.17/S(7)/E                           
      GO TO 10311                                                       
 9333 CONTINUE                                                          
C   END OF NCEL ROUTINE ----------------------------------------------- 
C  W,COSL, ELT ARE DIFINED UNDER NPNP ABOVE                             
      GO TO 1000                                                        
C  INELASTIC CARBON INTERACTION OCCURRED.  CHOOSE WHICH.                
  300 IF (S(3)/S(7) .LE. UNIRND(Y)) GO TO 400                           
C  N+C--N+C+GAMMA OCCURRED (REACION CHANNEL 3)                          
       MS=3                                                             
C   START OF NCNCGM ROUTINE ------------------------------------------- 
C  SIMULATE THE REACTION N+C--N+C+GAMMA                                 
C  FIND THE GAMMA ANGLE.  THE ANGULAR DISTRIBUTION (TAKEN FROM 14 MEV   
C  DATA) IS GIVEN BY ANGDIS, WHICH IS INVERTED BY RTNI.                 
C  C1,C2,C3, AND V ARE COEFFICIENTS FOR ANGDIS.                         
      V = UNIRND(Y)                                                     
      C1 = .7444                                                        
      C2 = .4342                                                        
      C3 = -.1786                                                       
      CALL RTNI(CSG,F,D,ANGDIS,.5,.001,10,IER)                          
      IF(IER .NE. 0) WRITE(61,9111) IER,CSG,F,D                          
 9111 FORMAT (4H ***,I3,3E12.4)                                         
      IF(UNIRND(Y) .GT. .5) CSG = -CSG                                  
C  FOLLOW THE GAMMA                                                     
C  SCATTR FINDS THD DIRECTION COSINES CG(I) OF THE GAMMA FROM THE       
C  INITIAL NEUTRON DIRECTION COSINES C(I) AND THE LAB COSINE CSG OF THE 
C  GAMMA EMISSION ANGLE AND A UNIFORMLY-DISTRIBUTIED AZIMUTH ANGLE PHI  
      DO 9150 IM = 1,3                                                  
 9150 XG(IM) = X(IM)                                                    
      EMM  = 0.1                                                        
      PHI = 6.2832*UNIRND(Y)                                            
      EGM  = 4.43                                                       
      CALL SCATTR(C,CSG,PHI,CG)                                         
      CALL FOLGAM(XG,CG,EGM,EMM,ELT)                                    
C  DETERMINE THE NEUTRON ANGLE.  THE ANGULAR DISTRIBUTION IS TAKEN FROM 
C  10 MEV PP DATA.                                                      
C  C1,C2,C3, AND V ARE COEFFICIENTS FOR ANGDIS.                         
      C1 = .5357                                                        
      C2 = .8929                                                        
      C3 = -.4826                                                       
      V = UNIRND(Y)                                                     
      CALL RTNI (CSN,F,D,ANGDIS,.5,.001,10,IERR)                        
      IF(IERR .NE. 0) WRITE(61,9111) IERR,CSN,F,D                        
      IF(UNIRND(Y) .GT. .5) CSN = -CSN                                  
C  TRANSFORM NEUTRON TO LAB COORDINATES                                 
C  KINNR IS THE KINEMATICS SUBROUTINE                                   
      CALL KINNR(RM1,RM2,RM1,RM2A,E,CSN,VB,WCM,PCM,COSL,W)              
C   END OF NCNCGM ROUTINE --------------------------------------------  
      GO TO 1000                                                        
  400 SR=S(4)+S(5)+S(6)                                                 
      U=UNIRND(Y)                                                       
      IF (S(4)/SR .LE. U) GO TO 500                                     
C  N+C--ALPHA+9BE    OCCURRED (REACTION CHANNEL 4)                      
      MS=4                                                              
C   START OF NALPHA ROUTINE-------------------------------------------- 
C  SIMULATE THE REACTION N+C--ALPHA+9BE                                 
C  ISOTROPIC TWO-BODY REACTION ASSUMED.                                 
      CNAL = -1.+2.*UNIRND(Y)                                           
      CALL KINNR(RM1,RM2,RM3,RM4,E,CNAL,VB,WCM,PCM,COSL,T)              
      ELT = EEQUIV(T,X,C,COSL,4)                                        
C   END OF NALPHA ROUTINE---------------------------------------------- 
      GO TO 950                                                         
  500 IF((S(4)+S(5))/SR .LE. U) GO TO 600                               
C  N+C--N+THREE ALPHAS OCCURRED (REACTION CHANNEL 5)                    
      MS=5                                                              
C   START OF NN3AL ROUTINE--------------------------------------------- 
C  SIMULATES THE REACTION N+C--N+THREE ALPHAS                           
C  PHASE SPACE ENERGY DISTRIBUTION OF OUTGOING NEUTRON ASSUMED.         
C      DATA EMN, EMC,QQ/940.,11280.,-7.26/                              
C     COMMON/ENG/U                                                      
C     EXTERNAL ENGDIS                                                   
      WCM=E*.923+QQ                                                     
      IF(WCM) 2,2,15004                                                 
    2 W=E                                                               
      COSL=1.                                                           
      GOTO 9500                                                         
C  MAXIMUM NEUTRON ENERGY IN CM                                         
15004 TM=.923*WCM                                                       
C  SIMULATE PHASE SPACE DISTRIBUTION FOR OUTGOING NEUTRON               
      U=UNIRND(Y)                                                       
      X3=.01                                                            
      IF(U .LE. .01) GO TO 3                                            
      CALL RTNI(X3,FN,DV,ENGDIS,.5,.001,20,IER)                         
      IF(IER .NE. 0) WRITE (61,15001) IER,X3,FN,DV                       
15001 FORMAT ( 8HERRNN3AL,I3,3E12.4)                                    
    3 T=TM*X3                                                           
      IF(T .LE. 0.) T=0.                                                
C  FIND THE LAB ENERGY EN AND COSINE COSL OF THE LAB ASCATTERING ANGLE  
C  FOR THE OUTGOING NEUTRON                                             
      PCM=SQRT(2.*RM1*T)                                                
      VB=SQRT(2.*RM1*E)/(RM1+RM2)                                       
C  ISOTROPIC CM ANGULAR DISTRIBUTION OF THE NEUTRON IS ASSUMED.         
      COSCM=-1.+2.*UNIRND(Y)                                            
      W=T+PCM*VB*COSCM+.5*RM1*VB**2                                     
      IF(W .LE. 0.) W=0.                                                
      PN=SQRT(2.*RM1*W)                                                 
      COSL=(PCM*COSCM+RM1*VB)/PN                                        
      TR=E-W+QQ                                                         
      DO 15 I=1,2                                                       
      TA=TR*UNIRND(Y)                                                   
      ELT = EEQUIV(TA,X,C,UNIRND(Y),4) + ELT                            
   15 TR=TR-TA                                                          
      ELT = EEQUIV(TR,X,C,UNIRND(Y),4) + ELT                            
 9500 CONTINUE                                                          
C   END OF NN3AL ROUTINE----------------------------------------------- 
      GO TO 1000                                                        
C  N+C--P+12B  OCCURRED (REACTION CHANNEL 6)                            
  600 MS=6                                                              
C                                                                       
C   START OF NPB ROUTINE----------------------------------------------- 
C     WE DON'T BOTHER WITH RESCATTERED NEUTRONS OR N2N IN SECNUT        
C     DATA ABOVE                                                        
      CALL KINNR(RM1,RM2,RM5,RM6,E,SQRT(UNIRND(Y)),VB,WCM,PCM,COSLP,E)  
      W = E*UNIRND(Y)                                                   
      ELT = EEQUIV(W,X,C,COSLP,1)                                       
C    APPROX. PCT N+C=N+P+B FOR RESCATTERED NEUTRON                      
      IF(UNIRND(Y).LE.0.5) GOTO 9220                                    
      W = E-W                                                           
C   GIVE NEUTRON ENERGY NOT GIVEN TO PROTON                             
      GOTO 1000                                                         
C   EEQUIV RETURNS LIGHT OUTPUT FOR PROTON OF ENERGY EN                 
C  (NO NEUTRON RELEASED)                                                
 9220 CONTINUE                                                          
C   END NPB ROUTINE---------------------------------------------------- 
  950 W=0.                                                              
      COSL=1.                                                           
C  INCREMENT THE ELEMENT MS=1 TO 6 OF THE LIGHT OUTPUT ARRAY ELT        
C  CORRESPONDING TO THE CHANNEL OF INTERACTION, AND ELT(7),THE TOTAL    
C  LIGHT OUTPUT                                                         
 1000 EL(MS)=EL(MS)+ELT                                                 
      EL(7)=EL(7)+ELT                                                   
      E=W                                                               
      PHI=6.2832*UNIRND(Y)                                              
      CALL SCATTR(C,COSL,PHI,C)                                         
      IF(NSW(1).LT.2) GOTO 1110                                         
      IF(NS-2) 800,900,1100                                             
  800 MC=MS                                                             
      DO 802 I=1,5                                                      
  802 NF(I)=1                                                           
  900 BIAS=.125                                                         
      DO 910 I=1,5                                                      
      BIAS=BIAS*2.                                                      
      IF(NF(I))910,910,902                                              
  902 IF (ELT-BIAS) 910,905,905                                         
  905 NFS(MC,I,NS)=NFS(MC,I,NS)+1                                       
      NF(I)=0                                                           
  910 CONTINUE                                                          
 1100 IF (ICOUNT  .EQ. 1) GO TO 1110                                    
C  CHECK WHETHER TIMING BIAS IS EXCEEDED.  IF SO, SET FLAG AND INCREMENT
C  ARRAYS.                                                              
      IF(ELT-TBIAS) 1110,1105,1105                                      
 1105 ICOUNT=1                                                          
      JT=TIME/DTIM+1.                                                   
      IF(JT .GT. 100) JT=100                                            
      IF (JT .LT. 1) JT=1                                               
      JP=(X(1)-X0)/DPOS+50.                                             
      IF(JP .GT. 100) JP=100                                            
      IF(JP .LT. 1) JP=1                                                
      NTIM(JT)=NTIM(JT)+1                                               
      NPOS(JP)=NPOS(JP)+1                                               
C  PRINT DETAILED INFORMATION FOR THIS EVENT                            
 1110 IF (NDUMP .GE. MDUMP) GO TO 2000                                  
      WRITE (61,1) E,EL(L),MS,ELT,(X(I),I=1,3), (C(I),I=1,3),COSL,PHI    
     1 ,TIME                                                            
    1 FORMAT(F6.1,3X,F6.2,I3,F6.2,3X,3F6.2,3X,3F6.2,3X,3F7.4)           
      GO TO 2000                                                        
      END                                                               
      FUNCTION INBNDS(X)                                                
C  RETURNS 1 OR 0 ACCORDING TO WHETHER POSITION VECTOR X IS             
C  INSIDE OR OUTSIDE COUNTER BOUNDARIES                                 
      DIMENSION X(3)                                                    
      COMMON /SCIN/ DHYD,DCARB,IGEO,X0,Y0,Z0                            
      INBNDS=1                                                          
      IF(X(3))9,2,2                                                     
    2 IF(X(3)-Z0) 3,3,9                                                 
    3 IF(IGEO-1) 32,34,34                                               
C  BOX                                                                  
   32 IF(ABS(X(1))-X0) 33,33,9                                          
   33 IF(ABS(X(2))-Y0) 10,10,9                                          
C  CYLINDER                                                             
   34 R=SQRT(X(1)**2+X(2)**2)                                           
      IF(R-X0) 10,10,9                                                  
    9 INBNDS=0                                                          
   10 RETURN                                                            
      END                                                               
      SUBROUTINE FOLGAM(X,C,E,EMIN,ELIGHT)                              
C  MONTE CARLO GAMMA RAY TRACER                                         
C  GIVEN A GAMMA-RAY OF ENERGY E AT POSITION X(I),MOVING ALONG DIRECTION
C  COSINES C(I), FOLGAM FOLLOWS THE GAMMA THROUGH SUCCESSIVE COMPTON    
C  COLLISIONS UNTIL IT ESCAPES FROM THE SCINTILLATOR OR ITS ENERGY FALLS
C  BELOW EMIN. FOLGAM RETURNS ELIGHT, SUM OF ELECTRON RECOIL ENERGIES.  
      DIMENSION X(3),C(3)                                               
      COMMON /SCIN/ DHYD,DCARB,IGEO,XB,YB,ZB                            
      ELIGHT=0.                                                         
      NSMAX=10                                                          
      NS=0                                                              
C  CMPSCT(0,E,SIGT,Y,Y,Y) RETURNS THE TOTAL COMPTON SCATTERING CROSS    
C  SECTION SIGT (BARNS) PER ELECTRON FOR A GAMMA OF ENERGY E.  Y IS A   
C  DUMMY. DHYD AND 6.*DCARB ARE THE NUMBER OF HYDROGEN AND CARBON ELEC- 
C  TRONS PER BARN-INCH                                                  
 1000 CALL CMPSCT(0,E,SIGT,Y,Y,Y)                                       
      PATH=1./(SIGT*(DHYD+6.*DCARB))                                    
C  PROPAGATE GAMMA A DISTANCE D TO NEXT SCATTER                         
      D=-PATH*ALOG(UNIRND(Y))                                           
      DO 20 I=1,3                                                       
   20 X(I)=X(I)+C(I)*D                                                  
C  IS GAMMA STILL IN SCINTILLATOR?                                      
      IF(INBNDS(X) .LE. 0) RETURN                                       
      NS=NS+1                                                           
      IF(NS .GT. NSMAX) RETURN                                          
C  SIMULATE A COMPTON COLLISION.                                        
      CALL CMPSCT(1,E,SIGT,EEL,E,COSGAM)                                
      ELIGHT=ELIGHT+EEL                                                 
      IF(E .LE. EMIN) RETURN                                            
      PHI=6.2832*UNIRND(Y)                                              
C  FIND NEW DIRECTION COSINES                                           
      CALL SCATTR(C,COSGAM,PHI,C)                                       
      GO TO 1000                                                        
      END                                                               
      SUBROUTINE CMPSCT(NTRY,EGAM,SIGT,EEL,EGAMP,COSGAM)                
C  COMPUTE TOTAL CROSS SECTION                                          
      EPS=.001                                                          
      G=EGAM/.5110                                                      
      G4=(G*G)**2                                                       
      A=1.+2.*G                                                         
      TM=2.*G*G/A                                                       
      B=(1.+G)/G**2                                                     
      S=B*(2.*(1.+G)/A-ALOG(A)/G)+ALOG(A)*.5/G-(1.+3.*G)/A**2           
      SIGT=0.4989*S                                                     
      IF(NTRY .EQ. 0) RETURN                                            
      U=UNIRND(X)                                                       
      C=0.                                                              
      D=TM                                                              
    6 D=.5*D                                                            
      E=C+D                                                             
      F=G/(G-E)                                                         
      FL=ALOG(F)/G                                                      
      P=(B*(B*.5*E-FL)+.5*FL+.5*E*(F-E*G*.5)/G4)/S                      
      IF(ABS(U-P)-EPS) 20,20,10                                         
   10 IF(U-P) 6,20,12                                                   
   12 C=E                                                               
      GO TO 6                                                           
   20 EEL=0.5110*E                                                      
      EGAMP=EGAM-EEL                                                    
      COSGAM=1.-(EGAM/EGAMP-1.)/G                                       
      IF (COSCAM .GT.  1.0) COSGAM=1.0000000                            
      RETURN                                                            
      END                                                               
      SUBROUTINE KINNR(EM1,EM2,EM3,EM4,T1,CSCM,VB,WCM,PCM,CSL,T3)       
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C                                                                       
C      RELATIVISTIC KINEMATICS FOR TWO BODY SCATTERING PROBLEM          
C                                                                       
C      ADAPTED FROM J.B. MARION AND J.L. FOWLER                         
C      'FAST NEUTRON PHYSICS,PART 1', PAGES 51-55, VOL 4 OF             
C      INTERSCIENCE MONOGRAPHS AND TEXTS IN PHY.+ASTRO.,1960 NY.NY.     
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
      P1 = SQRT(T1*(T1+2.*EM1))                                         
      W1 = T1+EM1                                                       
      WCM = .5*(EM1*EM1+EM2*EM2+EM3*EM3-EM4*EM4+2.*EM2*W1)              
      IF(WCM.LE.0.0)  GOTO 10                                           
      WCM = WCM/SQRT(EM1*EM1+EM2*EM2+2.*EM2*W1)                         
      IF(WCM.LE.EM3)  GOTO 10                                           
      PCM = SQRT(WCM*WCM-EM3*EM3)                                       
      B0 = P1/(W1+EM2)                                                  
      G0 = 1./SQRT(1.-B0*B0)                                            
      T3 = G0*(WCM+B0*PCM*CSCM)-EM3                                     
      IF(T3.LT.0.001) T3 = 0.001                                        
      CSL = G0*(PCM*CSCM+B0*WCM)/SQRT(T3*(T3+2.*EM3))                   
      IF(CSL.GT.1.) CSL = 1.00000                                       
      VB = PCM/(G0*EM3)                                                 
      RETURN                                                            
   10 WCM = 0.                                                          
      PCM = 0.                                                          
      T3 =0.                                                            
      VB = 0.                                                           
      CSL = 1.00000                                                     
      RETURN                                                            
      END                                                               
      SUBROUTINE ANGDIS(X,F,DF)                                         
C  F+V IS THE INTEGRAL DISTRIBUTION, DF IS ITS DERIVATIVE.              
      COMMON /COEF/ A,B,C,V                                             
      XS=X**2                                                           
      F=X*(A+XS*(B+C*XS))-V                                             
      DF=A+XS*(3.*B+XS*5.*C)                                            
      IF(DF .LT. 0.0001 ) DF=.0001                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE ENGDIS(X,F,DF)                                         
C  PHASE SPACE ENERGY DISTRIBUTION FOR USE WITH THE NEWTONS             
C  METHOD SUBROUTINE RTINI                                              
      COMMON /ENG/ U                                                    
      IF(X .LE. .01) GO TO 2                                            
      XR=SQRT(X)*6.562                                                  
      XS=X*X                                                            
      DF=XR*(1.-X)**2                                                   
      F=XR*X*(.6667-.8*X+.2857*XS)-U                                    
      RETURN                                                            
    2 DF=.6431                                                          
      F=.01+DF*(X-.01)-U                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE SCATTR(C,COSTH,PHI,CNEW)                               
      DIMENSION C(3), CNEW(3)                                           
       SI(X)=SQRT(1.-X**2)                                              
C  PROTECT THE ANGLES                                                   
      CT=C(3)                                                           
      IF(CT-1.)6,4,2                                                    
    2 CT=1.0000000                                                      
    4 ST=0.0000000                                                      
      SP=0.0000000                                                      
      CP=1.0000000                                                      
      GO TO 30                                                          
    6 IF(-CT-1.)20,4,8                                                  
    8 CT=-1.0000000                                                     
      GO TO 4                                                           
   20 ST=SI(CT)                                                         
      CP=C(1)/ST                                                        
      IF(CP .GT. 1.) CP=1.0000000                                       
      IF(CP .LT. -1.) CP=-1.0000000                                     
      SP=C(2)/ST                                                        
      IF(SP .GT. 1.) SP=1.0000000                                       
      IF(SP .LT. -1.) SP=-1.0000000                                     
   30 IF(COSTH .GT. 1.) COSTH=1.0000000                                 
      IF(COSTH .LT. -1.) COSTH=-1.0000000                               
      S=SI(COSTH)                                                       
      CXP=S*COS(PHI)                                                    
      CYP=S*SIN(PHI)                                                    
      CZP=COSTH                                                         
      CNEW(1)=CXP*CT*CP-CYP*SP+CZP*ST*CP                                
      CNEW(2)=CXP*CT*SP+CYP*CP+CZP*ST*SP                                
      CNEW(3)=-CXP*ST+CZP*CT                                            
      RETURN                                                            
      END                                                               
      FUNCTION EEQUIV(E,X,C,COSLP,L)                                    
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C                                                                       
C      THIS FUNCTION COMPUTES THE ELECTRON EQUIVALENTS OF               
C      LIGHT DEPOSITED IN THE SCINTILLATOR,WHEN A PARTICLE              
C      OF THE TYPE 'L' (1=PROTON, 4=ALPHA) WITH ENERGY E                
C      SCATTERS FROM COORDINATES X(I) WITH DIRCTION COSINES C(I)        
C      AND SCATTERING ANGLE COSLP                                       
C                                                                       
C      THE SCINTILLATOR RESPONSE FUNCTION IS                            
C                                    ELM(ENERGY,PAR1,PAR2,PAR3,PAR4)    
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
      DIMENSION C(3),CN(3),X(3)                                         
      COMMON /SCIN/ DHYD,DCARB,IGEO,X0,Y0,Z0                            
      COMMON /SCINP/ A,B,R,S,T,V                                        
      COMMON /SCINA/ G,H,O,P,Q,W                                        
      ELM(Y,A,B,R,S) = A*(1.-EXP(B*Y**R))+S*Y                           
      ELT = 0                                                           
      IF(E.LT.0.02) GOTO 300                                            
      IF(E.LE.0.2)  GOTO 100                                            
      U = 6.2832*UNIRND(Y)                                              
      CALL SCATTR(C,COSLP,U,CN)                                         
C    FIND DISTANCE DSM TO WALL                                          
      IF(CN(3)) 3,4,5                                                   
    3 DSM = -X(3)/CN(3)                                                 
      GOTO 6                                                            
    4 DSM = Z0*1000.                                                    
      GOTO 6                                                            
    5 DSM = (Z0-X(3))/CN(3)                                             
    6 IF(IGEO-1) 10,20,20                                               
C    FOR RECTANGULAR SCINTILLATOR THE DISTANCE TO WALL IS SIMPLE        
   10 IF( CN(1).EQ.0. ) GOTO 12                                         
      D = ABS((X0-(ABS(CN(1))/CN(1))*X(1))/CN(1))                       
      IF(D.LT.DSM) DSM=D                                                
   12 IF( CN(2).EQ.0. ) GOTO 32                                         
      D = ABS((Y0-(ABS(CN(2))/CN(2))*X(2))/CN(2))                       
      GOTO 30                                                           
C    FOR CYLINDER WE FIND THE DISTANCE A HARDER WAY                     
   20 IF( (CN(1)+CN(2)).EQ.0. ) GOTO 32                                 
      AQ = CN(1)*CN(1)+CN(2)*CN(2)                                      
      BQ = 2.*(X(1)*CN(1)+X(2)*CN(2))                                   
      CQ = X(1)*X(1)+X(2)*X(2)-X0*X0                                    
      D = (-BQ+SQRT(BQ*BQ-4.*AQ*CQ))/(2.*AQ)                            
   30 IF(D.LT.DSM) DSM = D                                              
   32 D = E                                                             
      IF(L.EQ.4) D = D*0.25                                             
      D = ALOG(D)                                                       
C    NEXT TWO LINES FIND RANGE OF PARTICLE IN SCINTILLATOR:             
      F = (((3.1471E-3-2.321E-4*D)*D-0.020364)*D+0.0819283)*D           
      D = EXP((F+1.61711)*D-3.8103)/25.4                                
C    IF PARTICLE WONT ESCAPE SCINT  I.E. D.LE.DSM CALCULATION IS        
C    SIMPLIFIED TO JUST PLUGGING ENERGY INTO ELM(E, , , , , )           
      IF(D.LE.DSM) GOTO 100                                             
C    RANGE IS TO BIG SO WE MUST CALCULATE ENERGY LOST IN                
C    TRAVELING A DISTANCE D-DSM OUTSIDE OF SCINT                        
      D = ALOG(25.4*(D-DSM))                                            
      F = (((D*2.742E-5-1.8209E-4)*D-8.88849E-5)*D+1.00554E-3)*D        
      EEXT = EXP((F+.561839)*D+2.1964)                                  
C    EEXT=ENERGY FOR PRO OR ALPHA OUTSIDE SCINT                         
      IF(EEXT.LE.0.02) GOTO 100                                         
      IF(L.EQ.4) GOTO 150                                               
      ELT = ELM(EEXT,A,B,R,S)                                           
  100 IF(L.EQ.4) GOTO 400                                               
      ELT = ELM(E,A,B,R,S)-ELT                                          
      GOTO 300                                                          
  150 ELT = ELM(4.0*EEXT,G,H,O,P)                                       
  400 ELT = ELM(E,G,H,O,P)-ELT                                          
  300 IF(ELT.LE.0.) ELT = 0.                                            
      EEQUIV = ELT                                                      
      RETURN                                                            
      END                                                               
      FUNCTION IBNSH(A,B,N)                                             
C  BINARY SEARCH FOR AN ELEMENT L OF ARRAY B SUCH THAT                  
C  A .GE. B(I) AND A .LT. B(I+1).                                       
C  NOTE-DIMENSION N OF B MUST BE A POWER OF 2.                          
      DIMENSION B(2)                                                    
    4 J=0                                                               
      K=N                                                               
    6 K=K/2                                                             
      L=K+J                                                             
      IF(A-B(L))6,20,10                                                 
   10 IF(A-B(L+1)) 20,12,12                                             
   12 J=L                                                               
      GO TO 6                                                           
   20 IBNSH=L                                                           
      RETURN                                                            
      END                                                               
      SUBROUTINE RTNI(X,F,DERF,FCT,XST,EPS,IEND,IER)                    
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C     RTNI SOLVES NONLINEAR EQUATIONS OF THE FORM F(X)=0                
C     FOR THEIR ROOTS BY NEWTONS METHOD                                 
C                                                                       
C     FCT IS THE EXTERNAL FUNCTION TO BE SOLVED                         
C                                                                       
C     X     =  ROOT                                                     
C     F     =  FUNCTION VALUE AT X                                      
C     DERF  =  VAL OF DERIVATIVE AT X                                   
C     XST   =  INITIAL GUESS OF X                                       
C     EPS   =  ERROR ALLOWED IN ROOT                                    
C     IEND  =  MAX ITERATIONS ALLOWED                                   
C     IER   =  ERROR PARAM , IER=0 THEN NO ERROR                        
C                                                                       
C-----------------------------------------------------------------------
C                                                                       
C  PREPARE ITERATION                                                    
      IER=0                                                             
      X=XST                                                             
      TOL=X                                                             
      CALL FCT(TOL,F,DERF)                                              
      TOLF=100.*EPS                                                     
C  START ITERATION LOOP                                                 
      DO 6 I=1,IEND                                                     
      IF(F)1,7,1                                                        
C  EQUATION IS NOT SATISFIED BY X                                       
    1 IF(DERF)2,8,2                                                     
C  ITERATION IS POSSIBLE                                                
    2 DX=F/DERF                                                         
      X=X-DX                                                            
      TOL=X                                                             
      CALL FCT(TOL,F,DERF)                                              
C  TEST ON SATISFACTORY ACCURACY                                        
      TOL=EPS                                                           
      A=ABS(X)                                                          
      IF(A-1.)4,4,3                                                     
    3 TOL=TOL*A                                                         
    4 IF(ABS(DX)-TOL)5,5,6                                              
    5 IF(ABS(F)-TOLF)7,7,6                                              
    6 CONTINUE                                                          
C  END OF ITERATION LOOP                                                
C  NO CONVERGENCE AFTER IEND ITERATION STEPS.  ERROR RETURN.            
      IER=1                                                             
    7 RETURN                                                            
C  ERROR RETURN IN CASE OF ZERO DIVISOR                                 
    8 IER=2                                                             
      RETURN                                                            
      END                                                               
      FUNCTION UNIRND(X)                                                
C                                                                       
C                                                                       
      COMMON /RANGEN/ KSEED                                             
      temp=rand(0)
      unirnd=amax1(10.e-10,temp)
      RETURN                                                            
      END                                                               
