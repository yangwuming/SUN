C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     SURFOPAC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE SURFOPAC(XENV)
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT LOGICAL*4(L)
      COMMON /NEWOPAC/ZLAOL1,ZLAOL2,ZOPAL1,ZOPAL2, ZOPAL951,
     +       ZOPAL952, ZALEX1, ZALEX2, ZKUR1, ZKUR2,  
     +       LLAOL89,LOPAL92,LOPAL95,LKUR90,LALEX95,L2Z 
C
C     THIS SUBROUTINE SETS UP SURF OPACITY TABLES
C     ASSUMES TABLES HAVE ALREADY BEEN READ IN
C
C     INTERIOR TABLES
C
C     SETUP OPAL95 TABLES AT ZOPAL951 AND ZOPAL952
      IF (LOPAL95) THEN
         CALL LL4TH95(XENV)
         CALL LL4TH951(XENV)
         CALL LL4TH952(XENV)
         CALL LL4TH953(XENV)
         CALL LL4TH954(XENV)
         CALL LL4TH955(XENV)
      END IF
C     SETUP IN OPAL92 TABLES AT ZOPAL1 AND ZOPAL2
      IF (LOPAL92) THEN
 	 CALL LL4TH(XENV)
      END IF
C
C     LOW TEMP TABLES
C
C     READ IN ALEX TABLES AT ZALEX1 AND ZALEX2
      IF (LALEX95) THEN
 	CALL ALX8TH(XENV)
      END IF
      RETURN
      END
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     SETUPOPAC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE SETUPOPAC(XENV, V)
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT LOGICAL*4(L)
      REAL*4 OLAOL,OXA,OT,ORHO,TOLLAOL
      CHARACTER*80 FLAOL, FPUREZ
      CHARACTER *80 FLAOL2, FOPAL2
      COMMON /NEWOPAC/ZLAOL1,ZLAOL2,ZOPAL1,ZOPAL2, ZOPAL951,
     +       ZOPAL952, ZALEX1, ZALEX2, ZKUR1, ZKUR2,  
     +       LLAOL89,LOPAL92,LOPAL95,LKUR90,LALEX95,L2Z 
      COMMON/ZRAMP/RSCLZC(50), RSCLZM1(50), RSCLZM2(50),
     *             ZOP2, IOLAOL2, IOOPAL2, NK,
     *             LZRAMP, FLAOL2, FOPAL2
      COMMON/GRAVS3/FGRY,FGRZ,LTHOUL,LDIFZ
      COMMON/NWLAOL/OLAOL(12,104,52),OXA(12),OT(52),ORHO(104),TOLLAOL,
     *  IOLAOL, NUMOFXYZ, NUMRHO, NUMT, LLAOL, LPUREZ, IOPUREZ,
     *   FLAOL, FPUREZ
      DIMENSION V(12)
C
C     THIS SUBROUTINE READS IN SPECIFIED OPACITY TABLES AND
C     SET UP SPLINES FOR THE TABLES.
C     WHEN LZRAMP=T OR LDIFZ=T THEN READ IN SECOND SET OF
C     OPACITY TABLES AT DIFFERENT Z (E.G. ZOPAL952).
      L2Z = LZRAMP.OR.LDIFZ
C
C     INTERIOR TABLES
C
C     READ IN OPAL95 TABLES AT ZOPAL951 AND ZOPAL952
      IF (LOPAL95) THEN
         CALL LL95TBL
         CALL LL95TBL1
         CALL LL95TBL2
         CALL LL95TBL3
         CALL LL95TBL4
         CALL LL95TBL5
         CALL LL4TH95(XENV)
         CALL LL4TH951(XENV)
         CALL LL4TH952(XENV)
         CALL LL4TH953(XENV)
         CALL LL4TH954(XENV)
         CALL LL4TH955(XENV)
      END IF
C     READ IN OPAL92 TABLES AT ZOPAL1 AND ZOPAL2
      IF (LOPAL92) THEN
         CALL SETLLO
	 CALL LL4TH(XENV)
      END IF
C     READ IN LAOL89 TABLES AT ZLAOL1 AND ZLAOL2
      IF (LLAOL89) THEN
         CALL RDLAOL(V)
	 CALL SULAOL
      END IF
C
C     READ IN LAOL89 PURE Z TABLE 
C
      IF(LPUREZ)THEN
c         CALL RDZLAOL
c	 CALL ZSULAOL
       stop 'not supported.'
      END IF
C
C     LOW TEMP TABLES
C
C     READ IN ALEX TABLES AT ZALEX1 AND ZALEX2
      IF (LALEX95) THEN
        CALL ALXTBL
	CALL ALX8TH(XENV)
      END IF
C     READ IN KURUCZ TABLE AT ZKUR1 AND ZKUR2
      IF (LKUR90) THEN
          CALL SETKRZ
      END IF
      RETURN
      END
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     GETOPAC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
      SUBROUTINE GETOPAC(DL,TL,X,Z,O,OL,QOD,QOT)
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT LOGICAL*4(L)
      REAL*4 OLAOL,OXA,OT,ORHO,TOLLAOL
      CHARACTER*80 FLAOL, FPUREZ
      COMMON /NEWOPAC/ZLAOL1,ZLAOL2,ZOPAL1,ZOPAL2, ZOPAL951,
     +       ZOPAL952, ZALEX1, ZALEX2, ZKUR1, ZKUR2,  
     +       LLAOL89,LOPAL92,LOPAL95,LKUR90,LALEX95,L2Z 
      COMMON/COMP/XENV,ZENV,ZENVM,AMUENV,FXENV(12),XNEW,ZNEW,STOTAL,SENV
      COMMON/LUNUM/IOWR, ILAST, IFIRST, IRUN, ISTAND, IFERMI,
     1    ICAPPA, IOPAC, IDEBUG, ITRACK, ISHORT, IMILNE, IMODPT,
     2    ISTOR, IOPMOD, IOPENV, IOPATM, ILSTBN, ISTOBN, IDYN,
     3    IUBDAT, ILLDAT, ISNU, ISCOMP, ICOX, IKUR, IFSTBN
      COMMON/NWLAOL/OLAOL(12,104,52),OXA(12),OT(52),ORHO(104),TOLLAOL,
     *  IOLAOL, NUMOFXYZ, NUMRHO, NUMT, LLAOL, LPUREZ, IOPUREZ,
     *   FLAOL, FPUREZ
      COMMON/OPTAB/OPTOL,ZSI,IDT,IDD(4)
      common/zpoint/zp(11),zpa(11)      
C
C     THIS SUBROUTINE CALCULATES THE OPACITY FOR A GIVEN X AND Z.
C     IF LDIFZ=T OR LZRAMP=T THEN INTERPOLATE BETWEEN TWO Z TABLES.
C     IN A SMALL T RANGE THE ATMOSPHERE AND INTERIOR OPACITY ARE
C     RAMPED FROM ONE TO THE OTHER.
C
C     
C     GET ATMOSPHERE OPACITY
C
      LGOTATM = .FALSE.
      if(tl.ge.4.d0)then 
C     GET INTERIOR OPACITY IF NEEDED
C
c      IF ((TL .LE. 4.0D0).AND.LGOTATM) GOTO 1000
C
C     HELIUM BURNING REGION (HB EVOLUTION) USE PURE Z TABLE
C
      IF((Z .GT. 0.15D0) .OR. 
     *     ((ABS(Z-ZENV) .GT. OPTOL).AND..NOT.L2Z)) THEN
       IF(.NOT.LPUREZ) THEN
	    WRITE(ISHORT, *)' ERROR: Z.NE.ZENV. NEED PURE Z',
     *      ' TABLE TO CONTINUE. Z,ZENV=',Z, ZENV
          STOP
       END IF
       CALL GTPURZ(DL,TL,OZ,OLZ,QODZ,QOTZ)
         IF (LOPAL95) THEN
	     CALL YLL3D95(DL,TL,X,O,OL,QOD,QOT)
	     ZIT=ZOPAL951
         ELSE IF (LOPAL92) THEN
	     CALL YLLO3D(DL,TL,X,O,OL,QOD,QOT)
	     ZIT=ZOPAL1
         ELSE IF (LLAOL89) THEN
	     CALL GTLAOL(DL,TL,X,O,OL,QOD,QOT)
	     ZIT=ZLAOL1
         END IF
	 SLOPE = (OL-OLZ)/(ZIT-1.0D0)
	 OL = OLZ + (Z-1.0D0)*SLOPE
	 O = 10.0D0**OL
	 SLOPE = (QOD-QODZ)/(ZIT-1.0D0)
	 QOD = QODZ + (Z-1.0D0)*SLOPE 
	 SLOPE = (QOT-QOTZ)/(ZIT-1.0D0)
	 QOT = QOTZ + (Z-1.0D0)*SLOPE 
      ELSE
C
C     NOT HELIUM BURNING REGION (HB EVOLUTION) OR L2Z=T AND
C     Z STILL NOT TOO LARGE IN CORE (<.15) SO CAN USE
C     SECOND Z TABLE RATHER THAN PURE Z TABLE
C
        IF (LOPAL95) THEN
         if(z.lt.zp(2))then
             kz=1
             goto 42
           else if(z.ge.zp(10))then
             kz=10
             goto 42
         endif
         do jz=2,9
           if(z.ge.zp(jz).and.z.lt.zp(jz+1))then
             kz=jz
             goto 42
           endif
         enddo
         kz=1
  42     continue 
C This is stupid, but it does work. The right way should be like
C Lalex95 way, but that doesn't work. 
         if(kz.eq.1)then
           CALL YLL3D95(DL,TL,X,O,OL,QOD,QOT)
           if(L2Z)CALL YLL3D951(DL,TL,X,O1,OL1,QOD1,QOT1)
         else if(kz.eq.2)then 
           CALL YLL3D951(DL,TL,X,O,OL,QOD,QOT)
           if(L2Z)CALL YLL3D952(DL,TL,X,O1,OL1,QOD1,QOT1)
         else if(kz.eq.3)then 
           CALL YLL3D952(DL,TL,X,O,OL,QOD,QOT)
           if(L2Z)CALL YLL3D953(DL,TL,X,O1,OL1,QOD1,QOT1)
         else if(kz.eq.4)then 
           CALL YLL3D953(DL,TL,X,O,OL,QOD,QOT)
           if(L2Z)CALL YLL3D954(DL,TL,X,O1,OL1,QOD1,QOT1)
         else if(kz.eq.5)then 
           CALL YLL3D954(DL,TL,X,O,OL,QOD,QOT)
           if(L2Z)CALL YLL3D955(DL,TL,X,O1,OL1,QOD1,QOT1)
         else if(kz.eq.6)then 
           CALL YLL3D955(DL,TL,X,O,OL,QOD,QOT)
           if(L2Z)CALL YLL3D956(DL,TL,X,O1,OL1,QOD1,QOT1)
         else if(kz.eq.7)then 
           CALL YLL3D956(DL,TL,X,O,OL,QOD,QOT)
           if(L2Z)CALL YLL3D957(DL,TL,X,O1,OL1,QOD1,QOT1)
         else if(kz.eq.8)then 
           CALL YLL3D957(DL,TL,X,O,OL,QOD,QOT)
           if(L2Z)CALL YLL3D958(DL,TL,X,O1,OL1,QOD1,QOT1)
         else if(kz.eq.9)then 
           CALL YLL3D958(DL,TL,X,O,OL,QOD,QOT)
           if(L2Z)CALL YLL3D959(DL,TL,X,O1,OL1,QOD1,QOT1)
         else 
           CALL YLL3D959(DL,TL,X,O,OL,QOD,QOT)
           if(L2Z)CALL YLL3D950(DL,TL,X,O1,OL1,QOD1,QOT1)        
         endif
CCC test         
         lenop=.false.
         if(lenop)then
           if(tl.gt.6.7 ) then ! 6.338 6.334
             o=o*1.015
             o1=o1*1.015
             oL=ol+log10(1.015 )
             oL1=ol1+log10(1.015 )
           endif                
           if(tl.lt.6.70.and.tl.gt.6.68 ) then ! 6.338 6.334
             o=o*1.013
             o1=o1*1.013
             oL=ol+log10(1.013 ) 
             oL1=ol1+log10(1.013 )
           endif    
           if(tl.lt.6.68.and.tl.gt.6.66 ) then ! 6.338 6.334
             o=o*1.011
             o1=o1*1.011
             oL=ol+log10(1.011 ) 
             oL1=ol1+log10(1.011 )
           endif                                      
           if(tl.lt.6.66.and.tl.gt.6.64 ) then ! 6.338 6.334
             o=o*1.009
             o1=o1*1.009
             oL=ol+log10(1.009 ) 
             oL1=ol1+log10(1.009 )
           endif      
           if(tl.lt.6.64.and.tl.gt.6.62 ) then ! 6.338 6.334
             o=o*1.007
             o1=o1*1.007
             oL=ol+log10(1.007 ) 
             oL1=ol1+log10(1.007 )
           endif     
           if(tl.lt.6.62.and.tl.gt.6.61 ) then !6.334
             o=o*1.005
             o1=o1*1.005
             oL=ol+log10(1.005 ) 
             oL1=ol1+log10(1.005 )
           endif                                                               
           if(tl.lt.6.61.and.tl.gt.6.6 ) then !6.334
             o=o*1.003
             o1=o1*1.003
             oL=ol+log10(1.003 ) 
             oL1=ol1+log10(1.003 )
           endif 
         endif          
         IF (L2Z) THEN
            detaz=zp(kz)-zp(kz+1)
            SLOPE = (OL-OL1)/detaz
            OL = OL1 + (Z-zp(kz+1))*SLOPE
            O = 10.0D0**OL
            SLOPE = (QOD-QOD1)/detaz
            QOD = QOD1 + (Z-zp(kz+1))*SLOPE 
            SLOPE = (QOT-QOT1)/detaz
            QOT = QOT1 + (Z-zp(kz+1))*SLOPE 
         END IF
        ELSE IF (LOPAL92) THEN
          CALL YLLO3D(DL,TL,X,O,OL,QOD,QOT)
          IF (L2Z) THEN
            CALL YLLO3D2(DL,TL,X,O1,OL1,QOD1,QOT1)
            SLOPE = (OL-OL1)/(ZOPAL1-ZOPAL2)
            OL = OL1 + (Z-ZOPAL2)*SLOPE
            O = 10.0D0**OL
            SLOPE = (QOD-QOD1)/(ZOPAL1-ZOPAL2)
            QOD = QOD1 + (Z-ZOPAL2)*SLOPE 
            SLOPE = (QOT-QOT1)/(ZOPAL1-ZOPAL2)
            QOT = QOT1 + (Z-ZOPAL2)*SLOPE 
          END IF
        ELSE IF (LLAOL89) THEN
          CALL GTLAOL(DL,TL,X,O,OL,QOD,QOT)
          IF (L2Z) THEN
            CALL GTLAOL2(DL,TL,X,O1,OL1,QOD1,QOT1)
            SLOPE = (OL-OL1)/(ZLAOL1-ZLAOL2)
            OL = OL1 + (Z-ZLAOL2)*SLOPE
            O = 10.0D0**OL
            SLOPE = (QOD-QOD1)/(ZLAOL1-ZLAOL2)
            QOD = QOD1 + (Z-ZLAOL2)*SLOPE 
            SLOPE = (QOT-QOT1)/(ZLAOL1-ZLAOL2)
            QOT = QOT1 + (Z-ZLAOL2)*SLOPE 
          END IF
        END IF
      END IF

      else
      IF (LALEX95) THEN
           if(z.lt.zpa(2))then
             kz=1
             goto 41
             else if(z.ge.zpa(10))then
             kz=10
             goto 41
           endif
         do jz=2,9
           if(z.ge.zpa(jz).and.z.lt.zpa(jz+1))then
             kz=jz
c             write(*,*)'kz=',kz,z,zpa(jz),zpa(jz+1)
             goto 41
           endif
         enddo
         kz=1
  41     continue
         CALL YALO3D(DL, TL, X, SO, SOL, SQOD, SQOT,kz)
	   IF(SO.GT.1.0D30) THEN
            SO=1.0D30
            SOL=30.0D0
	   ENDIF
         IF (L2Z) THEN
            CALL YALO3D(DL,TL,X,SO1,SOL1,SQOD1,SQOT1,kz+1)
c            write(*,*)'SO= ',SO,SO1
	      IF(SO1.GT.1.0D30) THEN
                SO1=1.0D30
	        SOL1=30.0D0
	      ENDIF
            delaz=zpa(kz)-zpa(kz+1)
            SLOPE = (SOL-SOL1)/delaz
            SOL = SOL1 + (Z-zpa(kz+1))*SLOPE
            SO = 10.0D0**SOL
            SLOPE = (SQOD-SQOD1)/delaz
            SQOD = SQOD1 + (Z-zpa(kz+1))*SLOPE 
            SLOPE = (SQOT-SQOT1)/delaz
            SQOT = SQOT1 + (Z-zpa(kz+1))*SLOPE 
         END IF
	  LGOTATM = .TRUE.
      ELSE IF (LKUR90) THEN
        CALL KURUCZ(DL, TL, SO, SOL, SQOD, SQOT)
        IF (L2Z) THEN
           CALL KURUCZ2(DL, TL, SO1, SOL1, SQOD1, SQOT1)
           SLOPE = (SOL-SOL1)/(ZKUR1-ZKUR2)
           SOL = SOL1 + (Z-ZKUR2)*SLOPE
           SO = 10.0D0**SOL
           SLOPE = (SQOD-SQOD1)/(ZKUR1-ZKUR2)
           SQOD = SQOD1 + (Z-ZKUR2)*SLOPE 
           SLOPE = (SQOT-SQOT1)/(ZKUR1-ZKUR2)
           SQOT = SQOT1 + (Z-ZKUR2)*SLOPE 
        END IF
        LGOTATM = .TRUE.
      END IF
      endif
c  100 CONTINUE
C

C
c 1000 CONTINUE
C
C     DO A RAMP BETWEEN SURFACE AND INTERIOR OPACITY
C
C      IF (LGOTATM) THEN
C         IF( TL.GE.4.0D0 .AND. TL.LE.4.176D0) THEN
C 	     RMPWT = (TL-4.0D0)/(4.176D0-4.0D0)
C 	     O = RMPWT*O + (1.0-RMPWT)*SO
C 	     OL = DLOG10(O)
C 	     QOD = RMPWT*QOD + (1.0-RMPWT)*SQOD
C 	     QOT = RMPWT*QOT + (1.0-RMPWT)*SQOT
C	 ELSE
C 	     O = SO
C 	     OL = DLOG10(O)
C 	     QOD = SQOD
C 	     QOT = SQOT
C	 END IF
C      END IF
C ALEX LOW OPACITIES GO UP TO T=4.1, RATHER THAN 4.176
C BUT KUR90 DOES GOES UP TO 4.176
C THIS BUG WAS FIXED BY CHRISTINE STRAKA ON 3/7/2005
      IF (LGOTATM) THEN 
         IF(LKUR90) THEN 
            IF( TL.GE.4.0D0 .AND. TL.LE.4.176D0) THEN 
               RMPWT = (TL-4.0D0)/(4.176D0-4.0D0) 
               O = RMPWT*O + (1.0-RMPWT)*SO 
               OL = DLOG10(O) 
               QOD = RMPWT*QOD + (1.0-RMPWT)*SQOD 
               QOT = RMPWT*QOT + (1.0-RMPWT)*SQOT 
CWS            ELSE 
            ELSE IF(TL.LT.4.0D0) THEN 
               O = SO 
               OL = DLOG10(O) 
               QOD = SQOD 
               QOT = SQOT 
            END IF 
         ELSE 
CWS RAMP AVERAGE BUG FIXED WHEN LALEX95=T 
            IF( TL.GE.4.0D0 .AND. TL.LE.4.1D0) THEN 
               RMPWT = (TL-4.0D0)/(4.1D0-4.0D0) 
               O = RMPWT*O + (1.0-RMPWT)*SO 
               OL = DLOG10(O) 
               QOD = RMPWT*QOD + (1.0-RMPWT)*SQOD 
               QOT = RMPWT*QOT + (1.0-RMPWT)*SQOT 
CWS            ELSE 
            ELSE IF(TL.LT.4.0D0) THEN 
               O = SO 
               OL = DLOG10(O) 
               QOD = SQOD 
               QOT = SQOT 
            END IF 
         END IF 
      END IF 
C
C     DO CONDUCTIVE OPACITY CORRECTION
C
      IF(TL.LT.4.2D0) THEN
         RETURN
      ELSE IF(DL.LT.(2.0D0*TL-13.0D0)) THEN
         RETURN
      ELSE
	 COND = DLOG10(1.0D0-0.6D0*X) - 14.6196D0 -
     *          (3.5853D0+0.1386D0*DL)*DL + 
     *          (5.1324D0-0.3219D0*TL)*TL + 0.3901D0*DL*TL
	 OC = 10.0**COND
	 OX = O
	 QODX = QOD
	 QOTX = QOT
	 O = OX*OC/(OX + OC)
	 OL = DLOG10(O)
	 QODC = 0.3901D0*TL - 0.2772D0*DL - 3.5853D0
	 QOTC = 0.3901D0*DL - 0.6438D0*TL + 5.1324D0
	 QOD = (QODX + QODC - (OX*QODX + OC*QODC)/(OX + OC))
	 QOT = (QOTX + QOTC - (OX*QOTX + OC*QOTC)/(OX + OC))
      ENDIF
      RETURN
      END
