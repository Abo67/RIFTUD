      PROGRAM RIFTUD 
      IMPLICIT NONE
!
      real ALPHAA,ALPHA0,ALPHA1,ALPHA2,ALPH0,ALPH1,ALPH1A,ALPH2,ALP0,
     &ALP1A,ALSTG,ALUNC
      real BETA1A,BETA2,BET1A,BET2,BET2M
      real CDT2,CHRD,CN,CP,CPOW,CR,CS
      real DELVX,DHIDS,DHIDSA,DHIDT,DHIDTA,DHSHFT,DHVD,DHVDAV,DOR,
     &DVUAVC,DVUTOT,DVU2AV,D0,D1,D1A,D2,D2M
      real EBARR,EBARS,EL,EN,ENS,ENST,ENT,ER,ES,ES1,ETAS,ETASV,ETASVD,
     &ETAT,ETATV,ETATVD,EX
      real F2
      real G,GAM
      real HR,HS
      INTEGER I,ID,II,IND,IST,ISTT,ITER,IU
      INTEGER K,KK,KP1,K1
      INTEGER M
      INTEGER NSTAR
      real PI,POW,PS0,PS1,PS1A,PS2,PTIN,PTR1A,PTR2,PTR2ID,PT0,PT1,PT1A,
     &PT2
      real Q
      real R,RHOT0,RHOX,RHO0,RHO1,RHO1A,RHO2,RH2RT2,RLOSS,RSTG,RT2R1A,
     &RV1AAV,RV2IM,R0R1A,R1R1A,RV2I2M
      real SIGRV,SIGS,SIGSIN,SS,SSPD,STAR
      real TGJCP,TSPR,TS0,TS1,TS1A,TS2,TTIN,TTPR,TTR1A,TTR2,TT0,TT1,
     &TT1A,TT2,TT2AV
      character*16 TITLE
      real U1A,U1ASQ,U2
      real VCR1,VCR1A,VCR2,VOVCRA,VOVCR0,VOVCR1,VOVCR2,VR0,VR1,VR1A,
     &VUOU1A,VU0,VU1,VU1A,VU2O,VU2,VXVCR,VXVCRP,VX2,V0,V1,V1A,V1IDSQ,
     &V2,V2LOSS
      real W,WCALC,WCR1A,WCR2,WGIV,WOWCRA,WOWCRM,WOWCR2,WU1A,WU2,W1A,
     &W2,W2IDSQ,W2TOT
      real Z1,Z2,Z3
!
caldo
      integer NMAX
      PARAMETER(NMAX=17)
caldo
      REAL J,N,MU,LCDH,LW,LWX,LC,LCDHIS,LWDHIS,LLDHIS,LSDHIS,LRDHIS 
      DIMENSION D2(NMAX),U2(NMAX),TTR2(NMAX),WCR2(NMAX),PTR2ID(NMAX),
     &VU2(NMAX),WU2(NMAX),DHVD(NMAX),TT2(NMAX),VCR2(NMAX),BET2(NMAX),
     1W2(NMAX),TS2(NMAX),PS2(NMAX),W2IDSQ(NMAX),RHO2(NMAX),F2(NMAX),
     3RV2IM(NMAX),VX2(NMAX),ALPH2(NMAX),V2(NMAX),PT2(NMAX),PTR2(NMAX),
     4VOVCR2(NMAX),WOWCR2(NMAX),DHIDT(NMAX),DHIDS(NMAX),ETATVD(NMAX),
     &ETASVD(NMAX),ALPHA2(NMAX),BETA2(NMAX)
      DIMENSION RV2I2M(NMAX),TITLE(13)
      INTEGER NITER
!
      COMMON/EFF/GAM,VOVCR1,W,PI,STAR,MU,D1,ALPH1,NSTAR,ALPH0,ALUNC,SIGS
     1,D0,HS,CS,WOWCRM,D1A,D2M,HR,BET2M,EN,RH2RT2,CR
      NAMELIST/INPUT/R,GAM,ALPHA0,ALPHA1,N,TTIN,PTIN,W,POW,RV1AAV,R1R1A,
     1R0R1A,RT2R1A,RH2RT2,CDT2,MU,RV2I2M,K,IU,EBARS,EBARR,STAR,NSTAR 
     2,CS,CR,SIGSIN,Ref
!aldo
!aldo when COOLPROP=.false. we should be using the perfect gas law
!     because COOLPROP is invoked, but the PG variables are NOT
!     updated; if STAMPA is also .TRUE. a comparison btw
!     COOLPROP and the PG law is printed on screen
      LOGICAL verbose,COOLPROP,STAMPA,DEBUG
      PARAMETER(verbose=.false.,COOLPROP=.true.,STAMPA=.false.,
     &DEBUG=.false.)
!     PARAMETER(verbose=.false.,COOLPROP=.false.,STAMPA=.false.)
!     PARAMETER(verbose=.false.,COOLPROP=.false.,STAMPA=.true.)
!     PARAMETER(verbose=.false.,COOLPROP=.true.,STAMPA=.false.)
      REAL help,eps
!aldo
Coolprop
      character(LEN=32) Ref, NameR, NameT, NameP, NameH, NameS
      double precision outVal
      REAL HT0,HT1,HS0,HS1,HS1S,S0,S1,HS1A,HT1A,HTR1A,HTR2(NMAX),
     &HT2(NMAX),HS2S,HS2(NMAX),PCRIT,S2(NMAX),HT2AV
      REAL sonic_speed
      integer ipos
Coolprop stuff
      NameR  = "D"//CHAR(0) ! density
      NameT  = "T"//CHAR(0) ! temperature
      NameP  = "P"//CHAR(0) ! pressure
      NameH  = "H"//CHAR(0) ! Mass specific enthalpy
      NameS  = "S"//CHAR(0) ! Mass specific entropy
Coolprop
      EBARS=1.0
      EBARR=1.0
      CS=1.0
      CR=1.0
      SIGSIN= 1.35
      ES=0.0
      RV1AAV=1.0
      CDT2=0.0
      DO 1 I=1,NMAX
         RV2I2M(I)=1.0 
    1 CONTINUE
      IU=2
      ITER=0
      PI=3.14159
      DOR=57.2958 
      R0R1A=0.0
      ALPHA0=999.99 
      WRITE(6,100)
  100 FORMAT(1H1,39X,54HRADIAL INFLOW TURBINE VELOCITY DIAGRAM DESIGN AN
     1ALYSIS)
    2 READ (5,101) TITLE
  101 FORMAT(13A6)
      IF(ITER.GT.0) WRITE(6,1011)
 1011 FORMAT(1H1) 
      WRITE(6,102) TITLE
  102 FORMAT(1H ,13A6) 
      READ (5,INPUT)
      IPOS = LEN_TRIM(Ref)
      Ref    = TRIM(Ref)//CHAR(0)
      WRITE(6,*)'FLUID IS ',Ref(1:IPOS)
      IF (NSTAR.EQ.0) STAR=0.0
      IST=0
      VXVCRP=0.0
      DELVX=0.01
      ALPH0=ALPHA0/DOR
      ALPH1=ALPHA1/DOR
      IF(NSTAR.NE.2) ALSTG=(ALPH0+ALPH1)/2. 
      EN=PI/30.*(110.-ALPHA1)*TAN(ALPH1)
      ENT=AINT(EN) 
      IF(EN-ENT.LT..5) EN=ENT
      IF(EN-ENT.GE..5) EN=ENT+1.
      VUOU1A=1.-2./EN 
      WGIV=W
      KP1=K+1
      WRITE (6,103)
  103 FORMAT(39H THIS OUTPUT IS IN THE FOLLOWING UNITS./
     1       129H TEMPERATURE  PRESSURE    GAS CONST   ROT SPEED    MASS
     2FLOW     POWER     VISCOSITY   VELOCITY    SPEC WORK   DIAMETER
     3   ANGLE)
      GO TO (3,4),IU 
    3 CPOW=1000.
      J=1
      G=1.
      CN=2.
      PT0=PTIN*10000. 
      WRITE (6,104)
  104 FORMAT(3X6HKELVIN6X7HN/SQ CM4X8HJLS/KG-K5X7HRAD/SEC5X6HKG/SEC8X2HK
     1W6X10HN-SEC/S0 N5X5HM/SEC6X6HJLS/GM8X2HCM8X7HDEGREES)
      GO TO 5
    4 CPOW=550.
      J=777.649
      G=32.174
      CN=60./PI
      PT0=PTIN*144.
      WRITE (6,105)
  105 FORMAT(12H DEG RANKINE2X8HLB/SQ IN4X8HBTU/LB-R5X7HREV/MIN5X6HLB/SE
     1C8X2HHP7X9HLB/FT-SEC4X6HFT/SEC6X6HBTU/L88X2HIN8X7HDEGREES)
    5 DHSHFT=CPOW/J*POW/W
      EX=GAM/(GAM-1.) 
      CP=EX*R/J
      TGJCP=2.*G*J*CP
CoolProp begin
caldo compute input total enthalpy, given total temperature and pressure
!     call propssi("ISENTROPIC_EXPANSION_COEFFICIENT"//CHAR(0), NameT, 
!    &DBLE(TTIN), NameP, DBLE(PT0),
!    &Ref, outVal )
!     write(6,*)'k vs. gamma = ',GAM,outVal
      call propssi(NameH, NameT, DBLE(TTIN), NameP, DBLE(PT0),
     &Ref, outVal )
      HT0 = outVal ! total enthalpy in STATION 0
      HT1 = HT0
      IF(STAMPA)write(6,FMT=784)"HT0   ",CP*TTIN/1.e3,HT0/1.e3,
     &(1.-(CP*TTIN)/HT0)*100.
      call propssi(NameS, NameT, DBLE(TTIN), NameP, DBLE(PT0),
     &Ref, outVal )
      S0 = outVal
      IF(STAMPA)write(6,*)'Inlet Entropy is ',S0
CoolProp end
C
C     WRITE INPUT VALUES
C
      WRITE (6,106)TTIN,ALPHA0,EBARS,R,PTIN,ALPHA1,EBARR,GAM,N,STAR, 
     1RV1AAV,MU,W,K,POW,R0R1A
!23456789012345678901234567890123456789012345678901234567890123456789012
  106 FORMAT(8H *INPUT*/13H INLET TEMP =,F10.4,7X,18HSTATOR IN ANGLE  =,
     1F6.2,6X,19HSTATOR KE LOS COEF=,F6.4,5X,14HGAS CONSTANT =F8.4/13H I
     2NLET PRESS=,F10.4,7X,18HSTATOR EX ANGLE  =,F6.2,6X,19HROTOR KE LOS
     3S COEF=,F6.4,5X,14HSPEC HT RATIO=,F6.4/13H ROTAT SPEED=,F10.3,7X,1
     48HSTATOR ASPECT RAT=,F6.4,
     4                     6X,19HROTOR IN/DEL RVU  =,F6.4,5X,14HVISCOSIT
     5Y    =,E10.4/13H MASS FLOW  =,F10.4,7X,11HDIAM RATIOS,19X,19HROTOR
     6 EX RAD SECTS=,I2/13H SHAFT POWER=,F10.3,8X,17HSTAT IN/ROT IN  =, 
     7F6.4,6X,19HROT EX SECT/MN RVU=)
      K1=K
      IF(K.GT.10) K1=10
      WRITE (6,107) (RV2I2M(I),I=1,K1)
  107 FORMAT(1H+,78X,F4.2,9F5.2)
      WRITE (6,108) R1R1A
  108 FORMAT(31X,17HSTAT EX/ROT IN  =,F6.4)
      IF(K.GT.10) WRITE (6,109) (RV2I2M(I),I=11,K)
  109 FORMAT(1H+,77X,5F5.2)
      WRITE (6,110)RT2R1A,RH2RT2,CDT2
!23456789012345678901234567890123456789012345678901234567890123456789012
  110 FORMAT(31X,17HROT EX TP/ROT IN=,F6.4/31X,17HROT EX HUB/TIP  =F6.4,
     16X,19HCL HT/ROT EX TIP D=,F6.4)
      LCDH=CDT2*2./(1.-RH2RT2) ! tip-clearance losses
      LW=0.0 ! disk-friction losses
    6 CONTINUE
      IF(DEBUG)WRITE(6,*)'Looping on df losses LW = ',LW 
      DHVDAV=(DHSHFT+LW)/(1.-LCDH) ! Eq.(36b)
      ISTT=0
      TT2AV=TTIN-DHVDAV/CP ! perfect gas
caldo
      HT2AV=HT0-DHVDAV ! real gas
caldo
      IF(COOLPROP)THEN
         IF(HT2AV.LT.0.0) GO TO 203
      ELSE 
         IF(TT2AV.LT.0.0) GO TO 203
      ENDIF
      U1ASQ=G*J/VUOU1A*DHVDAV*RV1AAV ! Eq.(42)
      U1A=SQRT(U1ASQ)
      VU1A=U1A*VUOU1A
      D1A=CN*U1A/N ! Eq.(43)
C
C    STATION 1 STATOR EXIT
C
      D1=R1R1A*D1A 
      VU1=VU1A/R1R1A 
      V1=VU1/SIN(ALPH1) 
      VR1=VU1/TAN(ALPH1)
      TT1=TTIN
      TS1=TT1-V1*V1/TGJCP ! static temperature; perfect gas
caldo 
      HS1 = HT0 - 0.5*V1*V1 ! compute static enthalpy in STATION 1 STATOR EXIT; real gas 
      NITER = 0
   25 CONTINUE ! A first loop to compute the stator losses
      NITER = NITER+1
      IF(DEBUG)WRITE(6,*)'   looping on stator losses ES ',NITER,ES
      ES1=ES
      ES=EBARS
      IF(EBARS.EQ.1.0.AND.IST.EQ.0) ES=.05
      IF(EBARS.EQ.1.0.AND.IST.GT.0) CALL EFFIC (ES,ER,1)
      IST=IST+1
      ISTT=ISTT+1
    7 V1IDSQ=V1*V1/(1.-ES) ! IDEAL absolute flow velocity in STATION 1: STATOR EXIT
      HELP=1.-0.5*V1IDSQ/HT1  ! real gas
      Z1=1.-V1IDSQ/TGJCP/TT1 ! perfect gas
      IF(COOLPROP)THEN
         IF(HELP.LE.0.0) GO TO 204
      ELSE 
         IF(Z1.LE.0.0) GO TO 204
      ENDIF 
      IF(STAMPA)write(6,*)'Eos Z1 = ',Z1,' Coolprop Z1 = ',HELP
caldo
      PS1=PT0*Z1**EX  ! static pressure Perfect gas
      PT1=PS1*(TT1/TS1)**EX ! total pressure Perfect gas
caldo
      HS1S = HELP * HT1 ! static enthalpy for an isentropic expansion 
      call propssi(NameP, NameH, DBLE(HS1S), NameS, DBLE(S0),
     &             Ref, outVal)! compute static pressure from known entropy and (isentropic) enthalpy in station 1: STATOR EXIT
      IF(COOLPROP)PS1=outVal
      IF(STAMPA)write(6,FMT=784)"PS1   ",PS1/1.e3,outVal/1.e3,
     &(1.-PS1/outVal)*100.
caldo
      call propssi(NameS, NameH, DBLE(HS1), NameP, DBLE(PS1),
     &             Ref, outVal) ! compute entropy from known pressure and enthalpy in STATION 1 STATOR EXIT
      S1 = outval
      call propssi(NameT, NameH, DBLE(HS1), NameP, DBLE(PS1),
     &             Ref, outVal) ! compute temperature from known pressure and enthalpy in STATION 1 STATOR EXIT
      IF(COOLPROP)TS1=outVal
      IF(STAMPA)write(6,FMT=784)"TS1   ",TS1,outVal,(1.-TS1/outVal)*100.
caldo
      call propssi(NameP, NameS, DBLE(S1), NameH, DBLE(HT1),
     &             Ref, outVal) ! compute total pressure from known entropy and total enthalpy in STATION 1 STATOR EXIT
      IF(COOLPROP)PT1=outVal
      IF(STAMPA)write(6,FMT=784)"PT1   ",PT1/1.e3,outVal/1.e3,
     &(1.-PT1/outVal)*100.
caldo
      RHO1=PS1/R/TS1 ! Perfect gas law
caldo
      call propssi(NameR, NameP, DBLE(PS1), NameH, DBLE(HS1),
     &             Ref, outVal) ! compute density from known pressure and static enthalpy in STATION 1 STATOR EXIT
      IF(COOLPROP)RHO1=outVal
      IF(STAMPA)write(6,FMT=784)"RHO1  ",RHO1,outVal,
     &(1.-RHO1/outVal)*100.
Coolprop end
caldo
      HS=W/RHO1/VR1/PI/D1
caldo
      VCR1=SQRT(2.*GAM/(GAM+1.)*G*R*TT1) ! Perfect gas
Coolprop
      outVal = sonic_speed(HT1,S1,PCRIT,Ref)
      IF(COOLPROP)VCR1=outVal
      IF(STAMPA)write(6,FMT=784)"VCR1  ",VCR1,outVal,(1.-VCR1/outVal)*
     &100.
Coolprop
      VOVCR1=V1/VCR1
      IF(verbose)THEN
      write(6,*)'STATION 1 STATOR EXIT PS1 =',PS1/1e5,' TS1 = ',TS1,' PT
     &1 =',PT1/1e5,' TT1 = ',TT1,' D1 = ',D1,' V1 = ',V1,'ALPH1 = ',
     &ALPH1*DOR
      ENDIF
C
C    STATION 0 - STATOR INLET
C
      IF(NSTAR.GT.0) GO TO 71
      D0=D1A*R0R1A
      CHRD=SQRT(D0**2+D1**2-SQRT((D0**2+D1**2)**2-(D0**2-D1**2)**2/ 
     1COS(ALSTG)**2))/2.
      STAR=HS/CHRD
      ALUNC=ALPH1-ALPH0-ACOS((D0**2+D1**2-4.*CHRD**2)/2./D0/D1)
  70  CONTINUE
      IF(IST.EQ.1) SIGS=SIGSIN
      SS=CHRD/SIGS 
      ENS= PI*D1/SS 
      ENST=AINT(ENS)
      IF(ENS-ENST.LT..5) ENS=ENST
      IF(ENS-ENST.GE..5) ENS=ENST+1.
      SS= PI*D1/ENS 
      SIGS=CHRD/SS
      IF(EBARS.NE.1.0) GO TO 26
      IF(ISTT.EQ.1) GO TO 25
      IF(ABS(ES-ES1)/ES.GT..0001) GO TO 25 ! check convergence on stator losses
  26  TT0=TTIN
      RHOT0=PT0/R/TT0 ! perfect gas
caldo
      call propssi(NameR, NameT, DBLE(TT0), NameP, DBLE(PT0),
     &Ref, outVal) ! compute total density from total pressure and total temperature
      IF(COOLPROP)RHOT0=outVal
      IF(STAMPA)write(6,FMT=784)"RHOT0 ",RHOT0,outVal,
     &(1.-RHOT0/outVal)*100.
Coolprop end
      RHO0=RHOT0
      NITER = 0
   9  CONTINUE ! looping to compute RHO(0)
      NITER = NITER+1
      IF(DEBUG)WRITE(6,*)'   looping on RHO(0) ',NITER,RHO0
      V0=W/PI/D0/HS/COS(ALPH0)/RHO0 ! Eq.(56)
C
      TS0=TT0-V0**2/TGJCP ! Eq.(57) static temperature in 0; perfect gas 
      HS0=HT0-0.5*V0**2 ! static enthalpy in STATION 0 - STATOR INLET 
C
      PS0=PT0*(TS0/TT0)**EX ! Eq.(58) static pressure in 0; perfect gas
      call propssi(NameP, NameS, DBLE(S0), NameH, DBLE(HS0),
     &             Ref, outVal) ! static pressure in 0 real gas
      IF(COOLPROP)PS0 = outVal
      IF(STAMPA)write(6,FMT=784)"PS0   ",PS0/1.e3,outVal/1.e3,
     &(1.-PS0/outVal)*100.
      call propssi(NameT, NameS, DBLE(S0), NameH, DBLE(HS0),
     &             Ref, outVal) ! static temperature in 0 real gas
      IF(COOLPROP)TS0 = outVal
      IF(STAMPA)write(6,FMT=784)"TS0   ",TS0,outVal,(1.-TS0/outVal)*100.
C
      RHOX=RHO0
      RHO0=PS0/R/TS0 ! Eq.(59) static density in 0
      call propssi(NameR, NameS, DBLE(S0), NameH, DBLE(HS0),
     &             Ref, outVal) ! compute density using static enthalpy and entropy; real gas
      IF(COOLPROP)RHO0=outVal
      IF(STAMPA)write(6,FMT=784)"RHO0  ",RHO0,outVal,
     &(1.-RHO0/outVal)*100.
C
      IF(ABS(RHOX-RHO0)/RHO0.GT..0001) GO TO 9
      VU0=V0*SIN(ALPH0)
      VR0=V0*COS(ALPH0)
      VOVCR0=V0/VCR1
      IF(verbose)THEN
      write(6,*)'STATION 0 STATOR INLET PS0 =',PS0/1e5,' TS0 = ',TS0,' P
     &T0 =',PT0/1e5,' TT0 = ',TT0,' D0 = ',D0,' V0 = ',V0,'ALPH0 = ',
     &ALPH0*DOR
      ENDIF
C
C  STATION 1A - ROTOR INLET
C
      ALPH1A=ALPH1
      NITER = 0
  8   CONTINUE ! looping to compute ALPH1A
      NITER = NITER+1
      IF(DEBUG)WRITE(6,*)'   looping on ALPHA(1A) ',NITER,ALPH1A*DOR
      NITER = NITER + 1
      VR1A=VU1A/TAN(ALPH1A)
      V1A=VU1A/SIN(ALPH1A) 
      TT1A=TT1
      HT1A = HT1 ! total enthalpy in STATION 1A - ROTOR INLET
      TS1A=TT1A-V1A*V1A/TGJCP  ! static temperature in 1A, perfect gas
      HS1A = HT1A-0.5*V1A*V1A ! static enthalpy in STATION 1A - ROTOR INLET real gas
      PS1A=PT1*(TS1A/TT1)**EX ! perfect gas
      call propssi(NameP, NameS, DBLE(S1), NameH, DBLE(HS1A),
     &Ref, outVal)! static pressure using entropy and static enthalpy in STATION 1A - ROTOR INLET
      IF(COOLPROP)PS1A=outVal
      IF(STAMPA)write(6,FMT=784)"PS1A  ",PS1A/1.e3,outVal/1.e3,
     &(1.-PS1A/outVal)*100.
      RHO1A=PS1A/R/TS1A ! perfect gas
      call propssi(NameR, NameS, DBLE(S1), NameH, DBLE(HS1A),
     &             Ref, outVal) ! compute static density using entropy and static enthalpy in STATION 1A - ROTOR INLET
      IF(COOLPROP)RHO1A=outVal
      IF(STAMPA)write(6,FMT=784)"RHO1A  ",RHO1A,outVal,
     &(1.-RHO1A/outVal)*100.
Coolprop end
      VR1A=VR1*RHO1/RHO1A*R1R1A 
      ALP1A=ALPH1A
      ALPH1A=ATAN (VU1A/VR1A)
      IF(ABS(ALPH1A-ALP1A).GT..01/DOR) GO TO 8
      LWX=LW
      LW=.0061/G/J*RHO1A*U1A**3*D1A**2/W/(RHO1A*U1A*D1A/MU)**.2 ! disk friction losses 
      IF(ABS(LW-LWX)/LW.GT..0001) GO TO 6
      IF(DEBUG)write(6,*)'Has run ',ISTT,' stator iterations'
caldo
caldo Here we finished computing the stator
caldo
caldo
caldo
caldo
caldo
      PT1A=PT1 ! same total pressure in 1 and 1A
      WU1A=VU1A-U1A
      BET1A=ATAN(WU1A/VR1A)
      W1A=VR1A/COS(BET1A)
      TTR1A=TS1A+W1A**2/TGJCP ! total temperature in the relative reference frame; perfect gas
      HTR1A=HS1A+0.5*W1A**2 ! total enthalpy in the relative reference frame
      PTR1A=PS1A*(TTR1A/TS1A)**EX ! total pressure in the relative reference frame
caldo
      call propssi(NameP, NameS, DBLE(S1), NameH, DBLE(HTR1A),
     &Ref, outVal) ! compute total pressure using entropy and total enthalpy in STATION 1A - ROTOR INLET
      IF(COOLPROP)PTR1A=outVal
      IF(STAMPA)write(6,FMT=784)"PTR1A ",PTR1A/1.e3,outVal/1.e3,
     &(1.-PTR1A/outVal)*100.
      call propssi(NameT, NameS, DBLE(S1), NameH, DBLE(HTR1A),
     &             Ref, outVal)
      IF(COOLPROP)TTR1A=outVal
      IF(STAMPA)write(6,FMT=784)"TTR1A ",TTR1A,outVal,(1.-TTR1A/outVal)*
     &100.
caldo
      VCR1A=VCR1 ! is this true for both real and perfect gas? Yes! Because 1 and 1A are on an isentrope
caldo Perfect gas
      WCR1A=VCR1A*SQRT(TTR1A/TT1A) ! ok for a perfect gas
Coolprop
      outVal = sonic_speed(HTR1A,S1,PCRIT,Ref) ! real gas
      IF(COOLPROP)WCR1A=outVal
      IF(STAMPA)write(6,FMT=784)"WCR1A ",WCR1A,outVal,
     &(1.-WCR1A/outVal)*100.
Coolprop
      VOVCR1=V1/VCR1
      VOVCRA=V1A/VCR1A
      WOWCRA=W1A/WCR1A
      IF(DEBUG)THEN
      write(6,*)'STATION 1A ROTOR INLET PS1A =',PS1A/1e5,' TS1A = ',TS1A
     &,' V1A = ',V1A,' W1A = ',W1A,'ALPH1A = ',ALPH1A*DOR,' BET1A = ',
     &BET1A*DOR,' TTR1A = ',TTR1A,' PTR1A = ',PTR1A/1e5,' U1A = ',U1A,
     &'D1A = ',D1A
      ENDIF
C
C  STATION 2 - ROTOR EXIT
C
      M=(K+3)/2
      KK=K+2
C
      if(DEBUG)
     &write(6,*)'STATION 2 ROTOR EXIT; K = ',K,' M = ',M,' KK = ',KK
C
      D2(KK)=D1A*RT2R1A
      D2(1)=D2(KK)*RH2RT2
      HR=(D2(KK)-D2(1))/2.
C
      if(verbose)
     &write(6,*)'STATION 2 ROTOR EXIT; D2(KK) = ',D2(KK),
     &' D2(1) = ',D2(1),' HR = ',HR
caldo
      EL=HR/FLOAT(K)
      SIGRV=0.0
      DO 12 I=1,KK ! loop over the radial stripes
         IF(I.EQ.1.OR.I.EQ.KK) GO TO 11
         D2(I)=D2(1)+(2.*FLOAT(I)-3.)*EL 
         RV2IM(I)=RV2I2M(I-1)
         SIGRV=SIGRV+RV2IM(I)/FLOAT(K)
  11     U2(I)=D2(I)*N/CN ! Eq.(72)
C        PG
         TTR2(I)=TTR1A+(U2(I)**2-U1ASQ)/TGJCP  ! Eq.(73) total temperature in the relative reference frame
         WCR2(I)=WCR1A*SQRT(TTR2(I)/TTR1A) ! Eq.(74) CRITICAL velocity
         PTR2ID(I)=PTR1A*(TTR2(I)/TTR1A)**EX  ! Eq.(75)
caldo
         HTR2(I)=HTR1A+0.5*(U2(I)**2-U1ASQ)
caldo Qui c'è il problema......... perché dovrei usare l'entropia di fine espansione
         S2(I) = S1 ! S2
         outVal = sonic_speed(HTR2(I),S2(I),PCRIT,Ref) !
         IF(COOLPROP)WCR2(I)=outval
         call propssi(NameP, NameS, DBLE(S1), NameH, DBLE(HTR2(I)),
     &Ref, outVal) ! compute relative total pressure 
         IF(COOLPROP)PTR2ID(I)=outVal
         IF(STAMPA)write(6,FMT=784)"PTR2ID",PTR2ID(I)/1.e3,outVal/1.e3,
     &(1.-PTR2ID(I)/outVal)*100.
         call propssi(NameT, NameS, DBLE(S1), NameH, DBLE(HTR2(I)),
     &Ref, outVal)
         IF(COOLPROP)TTR2(I)=outVal
caldo
  12  CONTINUE ! End loop over the radial sectors
      RV2IM(1)=RV2IM(2)-(RV2IM(3)-RV2IM(2))/2. 
      RV2IM(KK)=RV2IM(K+1)-(RV2IM(K)-RV2IM(K+1))/2. 
caldo
      if(DEBUG)then
         do i = 1,KK
         write(6,FMT=444)I,D2(I)*100.,I,TTR2(I),I,PTR2ID(I)/1.e4,
     &I,U2(I),I,WCR2(I),I,RV2IM(I),I,S2(I)/1.e3
  444 FORMAT('D2(',I2,') = ',F10.4,' TTR2(',I2,') = ',F10.4,
     &   ' PTR2ID(',I2,') = ',F10.4,' U2(',I2,') = ',F10.4,
     &   ' WCR2(',I2,') = ',F10.4,' RV2IM(',I2,') = ',F8.4,
     &   ' S(',I2,') = ',F10.4)
         enddo
      endif
caldo
      DVU2AV=D1A*VU1A*(1.-1./RV1AAV) ! =0. whenever RV1AVV=1.
      IF(DEBUG)write(6,*)'DVU2AV= ',DVU2AV
      VU2(M)=DVU2AV/D2(M)*RV2IM(M)/SIGRV
      ITER=1
caldo Here we work at mid-span, i.e. K=M
  13  CONTINUE ! Here we iterate whenever DVU2AV .NE. 0
      IF(DEBUG)WRITE(6,*)'Entropy ad mid-span is ',S2(M)
      WU2(M)=VU2(M)-U2(M) ! Eq.(76)
      DHVD(M)=(U1A*VU1A-U2(M)*VU2(M))/G/J
      TT2(M)=TT0-DHVD(M)/CP ! total temperature in (2) PG
caldo
      HT2(M)=HT0-DHVD(M) ! total enthalpy in (2) real gas (CoolProp)
caldo
      IF(COOLPROP)THEN
         IF(HT2(M).LT.0.0) GO TO 205 
      ELSE
         IF(TT2(M).LT.0.0) GO TO 205 
      ENDIF
caldo
caldo
caldo
      VCR2(M)=VCR1A*SQRT(TT2(M)/TT1A)
      outVal = sonic_speed(HT2(M),S2(M),PCRIT,Ref) ! solito problema: S2 è S1
      IF(STAMPA)write(6,FMT=784)"VCR2  ",VCR2(M),outVal,
     &(1.-VCR2(M)/outVal)*100.
      IF(ITER.EQ.1) VXVCR=0.5 ! axial velocity over critical velocity
      IND=1
      NITER = 0
  14  CONTINUE ! Sub-iterations
      NITER = NITER+1
      IF(DEBUG)WRITE(6,FMT=333)ITER,NITER,VXVCR,WCALC,WGIV
 333  FORMAT('ITER = ',I3,' NITER = ',I3,' VXVCR = ',F10.4,' WCALC = 
     &',F10.4,' WGIV = ',F10.4)
caldo
caldo When the mass flow rate is not equal to W, VXVCR is changed
caldo so that VX2(M) is also changed
caldo
      VX2(M)=VXVCR*VCR2(M)
      BET2(M)=ATAN(WU2(M)/VX2(M))  ! Eq.(77)
      W2(M)=VX2(M)/COS(BET2(M)) ! Eq.(78)
      TS2(M)=TTR2(M)-W2(M)**2/TGJCP ! Eq.(79) relative static temperature in 2: PG
caldo
      HS2(M)=HTR2(M)-0.5*W2(M)**2 ! relative static entalphy in 2 real gas
      IF(STAMPA)write(6,FMT=784)"HS2(M)",CP*TS2(M)/1.e3,HS2(M)/1.e3,
     &(1.-(CP*TS2(M))/HS2(M))*100.
caldo qui S2(M) è in realtà S1(M)
      call propssi(NameT, NameS, DBLE(S2(M)), NameH, DBLE(HS2(M)),
     &Ref, outVal)
      IF(COOLPROP)TS2(M)=outVal
      IF(STAMPA)write(6,FMT=784)"TS2(M)",TS2(M),outVal,
     &(1.-TS2(M)/outVal)*100.
caldo
      D2M=D2(M)
      BET2M=BET2(M)
      WOWCRM= W2(M)/WCR2(M)
      ER=EBARR
      IF(EBARR.EQ.1.0) CALL EFFIC(ES,ER,2)
  15  W2IDSQ(M)=W2(M)**2/(1.-ER) ! Eq.(80) ! Ideal W2
caldo
      Z2=1.-W2IDSQ(M)/TGJCP/TTR2(M) ! perfect gas
      HELP = 1.-0.5*W2IDSQ(M)/HTR2(M) ! real gas
      IF(COOLPROP)THEN
         IF(HELP.LE.0.0) GO TO 206
      ELSE
         IF(Z2.LE.0.0) GO TO 206
      ENDIF
      PS2(M)=PTR2ID(M)*Z2**EX ! Eq.(81)
      RHO2(M)=PS2(M)/R/TS2(M) ! Eq.(82)
caldo
!
!     h_2^{"} - U_2^2/2 = h_{2s} + 0.5*W_2^2/(1-es) - U_2^2/2
!     h_2^{"} = h_{2s} + 0.5*W_2^2/(1-es)
!     h_{2s}/h_2^{"} = 1 - 0.5*W_2^2/[(1-es)*h_2^{"}] = HELP
!
caldo
      HS2S = HELP * HTR2(M)
      call propssi(NameP, NameS, DBLE(S1), NameH, DBLE(HS2S),
     &Ref, outVal)
      IF(COOLPROP)PS2(M)=outVal
      IF(STAMPA)write(6,FMT=784)"PS2(M)",PS2(M)/1.e3,outVal/1.e3,
     &(1.-PS2(M)/outVal)*100.
      call propssi(NameR, NameP, DBLE(PS2(M)), NameH, DBLE(HS2(M)),
     &Ref, outVal)
      IF(COOLPROP)RHO2(M)=outVal
      IF(STAMPA)write(6,FMT=784)"RHO2  ",RHO2(M),outVal,
     &(1.-RHO2(M)/outVal)*100.
caldo compute entropy in (2)
      call propssi(NameS, NameP, DBLE(PS2(M)), NameH, DBLE(HS2(M)),
     &Ref, outVal)
      S2(M) = outVal
caldo
caldo
      F2(M)=PI*D2(M)*EL*RHO2(M)*VX2(M) ! Eq.(92) 
      IF(ITER.EQ.1) WCALC=F2(M)*FLOAT(K)
      IF(ITER.EQ.1) GO TO 51
  67  I=M
      ID=-1
  16  II=I
      I=I+ID
      VU2(I)=VU2(M)*RV2IM(I)*D2(M)/D2(I) ! Eq.(83)
      WU2(I)=VU2(I)-U2(I)! Eq.(84)
      PS2(I)=PS2(II)+RHO2(II)/G*(VU2(II)**2/D2(II)+VU2(I)**2/D2(I))/2.*(
     1D2(I)-D2(II)) ! Eq.(85)
      W2IDSQ(I)=TGJCP*TTR2(I)*(1.-(PS2(I)/PTR2ID(I))**(1./EX)) !  Eq.(86)
      Z3=1.-W2IDSQ(I)/TGJCP/TTR2(I)
caldo
caldo isentropic expansion btw. 1 and isentropic 2
      call propssi(NameH, NameP, DBLE(PS2(I)), NameS, DBLE(S1),
     &Ref, outVal)
      HS2S=outVal
      IF(COOLPROP)W2IDSQ(I)=2.*(HTR2(I)-HS2S)
      HELP = 1.-0.5*W2IDSQ(I)/HTR2(I)
caldo
      IF(COOLPROP)THEN
         IF(HELP.LE.0.0) GO TO 207
      ELSE
         IF(Z3.LE.0.0) GO TO 207
      ENDIF
      IF(STAMPA)write(6,*)'Eos Z3 = ',Z3,' Coolprop Z3 = ',HELP
C
      W2(I)=SQRT(W2IDSQ(I)*(1.-ER))  ! Eq.(87)
      IF(W2(I).LE.ABS(WU2(I))) GO TO 68
      BET2(I)=ASIN(WU2(I)/W2(I)) ! Eq.(88)
      VX2(I)=W2(I)*COS(BET2(I)) ! Eq.(89)
      TS2(I)=TTR2(I)-W2(I)**2/TGJCP ! Eq.(90)
      RHO2(I)=PS2(I)/R/TS2(I) ! Eq.(91)
caldo
      HS2(I)=HTR2(I)-0.5*W2(I)**2 ! static enthalpy in 2
      IF(STAMPA)write(6,FMT=784)"HS2(I)",CP*TS2(I)/1.e3,HS2(I)/1.e3,
     &(1.-CP*TS2(I)/HS2(I))*100.
      call propssi(NameR, NameP, DBLE(PS2(I)), NameH, DBLE(HS2(I)),
     &Ref, outVal)
      IF(COOLPROP)RHO2(I)=outVal
      IF(STAMPA)write(6,FMT=784)"RHO2  ",RHO2(I),outVal,
     &(1.-RHO2(I)/outVal)*100.
caldo compute temperature in (2)
      call propssi(NameT, NameP, DBLE(PS2(I)), NameH, DBLE(HS2(I)),
     &Ref, outVal)
      IF(COOLPROP)TS2(I)=outVal
      IF(STAMPA)write(6,FMT=784)"TS2(I)",TS2(I),outVal,
     &(1.-TS2(I)/outVal)*100.
caldo compute entropy in (2)
      call propssi(NameS, NameP, DBLE(PS2(I)), NameH, DBLE(HS2(I)),
     &Ref, outVal)
      S2(I) = outVal
caldo
      if(DEBUG)
     &write(6,*)'D2(',I,') = ',D2(I)*100.,
     &'W2(',I,') = ',W2(I),'BET2(',I,') = ',
     &BET2(I)*180./3.141593,' TS2(',I,') = ',TS2(I)
C
      IF(I.EQ.1) GO TO 17 ! first strip
      IF(I.EQ.KK) GO TO 18 ! last strip
      F2(I)=PI*D2(I)*EL*RHO2(I)*VX2(I)
      GO TO 16
  17  I=M
      ID=1
      GO TO 16
  18  W2TOT=0.0
      DO 19 I=2,KP1
         W2TOT=W2TOT+F2(I)
  19  CONTINUE
      WCALC=W2TOT
      if(verbose)
!     if(.true.)
     &write(6,*)'After label 19, KP1 = ',KP1,' WCALC = ',WCALC,
     &' F2 = ',F2,' W = ',W
      IF(K.GT.1) GO TO 51 ! Only one radial sector
  69  IF(DVU2AV .EQ. 0.0)GO TO 21
      DVUTOT=0.0
      DO 20 I=2,KP1
         DVUTOT=DVUTOT*F2(I)*D2(I)*VU2(I)
  20  CONTINUE
      DVUAVC=DVUTOT/W
      IF(ABS(DVUAVC-DVU2AV)/DVU2AV.LE..0001) GO TO 21 
      VU2O=VU2(M)
      VU2(M)=VU2O*DVU2AV/DVUAVC
      GO TO 13 ! loop
  21  CONTINUE
      V2LOSS=0.0
      DHIDTA=0.0
      DHIDSA=0.0
      Q=0.0
      RLOSS=0.0
      DO 23 I=1,KK  ! loop over all sectors
        DHVD(I)=(U1A*VU1A-U2(I)*VU2(I))/G/J  ! Eq.(93)
        TT2(I)=TT0-DHVD(I)/CP  ! Eq.(94)
caldo
        HT2(I)=HT0-DHVD(I) ! real gas: total enthalpy
caldo
        IF(COOLPROP)THEN
           IF(HT2(I).LT.0.0) GO TO 208
        ELSE
           IF(TT2(I).LT.0.0) GO TO 208
        ENDIF
        VCR2(I)=VCR1A*SQRT(TT2(I)/TT1A) ! Eq.(95) perfect gas only
        ALPH2(I)=ATAN(VU2(I)/VX2(I))  ! Eq.(96)
        V2(I)=VX2(I)/COS(ALPH2(I)) ! Eq.(98)
        PT2(I)=PS2(I)*(TT2(I)/ TS2(I))**EX ! Eq.(97) perfect gas only
        PTR2(I)=PS2(I)*(TTR2(I)/ TS2(I))**EX
caldo
        HS2(I) = HT2(I)-0.5*V2(I)**2
        call propssi(NameS, NameP, DBLE(PS2(I)), NameH, DBLE(HS2(I)),
     &  Ref, outVal)
        S2(I) = outVal
caldo   speed of sound
        outVal = sonic_speed(HT2(I),S2(I),PCRIT,Ref)
        IF(COOLPROP)VCR2(I) = outVal
caldo   absolute total pressure
        call propssi(NameP, NameS, DBLE(S2(I)), NameH, DBLE(HT2(I)),
     &  Ref, outVal)
        IF(COOLPROP)PT2(I) = outVal
        IF(STAMPA)write(6,FMT=784)"PT2(I)",PT2(I)/1.e3,outVal/1.e3,
     &(1.-PT2(I)/outVal)*100.
caldo   relative total pressure
        call propssi(NameP, NameS, DBLE(S2(I)), NameH, DBLE(HTR2(I)),
     &  Ref, outVal)
        IF(COOLPROP)PTR2(I) = outVal
        IF(STAMPA)write(6,FMT=784)"PTR2  ",PTR2(I)/1.e3,outVal/1.e3,
     &(1.-PTR2(I)/outVal)*100.
caldo
        VOVCR2(I)=V2(I)/VCR2(I)
        WOWCR2(I)=W2(I)/WCR2(I)
C
C SECTOR PERFORMANCE
C
         DHIDT(I)=CP*TT0*(1.-(PT2(I)/PT0)**(1./EX)) ! Eq.(103) perfect gas
        call propssi(NameH, NameS, DBLE(S0), NameP, DBLE(PT2(I)),
     &  Ref, outVal) ! enthalpy corresponding to the isentrope through 0
         HELP =HT0-outVal
         IF(COOLPROP) DHIDT(I)=HELP
        IF(STAMPA)write(6,FMT=784)"DHIDT ",DHIDT(I)/1.e3,HELP/1.e3,
     &(1.-DHIDT(I)/HELP)*100.
caldo
         DHIDS(I)=CP*TT0*(1.-(PS2(I)/PT0)**(1./EX)) ! Eq.(104)
        call propssi(NameH, NameS, DBLE(S0), NameP, DBLE(PS2(I)),
     &  Ref, outVal) ! enthalpy corresponding to the isentrope through 0
         HELP =HT0-outVal
        IF(COOLPROP) DHIDS(I)=HELP
        IF(STAMPA)write(6,FMT=784)"DHIDS ",DHIDS(I)/1.e3,HELP/1.e3,
     &(1.-DHIDS(I)/HELP)*100.
         IF(I.EQ.1.OR.I.EQ.KK) GO TO 22 ! skip first and last sector
         V2LOSS=V2LOSS+F2(I)/W*V2(I)**2/2./G/J
         RLOSS=RLOSS+F2(I)/W*(W2IDSQ(I)-W2(I)**2)/2./G/J
         Q=Q+F2(I)/RHO2(I)
         DHIDTA=DHIDTA+F2(I)/W*DHIDT(I)
         DHIDSA=DHIDSA+F2(I)/W*DHIDS(I)
  22     ETATVD(I)=DHVD(I)/DHIDT(I)
         ETASVD(I)=DHVD(I)/DHIDS(I)
  23  CONTINUE ! loop over all sectors
      IF(DEBUG)THEN
         do i = 1,KK
            WRITE(6,FMT=555)I,D2(I)*100.,I,TTR2(I),I,PT2(I)/1.e3,I,
     &      S2(I)/1.e3,I,ALPH2(I)*180./3.141593,I,DHIDT(I)/1.e3,I,
     &      DHIDS(I)/1.e3
 555  FORMAT('D2(',I2,') = ',F7.3,' TTR2(',I2,') = ',F8.3,
     &   ' PT2(',I2,') = ',F9.3,' S2(',I2,') = ',F8.3,
     &   ' ALPH2(',I2,') = ',F8.3,
     &   ' DHIDT(',I2,') = ',F8.3,
     &   ' DHIDS(',I2,') = ',F8.3)
         enddo
         write(6,*)'DHIDSA = ',DHIDSA,' DHIDTA = ',DHIDTA,' Q = ',Q
         write(6,*)'V2LOSS = ',V2LOSS,' RLOSS = ',RLOSS
      ENDIF
C
C OVERALL PERFORMANCE
C
      ETATV=DHVDAV/DHIDTA 
      ETASV=DHVDAV/DHIDSA 
      ETAT=DHSHFT/DHIDTA 
      ETAS=DHSHFT/DHIDSA 
      SSPD=N*SQRT(Q)/(J*DHIDTA)**.75 ! Specific speed

      LC=LCDH*DHVDAV 
      LCDHIS=LC/DHIDSA 
      LWDHIS=LW/DHIDSA 
      LLDHIS=V2LOSS/DHIDSA
      LSDHIS=(V1IDSQ-V1*V1)/2./G/J/DHIDSA
      LRDHIS=RLOSS/DHIDSA  ! LRDHIS
      GO TO (30,32),IU
      STOP 'IU IS WRONG'
  30  PT0=PT0/10000. 
      PS0=PS0/10000. 
      PT1=PT1/10000. 
      PS1=PS1/10000. 
      PT1A=PT1A/10000. 
      PS1A=PS1A/10000. 
      PTR1A=PTR1A/10000. 
      D0=D0*100.
      D1=D1*100.
      D1A=D1A*100.
      HS=HS*100.
      DO 31 I=1,KK
         D2(I)=D2(I)*100. 
         PT2(I)=PT2(I)/10000. 
         PS2(I)=PS2(I)/10000. 
         PTR2(I)=PTR2(I)/10000.
         DHVD(I)=DHVD(I)/1000.
  31  CONTINUE
      DHVDAV=DHVDAV/1000. 
      GO TO 36
  32  PT0=PT0/144.
      PS0=PS0/144.
      PT1=PT1/144.
      PS1=PS1/144.
      PT1A=PT1A/144. 
      PS1A=PS1A/144. 
      PTR1A=PTR1A/144. 
      D0=D0*12.
      D1=D1*12.
      D1A=D1A*12.
      HS=HS*12.
      DO 33 I=1,KK
         D2(I)=D2(I)*12. 
         PT2(I)=PT2(I)/144. 
         PS2(I)=PS2(I)/144.
         PTR2(I)=PTR2(I)/144.
  33  CONTINUE
C
C WRITE CALCULATED VALUES
C
  36  WRITE (6,111)
!23456789012345678901234567890123456789012345678901234567890123456789012
  111 FORMAT(9H *OUTPUT*/ 26X,2(3HABS,6X),17X,3(3HABS,5X),9X,4HREL ,4(5X
     1,3HREL)/17X,4HDIA-,2(4X,5HTOTAL),4X,2(6HSTATIC,3X),4HFLOW,3X,5HVEL
     2O-,4X,4HCRIT,3X,5HBLADE,2(4X,5HTOTAL),4X,4HFLOW,3X,5HVELO-,4X,4HCR
     3IT/16X,5HMETER,2(5X,4HTEMP,4X,5HPRESS),3X,5HANGLE,4X,4HCITY,9H  VE
     4L RAT,7H  SPEED,5X,4HTEMP,4X,5HPRESS,8H   ANGLE,4X,13HCITY  VEL RA
     5T)
      ALP0=ALPH0*DOR
      ALPHAA=ALPH1A*DOR 
      BETA1A=BET1A*DOR
      DO 34 I=1,KK
         ALPHA2(I)=ALPH2(I)*DOR
         BETA2(I)=BET2(I)*DOR
  34  CONTINUE
      WRITE (6,112)D0,TT0,PT0,TS0,PS0,ALP0,V0,VOVCR0
  112 FORMAT(13H STATOR INLET,2(F9.3,F9.2),F9.3,F7.2,F9.2,F7.3) 
      WRITE(6,1121) ES,HS,ENS
 1121 FORMAT(11H LOSS COEF=,F6.4,67X,11HSTATOR HGT=,F7.4,17H,NUMBER OF V
     1ANES=,F5.1)
      WRITE (6,113)D1,TT1,PT1,TS1,PS1,ALPHA1,V1,VOVCR1
  113 FORMAT(13H STATOR EXIT ,2(F9.3,F9.2),F9.3,F7.2,F9.2,F7.3)
      WRITE (6,114)D1A,TT1A,PT1A,TS1A,PS1A,ALPHAA,V1A,VOVCRA,U1A,TTR1A, 
     1PTR1A,BETA1A,W1A,WOWCRA
  114 FORMAT(13H ROTOR INLET ,2(F9.3,F9.2)F9.3,F7.2,F9.2,F7.3,2F9.2, 
     1F9.3,F8.2,F9.2,F7.3)
      WRITE(6,1141) ER
 1141 FORMAT(11H LOSS COEF=,F6.4)
      WRITE (6,115)(D2(I),TT2(I),PT2(I),TS2(I),PS2(I),ALPHA2(I),V2(I), 
     1VOVCR2(I),U2(I),TTR2(I),PTR2(I),BETA2(I),W2(I),WOWCR2(I),I=1,KK)
  115 FORMAT(13H ROTOR EXIT ,2(F9.3,F9.2),F9.3,F7.2,F9.2,F7.3,2F9.2, 
     1F9.3,F8.2,F9.2,F7.3/(13X2(F9.3,F9.2)F9.3,F7.2,F9.2,F7.3,2F9.2, 
     2F9.3,F8.2,F9.2,F7.3))
      F2(1)=0.0
      F2(KK)=0.0
      WRITE(6,1161)
 1161 FORMAT(1H )
      WRITE(6,116) EN
  116 FORMAT(26X,4HMASS,5X,4HDIAG/17X,4HDIA-,5X,4HFLOW,5X,4HSPEC,4X,5HTO
     1TAL,3X,6HSTATIC/16X,5HMETER,5X,4HRATE,5X,4HWORK,2(4X,5HEFFIC),3X,
     223HNUMBER OF ROTOR BLADES=,F5.1)
      WRITE (6,117)(D2(I),F2(I),DHVD(I),ETATVD(I),ETASVD(I),I=1,KK)
  117 FORMAT(13X,F9.3,F9.4,F9.3,2F8.3)
      TTPR=PT0/PT2(M)
      TSPR=PT0/PS2(M)
      WRITE (6,118)TTPR,LSDHIS,TSPR,LRDHIS,DHVDAV,LWDHIS,ETATV,LCDHIS, 
     1ETASV,LLDHIS,ETAT,ETAS,SSPD
  118 FORMAT(22H *OVERALL PERFORMANCE*/48X,20HLOSS/IDEAL T-S DEL H/6X,24
     1HTOT-TOT PRESSURE RATIO =,F8.4,13X,10HSTATOR   =,F6.4/6X,24HTOT-ST
     2AT PRESSURE RATIO=,F8.4,13X,10HROTOR    =,F6.4/6X,24HDIAG AVG SPEC
     3IFIC WORK =,F8.4,13X,10HWINDAGE  =,F6.4/6X,24HDIAG TOTAL EFFICIENC
     4Y  =,F8.4,13X,10HCLEARANCE=,F6.4/6X,24HDIAG STATIC EFFICIENCY =,F8
     5.4,13X,10HEXIT KE  =,F6.4/6X,24HNET TOTAL EFFICIENCY   =,F8.4/6X, 
     624HNET STATIC EFFICIENCY  =,F8.4,8X,15HSPECIFIC SPEED=,F7.3)
!23456789012345678901234567890123456789012345678901234567890123456789012
!aldo
      WRITE(6,FMT="('Rotor blade height ',F6.2)")HS
      WRITE(6,FMT="('Ratio of stator blade height to rotor-inlet diamete
     &r, HS/D1 = ',F5.2,' should be less than 0.16')")HS/D1
      WRITE(6,FMT="('HS/D2M should be in the range 0.04 < ',F6.2,' < 0.6
     &8')")HS/D2(M)
      WRITE(6,FMT="('RH2R1A should be in the range 0.3 < ',F6.2,' < 0.7'
     &)")RT2R1A
      WRITE(6,FMT="('RH2R1A should be ',F6.2)") 
     &MAX(MIN(1.333333333333*SSPD-0.066666666666666,0.7),0.2)
      WRITE(6,FMT="('D2M/D1 should be in the range 0.2 < ',F6.2,' < 0.6'
     &)")D2(M)/D1
      WRITE(6,FMT="('Aungier efficiency is ',F7.4)")
     &0.87-1.07*(SSPD-0.55)**2-0.5*(SSPD-0.55)**3
      WRITE(6,FMT="('Aungier ALPHA1 should be ',F7.4,' is ',F7.4)")
     &90.-(10.8+14.2*SSPD*SSPD),ALPH1*180./3.141593
caldo
caldo
caldo call draw(0.5*D1A,RT2R1A*0.5*D1A,HS,0.5*D2M,RH2RT2*RT2R1A*0.5*D1A,
caldo&D1*.5,D0*.5,elr)
caldo
caldo
!aldo
      GO TO 2
   51 IF(IND.GE.6.AND.ABS(WGIV-WCALC)/WGIV.LE..0001) GO TO 65
      CALL CONTIN(VXVCR,WCALC,IND,1,WGIV,.05)
      IF(IND-10)14,61,61 ! if IND < 10 jumps to 14; if IND >= 10 to 61
   61 IF(ITER-1)62,62,63 ! if ITER <= 1 goes to 62; ITER > 1 goes to 63
   62 VXVCR=.9
      IF(K.EQ.1) GO TO 63 ! Only one radial sector at exit (K=1)
      IND=1
      ITER=ITER+1
      GO TO 14
   63 IF(IND-10)201,201,202 ! if IND <= 10 jumps to 201; else to 202
   65 IF(ITER-1)66,66,69 ! gets here when the check on massflow is ok (see label 51)
   66 ITER=ITER+1
      IND=1
      GO TO 67
   68 IF(VXVCR.GE.VXVCRP) GO TO 72
      VXVCR=VXVCRP-DELVX
      DELVX=DELVX/10.
      IF(DELVX.LE..00001) GO TO 209
   72 VXVCR=VXVCR+DELVX
      VXVCRP=VXVCR
   71 CHRD=HS/STAR
      IF(NSTAR.EQ.2) GO TO 76
      RSTG=(CHRD*COS(ALSTG)+SQRT(CHRD**2*(COS(ALSTG)**2-1.)+D1**2))/2. 
      D0=(SQRT(CHRD**2/2.+2.*RSTG**2-D1**2/4.))*2.
      ALUNC=ALPH1-ALPH0-ACOS((D0**2+D1**2-4.*CHRD**2)/2./D0/D1)
      GO TO 70
   76 ALPH0=ATAN(SIN(ALPH1)/(CHRD/D1*2.+COS(ALPH1)))
      ALSTG=(ALPH0+ALPH1)/2.
      D0=2.*SQRT(CHRD**2+D1**2/4.+CHRD*D1*COS(ALPH1))
      GO TO 70
  201 WRITE(6,120)WCALC
  120 FORMAT(48HO ROTOR EXIT CHOKES AT MAXIMUM MASS FLOW RATE = ,F9.4)
      GO TO 2
  202 WRITE(6,121)
  121 FORMAT(69HO NO SOLUTION FOUND AFTER 100 ITERATIONS FOR CONTINUITY
     1AT ROTOR EXIT)
      GO TO 2
  203 wRITE(6,122)
  122 FoRmAT(60HOREQUIRED SPECIFIC WORK GREATER THAN ENERGY AVAILABLE IN
     1 GAS)
      GO TO 2
  204 WRITE(6,123)
  123 FORMAT(74HOREQUIRED STATOR IDEAL KINETIC ENERGY GREATER THAN ENERG
     1Y AVAILABLE IN GAS)
      GO TO 2
  205 WRITE(6,124) M
  124 FORMAT(33HOSPECIFIC WORK REQUIRED IN SECTOR,I3,37H GREATER THAN EN
     1ERGY AVAILABLE IN GAS)
      GO TO 2
  206 WRITE(6,125) M
  125 FORMAT(55HOROTOR IDEAL RELATIVE KINETIC ENERGY REQUIRED IN SECTOR,
     1I3,37H GREATER THAN ENERGY AVAILABLE IN GAS)
      GO TO 2
  207 WRITE(6,125) I
      GO TO 2
  208 WRITE(6,124) I
      GO TO 2
  209 WRITE (6,128)
  128 FORMAT(123HOTHE PROGRAM CAN NOT FIND A SOLUTION SIMULTANEOUSLY SAT
     1ISFYING CONTINUITY, RADIAL EQ., AND THE LOSS MODEL AT THE ROTOR EX
     2IT)
  784 FORMAT('PG Eos returns ',A6,' = ',F12.6,' CoolProp returns ',
     &F12.6,' % diff. is ',F8.4)
      GO TO 2
 999  STOP
      END
