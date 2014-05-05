SISPP ;SIS/LM - Clone and optionally modify routine DGRPP
 ;;1.0;NON-VA REGISTRATION;;;Build 15
 ;Copyright 2008 - 2009, Sea Island Systems, Inc.  Rights are granted as follows:
 ;Sea Island Systems, Inc. conveys this modified VistA routine to the PUBLIC DOMAIN.
 ;This software comes with NO warranty whatsoever.  Sea Island Systems, Inc. will NOT be
 ;liable for any damages that may result from either the use or misuse of this software.
 ;
DGRPP ;ALB/MRL,AEG - REGISTRATION SCREEN PROCESSOR ;06 JUN 88@2300
 ;;5.3;Registration;**92,147,343,404,397,489,689**;Aug 13, 1993;Build 1
 ;
 ;DGRPS    : Screen to edit
 ;DGRPSEL  : If screen 9 (income screening) set to allowable selections
 ;           (V=Veteran, S=Spouse, D=Dependents)
 ;DGRPSELT : If screen 9, type selected (V, S, or D or all if none specified)
 ;DGRPAN   : Selectable items on screen for edit (user input)
 ;DGRPANP  : Selectable items for print on page footer - i.e. 1-3
 ;DGRPANN  : Selected item(s) extrapolated (screen_item)
 ;
 ;
EN ;
 D:'$$BEGUPLD^DGENUPL3(DFN)
 .D UNLOCK^DGENPTA1(DFN)
 .D CKUPLOAD^DGENUPL3(DFN)
 .I $$LOCK^DGENPTA1(DFN)
 D ENDUPLD^DGENUPL3(DFN)
 D Q1,WHICH^SISPP1 W ! K DGRP S DGRPAN="" F I=1:1:$L(DGRPVV(DGRPS)) I $S('DGRPV:1,DGRPS=6:I=2!(I=3),1:0) S:'$E(DGRPVV(DGRPS),I) DGRPAN=DGRPAN_I_","
 D STR^SISPP1 F I=$Y:1:20 W !
 I ("8^9"[DGRPS),$G(DGNOBUCK) S Z="C" D W W "=COPY,"
 I ("8^9"[DGRPS),($G(DGEFDT)'=DT) S Z="E" D W W "=ENTER new "_(DGISYR+1)_" data,"
 S Z="<RET>" D W W " to ",$S(DGRPS<DGRPLAST:"CONTINUE",1:"QUIT"),", "
 I DGRPAN]"" S Z=DGRPANP D W D
 . I '$G(DGRPV) W " or " S Z="ALL" D W
 . W " to "_$S('$G(DGRPV):"EDIT, ",DGRPS=6:"EXPAND, ",1:"")
 S DGRPOUT=0,Z="^N" D W W " for screen N or " S Z="'^'" D W W " to QUIT" I DGRPSEL=""!(DGRPVV(9)'["0")!+$G(DGRPV) W ": "
 I DGRPSEL]"" D MOREHLP^SISPP1
 R DGRPANN:DTIME S:'$T DGRPOUT=1 I DGRPANN']"",'DGRPOUT G NEXT
 I $E(DGRPANN)="E",$G(DGNOBUCK),("8^9"[DGRPS) D
 .S DGNOCOPY=1
 .S DGRPANN=U_DGRPS,DGRPVV(9)="0000000000",DGRPVV(8)="00"
 I $E(DGRPANN)="C",$G(DGNOBUCK),("8^9"[DGRPS) D
 .W !!,"  COPYING will move Family Demographic and Income Data into the next year...",!
 .I DGNOBUCK=2 D
 ..W !,"   YOU HAVE ALREADY MODIFIED CURRENT YEAR DEPENDENT INFORMATION"
 ..W !,"     COPYING will OVERWRITE this modified dependent information"
 ..W !,"     with LAST year's data -  ** Please review dependent data **"
 ..D COPY^DGMTU22(DFN,DT) S DGRPANN=U_8
 .I DGNOBUCK=1 D COPY^DGMTU22(DFN,DT) S DGRPANN=U_DGRPS
 .I Y>0 D
 ..W !,"   ...FAMILY DEMOGRAPHIC DATA COPIED"
 ..W !,"   ...............INCOME DATA COPIED"
 ..H 2
 ..S DGRPVV(9)="0000000000",DGRPVV(8)="00",DA=$$GETIN^DGMTU2(DFN,+DGREL("V"),DT) S DIE=408.21,DR=".18///^S X=""YES""" D ^DIE K DA,DIE,DR
JUMP G JUMP^SISPP1:DGRPANN?1"^"1N.".".1N I DGRPOUT!(DGRPANN?1"^".E) G Q
 S (DGRPANN,X)=$$UPPER^DGUTL(DGRPANN)
 I $E(DGRPANN)="A" S X=DGRPANN,Z="^ALL" D IN^DGHELP I %'=-1 S DGRPANN=DGRPANP
 I DGRPANN]"",(DGRPSEL[$E(DGRPANN)) S DGRPSELT=$E(DGRPANN),DGRPANN=$P(DGRPANN,DGRPSELT,2) ; save off type, run through all other checks
 I DGRPANN'?1N.E D ^DGRPH G:DGRPS'=1.1 @("^SISP"_DGRPS) G:DGRPS=1.1 ^SISPCADD
 S DGDR="" F I=1:1 S DGCH=$P(DGRPANN,",",I) Q:DGCH']""!($L(DGCH)>5)  D CHOICE
 I DGDR']"" D ^DGRPH S X=DGRPS G SCRX
 D ^DGRPE G QQ:'$D(^DPT(DFN,0)) S X=DGRPS G SCRX
Q I 'DGELVER D:$S(DGRPOUT:0,'$D(DGRPV):0,'DGRPV:1,1:0) LT^SISPP1
 K DGDEP,DGINC,DGINR,DGMTC,DGMTED,DGREL,DGTOT,DGSP
 K DGCH,DGGTOT,DGIRI,DGPRI,DGRPSE1,DGNOCOPY
 D SENSCHK
 I 'DGRPV S DGEDCN=1 D ^DGRPC K DGEDCN
QQ K DGRPNA,DGRPS,DGRPTYPE,DGRPU,DGRPV,DGRPVV,DGRPW,DGVI,DGVO,DGRPCM,DGELVER,DGRPLAST
Q1 K %DT,C,DGA,DGA1,DGA2,DGAD,DGDR,DGRP,DGRPAG,DGRPAN,DGRPANN,DGRPANP,DGRPD,DGRPSEL,DGRPSELT,DGRPVR,DGRPX,DGAAC
 K DIRUT,DUOUT,DTOUT
 K DIC,I,I1,I2,I3,J,X,X1,X2,X3,Y,Z,Z1 I $D(DFN)#2,DFN]"" S:$D(^DPT(DFN,0)) DA=DFN
 Q
 ;
SENSCHK ; check whether patient record should be made sensitive
 N ELIG,FLAG,X
 S ELIG=0,FLAG=0
 I '$D(^DPT($G(DFN),0)) Q  ; patient not defined
 I $D(^DGSL(38.1,DFN,0)) Q  ; patient already in dg security log file
 S X=$S($D(^DPT(DFN,"TYPE")):+^("TYPE"),1:"") I $D(^DG(391,+X,0)),$P(^(0),"^",4) D SEC Q:FLAG
 F  S ELIG=$O(^DPT(DFN,"E",ELIG)) Q:'ELIG  D  Q:FLAG
 . S X=$G(^DIC(8,ELIG,0))
 . I $P(X,"^",12) D SEC
 Q
 ;
SEC ;if patient type says make record sensitive, add to security log file
 K DD,DO S DIC="^DGSL(38.1,",(X,DINUM)=DFN,DIC(0)="L",DIC("DR")="2///1;3////"_DUZ_";4///NOW;" D FILE^DICN
 I $D(^DGSL(38.1,DFN,0)) W !!,"===> Record has been classified as sensitive." S FLAG=1
 K DIC,X,DINUM,DA,DD,DO,Y
 Q
 ;
CHOICE ;parse out which items were selected for edit
 ;
 ;DGCH=choice to be parsed (either number or number-number)
 ;
 N DGFL S DGFL=0
 I DGCH["-" Q:DGCH'?1.2N1"-"1.2N!($P(DGCH,"-",2)>17)  F J=$P(DGCH,"-",1):1:$P(DGCH,"-",2) I DGRPAN[(J_",") D:(DGRPS=9) SCR9 I 'DGFL S DGDR=DGDR_(DGRPS*100+J)_","
 I DGCH'["-",DGCH?1.2N,(DGRPAN[(DGCH_",")) S DGDR=DGDR_(DGRPS*100+DGCH)_","
 Q
 ;
NEXT ;find next available screen...goto
 I DGRPS=DGRPLAST G Q ;last screen and return...quit
 S X=DGRPLAST
 F I=DGRPS+1:1 S J=$E(DGRPVV,I) Q:J']""  I 'J S X=I Q
 I DGRPS=1 S X=1.1
 ; SIS/LM non-VA custom registration -
 I $P(X,".")>DGRPLAST G Q ;Past last number specified by parameter value
 ; End non-VA modification
SCRX ;goto screen X
 ;I DGRPLAST=DGRPS,DGRPLAST=X G Q
 I X[".",X'=1.1 S X=$P(X,".",1)
 G:X=1.1 ^SISPCADD
 G:X'=1.1 @("^SISP"_X) ;goto next available screen
 ;
W ;write highlighted text on screen (if parameter on)
 I IOST="C-QUME",$L(DGVI)'=2 W Z
 E  W @DGVI,Z,@DGVO
 Q
 ;
SCR9 ; see if MT is completed.  Allow only selective editing if so
 I 'DGMTC Q
 I '$D(DGRPSELT) S:DGMTC=1 DGFL=1 Q  ;if no non-mt dependents
 I DGRPSELT="S",$D(DGMTC("S")) Q
 I DGRPSELT="D",$D(DGMTC("D")) Q
 S DGFL=1
 Q
