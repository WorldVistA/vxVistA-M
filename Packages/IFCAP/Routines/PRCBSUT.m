PRCBSUT ;WISC@ALTOONA/CTB/SAW-GET STATION INFO ;8/25/00  16:18
V ;;5.1;IFCAP;**97**;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
EN1 ;STATION,FY,QUARTER,CONTROL POINT
 D ^PRCFSITE G:'% EX D QT G:PRC("QTR")="^" EX D CP G:Y<0 EX
 S %=1,Z=PRC("SITE")_"-"_PRC("FY")_"-"_PRC("QTR")_"-"_C1,X=$P(Z,"-",1,2)_"-"_C1 G EXIT
QT ;SELECT QUARTER
 D:'$D(DT) DT^DICRW I '$D(PRCSQTT) S:$D(PRC("QTR")) PRCSQTT=PRC("QTR") I '$D(PRCSQTT) S PRCSI=$E(DT,4,5),PRCSQTT=$P("2^2^2^3^3^3^4^4^4^1^1^1","^",PRCSI)
 W !,"Select QUARTER: ",PRCSQTT,"// " R PRC("QTR"):DTIME S:'$T PRC("QTR")=U S:PRC("QTR")=U %=0 S:PRC("QTR")="" PRC("QTR")=PRCSQTT Q:PRC("QTR")="^"  I PRC("QTR")<1!(PRC("QTR")>4)!(PRC("QTR")'?1N) W $C(7) G QT
 Q
CP ;SELECT CONTROL POINT
 S DIC="^PRC(420,"_PRC("SITE")_",1,",DIC(0)="AEMNQZ",DIC("S")="I $D(^(2))",DIC("A")="Select CONTROL POINT: "
 I $D(PRCSK) S DIC("S")=$P(DIC("S"),",1",1)_"!($P(^PRC(420,PRC(""SITE""),1,+Y,0),""^"",9)=""Y"")"
 S:$D(PRC("CP")) DIC("B")=PRC("CP") S D="B^C"
 D MIX^DIC1 K DIC G:Y<0 EXIT I Y>0 S C=$P(Y(0),"^",1),C1=$P(Y(0)," ",1),PRC("CP")=+Y
 S C=$P(^PRC(420,PRC("SITE"),1,PRC("CP"),0),"^",1),C1=$P(C," ",1)
 K DIC,N Q
EX S Y=-1,%=0 K PRC("QTR"),PRC("FY"),PRCSI I $D(PRC("CP")) K:PRC("CP")="ALL"!(PRC("CP")="^") PRC("CP")
EXIT K PRCSFYT,PRCSI,PRCSK,PRCSQTT Q
