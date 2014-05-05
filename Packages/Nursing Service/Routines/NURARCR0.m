NURARCR0 ;HIRMFO/RM/RD-VIEW PRINT OF PATIENT CLASSIFICATION ;1/17/89
 ;;4.0;NURSING SERVICE;;Apr 25, 1997
EN1 ;
 Q:'$D(^DIC(213.9,1,"OFF"))  Q:$P(^DIC(213.9,1,"OFF"),"^",1)=1
 S NURSSEL=0 G BGN
EN2 ;
 Q:'$D(^DIC(213.9,1,"OFF"))  Q:$P(^DIC(213.9,1,"OFF"),"^",1)=1
 S NURSSEL=1
BGN ;
 S (NURQUEUE,NUROUT,NURSW1,NURQUIT,NURPAGE)=0
 S NASK=1,DIC(0)="EQMZ",NACT=0 D EN5^NURSCUTL I DFN="" S NUROUT=1 G QUIT
 D DEM^VADPT
 S SSN=VA("PID"),N1=VADM(1)
 D EN6^NURSCUTL
 I VAIN(7)'="" S NDFLT=$P(VAIN(7),"^",2)
ENTADM ;
 S %DT("A")="Start date (time optional): ",%DT(0)=-DT,%DT("B")="T-7",%DT="AETX" D ^%DT K %DT I +Y'>0 S NUROUT=1 G QUIT
 S NADMDATE=+Y
 W ! S %DT("A")="Go to date (time optional): ",%DT="AETX",%DT("B")="NOW" D ^%DT K %DT I +Y'>0 S NUROUT=1 G QUIT
 S (X1,NURSDIS)=+Y,X2=NADMDATE D ^%DTC
 I X<0!(X=0&(((+("."_$P(NURSDIS,".",2))*10000)-((+("."_$P(NADMDATE,".",2))*10000)))'>0)) W !?5,"Ending date of range needs to be greater than starting date.",!?5,$C(7),"Please reenter!!" G ENTADM
 G:NURSSEL=0 DEV
ENTWRD ;
 S DIC("A")="ENTER WARD: ",DIC="^NURSF(211.4,",DIC(0)="AEMQ",DIC("S")="I $S('$D(^(""I"")):1,$P(^(""I""),U)'=""I"":1,1:0),$S('$D(^(1)):1,$P(^(1),U)=""A"":1,1:0)" W ! D ^DIC K DIC I +Y'>0 S NUROUT=1 G QUIT
 S NURSW1=+Y
 S NCK=0 F X=0:0 S X=$O(^NURSA(214.6,"AA",DFN,X)) Q:(X'>0)!(NUROUT)  S NURSCLAS=$O(^NURSA(214.6,"AA",DFN,X,"")) S:$D(^NURSA(214.6,"E",NURSW1,NURSCLAS)) NCK=1
 S NPWARD=+NURSW1 D EN6^NURSAUTL
 I 'NCK W !,*7,N1," NOT CLASSIFIED ON ",NPWARD,!,"FOR THIS ADMISSION DATE, PLEASE REENTER WARD." G ENTWRD
DEV W ! S ZTRTN="START^NURARCR0" D EN7^NURSUT0 G:POP!($D(ZTSK)) QUIT
START ;
 K ^TMP($J)
 F X=0:0 S X=$O(^NURSA(214.6,"AA",DFN,X)) Q:X'>0  F NURSCLAS=0:0 S NURSCLAS=$O(^NURSA(214.6,"AA",DFN,X,NURSCLAS)) Q:NURSCLAS'>0  D SORT
 S X=$O(^TMP($J,"")) I X="" S NUROUT=1,NL1="" D HEADER^NURARCR1 W !!,"**** NO DATA FOR THIS REPORT ****" G QUIT
PRINTIT U IO D ^NURARCR1
QUIT K ^TMP($J) D CLOSE^NURSUT1,^NURAKILL
 Q
SORT S NDATA=$S($D(^NURSA(214.6,NURSCLAS,0)):^(0),1:"") I NURSSEL Q:$P(NDATA,"^",8)'=NURSW1
 S CNDATE=$P(NDATA,"^")
 S NPWARD=$P(NDATA,"^",8) D EN6^NURSAUTL
 S CNWARD=$S($P(NDATA,"^",8)="":"  BLANK",'$D(^NURSF(211.4,$P(NDATA,"^",8),0)):"  BLANK",$P(^(0),"^")="":"  BLANK",$D(NPWARD):NPWARD,1:"  BLANK")
 S NBED=$S($P(NDATA,"^",9)="":"",$D(^NURSF(213.3,$P(NDATA,"^",9),0)):$P(^(0),"^"),1:"") Q:NBED=""!(NBED="HEMODIALYSIS")!(NBED="DOMICILIARY")!(NBED="RECOVERY ROOM")
 Q:'(CNDATE>NADMDATE&(CNDATE<NURSDIS))
 S ^TMP($J,CNWARD,CNDATE,NURSCLAS)=""
 Q
