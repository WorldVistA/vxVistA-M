PSOMGRP4 ;BHAM ISC/JMB - DAILY MANAGEMENT IV REPORT ; 3/1/93
 ;;7.0;OUTPATIENT PHARMACY;;DEC 1997
EN S DVCNT=0 F DV=0:0 S DV=$O(^PS(59,DV)) Q:'DV  S DIV=DV,DVCNT=DVCNT+1
 S (BEG,PG)=0 K ^TMP($J) F IV=0:0 S IV=$O(^PS(50.8,IV)) Q:'IV  I $D(^PS(50.8,IV,2)) F DAT=SDT-1:0 S DAT=$O(^PS(50.8,IV,2,DAT)) Q:'DAT!(DAT>EDT)  D
 .I $D(^PS(50.8,IV,2,DAT,1)) F WARD=0:0 S WARD=$O(^PS(50.8,IV,2,DAT,1,WARD)) Q:'WARD  D GETEM:WARD=.5
 .;*******  WARD=.5
 .I $D(^PS(50.8,IV,2,DAT,2)) F DRG=0:0 S DRG=$O(^PS(50.8,IV,2,DAT,2,DRG)) Q:'DRG  I $D(^(DRG,0)) S DRGCOST=$P(^(0),"^",5) D WARD
W S PG=0 I '$D(^TMP($J)) D H W !,"No data." W:$E(IOST)'="C" @IOF G K
 K TOT S (BTOT,IVTOT)=0 F IV=0:0 S IV=$O(^TMP($J,IV)) Q:'IV  D IV W ! D DLINE W !,"GRAND TOTAL: " F TYP="P","A","H","C" S PR="" D CTOT
 W ?101,$J(BTOT,8,0),?111,$J(IVTOT,13,2),!!!?17,"FINISHED PRINTING ON: " D NOW^%DTC S Y=% X ^DD("DD") W Y W:RUN="A" @IOF K ZTSK D ^%ZISC
K K BTOT,C,CCOL,CNT,CNTNDE,D,DA,DAT,DISP,DRG,DRGCOST,DS,EDT,I6,IV,IVTOT,JJ,LN,LO,DATE,PG,SDT,TCOL,^TMP($J)
 ;,^TMP($J),
 K TOT,TOTNDE,TUC,TYP,WARD,WCOST,WD,WDISP,WUNITS,X,Y,ZZ
 Q
LINE W ! F LN=1:1:124 W "-"
 Q
MLINE W !?15 F LN=1:1:109 W "-"
 Q
DLINE W ! F LN=1:1:124 W "="
 Q
WARD I $D(^PS(50.8,IV,2,DAT,2,DRG,3)) F WD=0:0 S WD=$O(^PS(50.8,IV,2,DAT,2,DRG,3,WD)) Q:'WD  I WD=.5,$D(^(WD,1)),'$D(^DIC(42,WD,0)) F TYP="P","A","H","S","C" D TYPE
 Q
TYPE S DA=$O(^PS(50.8,IV,2,DAT,2,DRG,3,WD,"B",TYP,0)) Q:DA'>0
 S COST=$P(^PS(50.8,IV,2,DAT,2,DRG,3,WD,1,DA,0),"^",2)*DRGCOST
 S:TYP="S" LO=$S($D(^TMP($J,IV,DAT,"P")):^("P"),1:"") S:TYP'="S" LO=$S($D(^TMP($J,IV,DAT,TYP)):^(TYP),1:"") S $P(LO,"^")=$P(LO,"^")+COST
 S:TYP="S" ^TMP($J,IV,DAT,"P")=LO F TT=1:1:2 S:TYP'="S" ^TMP($J,IV,DAT,TYP)=LO F TT=1:1:2
 K COST Q
H S PG=PG+1 U IO W @IOF W !!?30,"O U T P A T I E N T   P H A R M A C Y   M A N A G E M E N T   R E P O R T",!?55,"INTRAVENOUS ADMIXTURE",?117,"PAGE ",PG
 W !!?40,"FROM "_$E(SDT,4,5)_"-"_$E(SDT,6,7)_"-"_$E(SDT,2,3),?60,"TO "_$E(EDT,4,5)_"-"_$E(EDT,6,7)_"-"_$E(EDT,2,3)_"      ALL DIVISIONS",!!
 W !!?24,"PIGGYBKS & SYRS",?47,"L. V. P",?71,"T. P. N.",?87,"CHEMOTHERAPY",?110,"PER DATE",!?7,"DATE",?24,"TOT     AVG CST",?44,"TOT     AVG CST",?66,"TOT     AVG CST",?85,"TOT     AVG CST",?106,"TOT        TOT CST"
 D LINE Q
IV D H S DATE="",BEG=0 F JJ=0:0 S DATE=$O(^TMP($J,IV,DATE)) D:DATE="" MON Q:DATE=""  D:$Y+6>IOSL H D
 .S MN=$E(DATE,1,5) S:'BEG PRV=$E(DATE,1,5),BEG=1 S:MN'=PRV&('$D(MM(PRV_"^P"))) PRV=$E(DATE,1,5)
 .S:'$D(MM(MN_"^"_TYP)) MM(MN_"^"_TYP)="0^0^"
 .K WDISP,WCOST,WUNITS D WRT2
 Q
WRT2 I MN'=PRV D MON S PRV=$E(DATE,1,5)
 W !,$E(DATE,4,5)_"-"_$E(DATE,6,7)_"-"_$E(DATE,2,3) S TOTNDE=^TMP($J,IV,DATE,0) K WDISP,WCOST,TUC,DISP
 F TYP="P","A","H","C" S ZZ(TYP)=$S($D(^TMP($J,IV,DATE,TYP)):^(TYP),1:"") D COMPTE K PR D PRTLN1
WDTOT S DS=WDISP,TUC=$S(DS'>0:0,1:WCOST),DISP=WDISP S IVTOT=IVTOT+WCOST,BTOT=BTOT+DISP,TYP="Z" D PRTLN1
 Q
COMPTE S DISP=$P(TOTNDE,"^",$S(TYP="P":1,TYP="A":2,TYP="H":3,TYP="C":4,1:5)) I +$P(ZZ(TYP),"^")'>0 S TUC=0 G HERE
 S:DISP'>0 TUC=0 G:DISP'>0 HERE S TUC=$P(ZZ(TYP),"^")/DISP
HERE S WDISP=$S($D(WDISP):WDISP+DISP,1:DISP),WCOST=$S($D(WCOST):WCOST+$P(ZZ(TYP),"^"),1:$P(ZZ(TYP),"^"))
 S LO=$S($D(TOT(TYP)):TOT(TYP),1:""),$P(LO,"^")=$P(LO,"^")+$P(ZZ(TYP),"^"),$P(LO,"^",2)=$P(LO,"^",2)+DISP
 S $P(MM(MN_"^"_TYP),"^")=$S('$D(MM(MN_"^"_TYP)):0,1:$P(MM(MN_"^"_TYP),"^"))+$P(ZZ(TYP),"^")
 S $P(MM(MN_"^"_TYP),"^",2)=$S('$D(MM(MN_"^"_TYP)):0,1:$P(MM(MN_"^"_TYP),"^",2))+DISP
 S TOT(TYP)=LO
 Q
MON K DISP,TUC,WDISP,WCOST D MLINE W !,"MONTH TOTAL" F TYP="P","A","H","C" D
 .S TUC=$S('$D(MM(PRV_"^"_TYP)):0,$P(MM(PRV_"^"_TYP),"^",2)<1:0,1:$P(MM(PRV_"^"_TYP),"^")/$P(MM(PRV_"^"_TYP),"^",2))
 .S DISP=$S($D(MM(PRV_"^"_TYP)):$P(MM(PRV_"^"_TYP),"^",2),1:0)
 .S WDISP=$S($D(WDISP):WDISP+DISP,1:DISP),WCOST=$S($D(WCOST):WCOST+$P(MM(PRV_"^"_TYP),"^"),1:$P(MM(PRV_"^"_TYP),"^"))
 .D PRTLN1
 S DS=WDISP,TUC=$S(DS'>0:0,1:WCOST),DISP=WDISP,TYP="Z" D PRTLN1
 Q
CTOT S TUC=$S('$D(TOT(TYP)):0,$P(TOT(TYP),"^",2)<1:0,1:$P(TOT(TYP),"^")/$P(TOT(TYP),"^",2))
 S DISP=$S($D(TOT(TYP)):$P(TOT(TYP),"^",2),1:0)
PRTLN1 S TCOL=$S(TYP="P":20,TYP="A":40,TYP="H":62,TYP="C":73,1:102),CCOL=$S(TYP="P":27,TYP="A":40,TYP="H":62,TYP="C":73,1:112) W ?TCOL,$J(DISP,7,0),?CCOL,$J(TUC,12,2)
 Q
GETEM I $D(^PS(50.8,IV,2,DAT,1,WARD,0)) S CNTNDE=^(0),X=0 D SETEM
 I $D(^PS(50.8,IV,2,DAT,1,WARD,"R")) S CNTNDE=^("R"),X="R" D SETEM
 Q
SETEM F ZZ=1:1:5 S CNT(ZZ)=$P(CNTNDE,"^",ZZ+1)
 S LO=$S($D(^TMP($J,IV,DAT,X)):^(X),1:"") F ZZ=1:1:5 S $P(LO,"^",ZZ)=$P(LO,"^",ZZ)+CNT(ZZ)
 S CNT(1)=CNT(1)+CNT(5),$P(LO,"^")=$P(LO,"^")+$P(LO,"^",5),$P(LO,"^",5)=""
 S ^TMP($J,IV,DAT,X)=LO Q
