PRCPRDI2 ;WISC/RFJ-print calculated due-ins                         ;30 Aug 91
 ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 Q
 ;
 ;
PRINT ;  called from prcprdi1 to print calculated due-ins
 N %,%H,%I,D,DATA,DATE,DUEIN,ITEMDA,NSN,PAGE,PRCPFLAG,QTY,SCREEN,TRANDA,TRANNO,X,Y
 D NOW^%DTC S Y=% D DD^%DT S DATE=Y,PAGE=1,SCREEN=$$SCRPAUSE^PRCPUREP,ITEMDA=0 U IO D H
 ;  sort by nsn
 K ^TMP($J,"PRCPRDI2")
 F  S ITEMDA=$O(^TMP($J,"PRCPRDI1-DI",ITEMDA)) Q:'ITEMDA  S NSN=$$NSN^PRCPUX1(ITEMDA) S:NSN="" NSN=" " S ^TMP($J,"PRCPRDI2",NSN,ITEMDA)=""
 ;
 S NSN="" F  S NSN=$O(^TMP($J,"PRCPRDI2",NSN)) Q:NSN=""!($D(PRCPFLAG))  S ITEMDA=0 F  S ITEMDA=$O(^TMP($J,"PRCPRDI2",NSN,ITEMDA)) Q:'ITEMDA!($D(PRCPFLAG))  D
 .   I $Y>(IOSL-4) D:SCREEN P^PRCPUREP Q:$D(PRCPFLAG)  D H
 .   S D=$G(^PRCP(445,PRCP("I"),1,ITEMDA,0))
 .   W !!,NSN,?20,$E($$DESCR^PRCPUX1(PRCP("I"),ITEMDA),1,20),?42,"[#",ITEMDA,"]",?49,$J($$UNITVAL^PRCPUX1($P(D,"^",14),$P(D,"^",5)," per "),13),?70,$J($$GETIN^PRCPUDUE(PRCP("I"),ITEMDA),10)
 .   D H1
 .   S (TRANDA,DUEIN)=0 F  S TRANDA=$O(^TMP($J,"PRCPRDI1-DI",ITEMDA,TRANDA)) Q:'TRANDA!($D(PRCPFLAG))  S DATA=^(TRANDA) D
 .   .   I $Y>(IOSL-4) D:SCREEN P^PRCPUREP Q:$D(PRCPFLAG)  D H,H1
 .   .   S TRANNO=$P($G(^PRCS(410,TRANDA,0)),"^"),DUEIN=DUEIN+$P(DATA,"^",2)
 .   .   W !?10,TRANNO,?30,$P($P($G(^PRC(442,+$P(DATA,"^",6),0)),"^"),"-",2),?37,$J($$UNITVAL^PRCPUX1($P(DATA,"^",4),$P(DATA,"^",3)," per "),13),?56,$J($P(DATA,"^",5),6),$J($P(DATA,"^",2),10)
 .   .   I '$D(^PRCP(445,PRCP("I"),1,ITEMDA,7,TRANDA,0)) W ?77,"ADD" Q
 .   .   I $P(^PRCP(445,PRCP("I"),1,ITEMDA,7,TRANDA,0),"^",2)'=$P(DATA,"^",2) W ?77,"UPD"
 .   ;
 .   W !?33,"CALCULATED TOTAL DUE-IN QTY: ",$J(DUEIN,10) I $$GETIN^PRCPUDUE(PRCP("I"),ITEMDA)'=DUEIN W " <-----*"
 ;
 I $D(PRCPFLAG),$G(PRCPFUPD) U IO(0) W !,"DUE-INS AND OUTSTANDING TRANSACTIONS WILL NOT BE UPDATED UNTIL ENTIRE REPORT",!,"IS PRINTED."
 I '$D(PRCPFLAG),$G(PRCPFUPD) D UPDATE W !!,"DUE-INS AND OUTSTANDING TRANSACTIONS HAVE BEEN UPDATED."
 I '$D(PRCPFLAG) D END^PRCPUREP
 D ^%ZISC K ^TMP($J,"PRCPRDI2")
 Q
 ;
 ;
H S %=DATE_"  PAGE "_PAGE,PAGE=PAGE+1 I PAGE'=2!(SCREEN) W @IOF
 W $C(13),"CALCULATED DUE-INS REPORT FOR: ",PRCP("IN"),?(80-$L(%)),%,!,"NSN",?20,"DESCRIPTION",?42,"[#MI]",?51,"UNIT per ISS",?70,"DUE-IN QTY",! S %="",$P(%,"-",81)="" W %
 Q
 ;
 ;
H1 W !?10,"TRANSACTION",?30,"PO #",?39,"UNIT per REC",?53,"CONV FACT",?66,"DUE-IN"
 Q
 ;
 ;
UPDATE ;  update due-ins and outstanding transactions
 N %,DATA,ITEMDA,TRANDA
 S ITEMDA=0 F  S ITEMDA=$O(^PRCP(445,PRCP("I"),1,ITEMDA)) Q:'ITEMDA  D
 .   L +^PRCP(445,PRCP("I"),1,ITEMDA)
 .   ;  get rid of old transactions
 .   S TRANDA=0 F  S TRANDA=$O(^PRCP(445,PRCP("I"),1,ITEMDA,7,TRANDA)) Q:'TRANDA  I $D(^TMP($J,"PRCPRDI1-CK",TRANDA)),'$D(^TMP($J,"PRCPRDI1-DI",ITEMDA,TRANDA)) D KILLTRAN^PRCPUTRA(PRCP("I"),ITEMDA,TRANDA)
 .   ;  add new transactions
 .   S TRANDA=0 F  S TRANDA=$O(^TMP($J,"PRCPRDI1-DI",ITEMDA,TRANDA)) Q:'TRANDA  S DATA=^(TRANDA) D
 .   .   I '$D(^PRCP(445,PRCP("I"),1,ITEMDA,7,TRANDA,0)) D ADDTRAN^PRCPUTRA(PRCP("I"),ITEMDA,TRANDA,$P(DATA,"^",2,5))
 .   .   S %=^PRCP(445,PRCP("I"),1,ITEMDA,7,TRANDA,0),$P(%,"^",2)=$P(DATA,"^",2)
 .   .   I $P(DATA,"^",3) S $P(%,"^",3)=$P(DATA,"^",3)
 .   .   I $P(DATA,"^",4) S $P(%,"^",4)=$P(DATA,"^",4)
 .   .   I $P(DATA,"^",5) S $P(%,"^",5)=$P(DATA,"^",5)
 .   .   S ^PRCP(445,PRCP("I"),1,ITEMDA,7,TRANDA,0)=%
 .   ;  recalc total quantity due-in
 .   S QTY=0
 .   S TRANDA=0 F  S TRANDA=$O(^PRCP(445,PRCP("I"),1,ITEMDA,7,TRANDA)) Q:'TRANDA  S QTY=QTY+$P($G(^(TRANDA,0)),"^",2)
 .   I QTY<0 S QTY=0
 .   D SETIN^PRCPUDUE(PRCP("I"),ITEMDA,QTY-$$GETIN^PRCPUDUE(PRCP("I"),ITEMDA))
 .   L -^PRCP(445,PRCP("I"),1,ITEMDA)
 Q
