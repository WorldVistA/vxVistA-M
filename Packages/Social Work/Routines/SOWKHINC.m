SOWKHINC ;B'HAM ISC/SAB-Routine to inquire into RCH file ; 22 Jan 93 / 10:13 AM
 ;;3.0; Social Work ;;27 Apr 93
SEA W ! S DIC="^SOWK(652,",DIC(0)="AQEM",DIC("A")="Enter RCH: " D ^DIC G:"^"[X CLO G SEA:Y<0 S DA=+Y K DIC
 K %ZIS,IOP,ZTSK S SOWKION=ION,%ZIS="QM" D ^%ZIS K %ZIS I POP S IOP=SOWKION D ^%ZIS K IOP,SOWKION G CLO
 K SOWKION I $D(IO("Q")) S ZTDESC="INQUIRY INTO RCH FILE REPORT",ZTRTN="ENQ^SOWKHINC" F G="COM","DA" S:$D(@G) ZTSAVE(G)=""
 I  K IO("Q") D ^%ZTLOAD W:$D(ZTSK) !!,"Task Queued to Print",! K ZTSK,G,DA,DIC G CLO Q
ENQ S A=^SOWK(652,DA,0),SWN=$P(A,"^"),SWSN=$P(A,"^",3),SWW=$P(A,"^",4),SWNB=$P(A,"^",11),Y=$P(A,"^",13) X ^DD("DD") S SWD=Y,SWW=$P(^VA(200,SWW,0),"^"),STA=$P(A,"^",7),STA=$P(^DIC(5,STA,0),"^")
 U IO W:$Y @IOF W "RCH HOME REGISTRY FOR A SINGLE HOME: ",$S($G(COM):"COMPLETE",1:"SUMMARY")
 W !!,"HOME: ",?25,SWN,!,"LAST ASSESSMENT DATE: ",?25,SWD,!,"SOCIAL WORKER: ",?25,SWW,!,"NUMBER OF BEDS: ",?25,SWNB
 W !,"ADDRESS 1: ",?25,$P(A,"^",5),!,"ADDRESS 2: ",?25,$P(A,"^",14),!,"CITY: ",?25,$P(A,"^",6),!,"STATE: ",?25,STA,!,"ZIP: ",?25,$P(A,"^",8),!,"TELEPHONE 1: ",?25,$P(A,"^",9),!,"TELEPHONE 2: ",?25,$P(A,"^",15) G:'$G(COM) CLO
 W !,"LICENSED BY STATE ?: ",?25,$P(A,"^",10),!,"VETS ONLY ?: ",?25,$P(A,"^",12),!
CLO W ! W:$E(IOST)'["C" @IOF D ^%ZISC K A,COM,SWNB,DA,DIC,IOP,POP,%ZIS,STA,SWD,SWN,SWSN,SWW,X,Y D:$D(ZTSK) KILL^%ZTLOAD Q
