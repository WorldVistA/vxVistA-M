PSOCST2 ;BHAM ISC/SAB - DRUG COSTS BY DRUG ; 08/19/92 8:38
 ;;7.0;OUTPATIENT PHARMACY;**10,31**;DEC 1997
 ;External Ref. to ^PSDRUG is supp. by DBIA# 221
BEG S RP=2 D HDC^PSOCSTX F  D CDT^PSOCSTX Q:CTR  D DRS^PSOCSTX Q:CTR  S RP=0 D CTP^PSOCSTX Q:CTR  I RP=0 D DEV Q
 D EX Q
DEV D DVC^PSOCSTX Q:$G(CTR)
 K PSOION I $D(IO("Q")) S ZTDESC="DRUG COSTS BY DRUG",ZTRTN="START^PSOCST2" D PAS^PSOCSTX
 I  K IO("Q") D ^%ZTLOAD W:$D(ZTSK) !,"REPORT QUEUED TO PRINT !!",! D EX Q
START U IO K ^TMP($J) F PSDT=(BEGDATE-1):0:ENDDATE S PSDT=$O(^PSCST(PSDT)) Q:'PSDT!(PSDT>ENDDATE)  D @$S('IFN:"DRUG",1:"SDRUG")
 S (CNT,CNTO,CNTR,COST)=0,DRUGX="" D HD I $O(^TMP($J,DRUGX))']"" D HDN^PSOCSTX Q
 F I=0:0 S DRUGX=$O(^TMP($J,DRUGX)) Q:DRUGX=""  D HD:($Y+4)>IOSL Q:$G(CTR)  S Y=^TMP($J,DRUGX),TTX=DRUGX D PRT^PSOCSTX
 I 'CTR,'IFN D HD:($Y+2)>IOSL D FTX^PSOCSTX
EX D EX^PSOCSTX Q
DRUG F DRUG=0:0 S DRUG=$O(^PSCST(PSDT,"D",DRUG)) Q:'DRUG  D SDRUG
 Q
SDRUG I $D(^PSCST(PSDT,"D",DRUG,0)) S X=^(0) D STORE
 Q
STORE Q:'$D(^PSDRUG(DRUG,0))  S DRUGX=$P(^(0),"^") S:'$D(^TMP($J,DRUGX)) ^TMP($J,DRUGX)="^0^0^0" S UTL=^(DRUGX),^TMP($J,DRUGX)="^"_($P(UTL,"^",2)+$P(X,"^",2))_"^"_($P(UTL,"^",3)+$P(X,"^",3))_"^"_($P(UTL,"^",4)+$P(X,"^",4))
 Q
HD D HD^PSOCSTX Q
