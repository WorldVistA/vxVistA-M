DGPTODB2 ;ALB/AS - PTF DRG Breakeven REPORTS (CONT) ; 26 JUN 87  10:00
 ;;5.3;Registration;;Aug 13, 1993
 F DRG=0:0 S:DRG>0&('$D(^UTILITY($J,"DGTC",DRG))) ^UTILITY($J,"DGTC",DRG,DGPAG)="" S DRG=$O(^UTILITY($J,"DGPTFR","D",DRG)) Q:DRG'>0  S Z=^(DRG) D LN
 D WM Q
LN I $Y>$S($D(IOSL):(IOSL-12),1:54) D HD^DGPTODB1
 S (DGHI,DGLODC,DGLODAY,DG1DAY)=0,DGWU=$P(Z,"^",6),DGTD=$P(Z,"^",2),DGLOTRIM=$P(Z,"^",3),DGTL=$P(Z,"^"),DGTT(5)=DGTT(5)+DGTD,DGTT(6)=DGTT(6)+DGTL,(DGA,DGU)=""
 W !,$J(DRG,3),$J(DGLOTRIM,5),$J($P(Z,"^",4),6),$J($P(Z,"^",5),9),$J(DGWU,6),$J($P(Z,"^",7),7) Q:DGRNO=3
 S D5=0 I DGRNO=1 F D=0:0 S D5=$O(^UTILITY($J,"DGPTFR","D",DRG,D5)) Q:D5']""  S %=^(D5) D AA:D5="ABE",BA:D5="BBE"
 I $D(DGBNM) F D=0:0 S D5=$O(^UTILITY($J,"DGPTFR","SB",DGSV,DGBS,DRG,D5)) Q:D5']""  S %=^(D5) D AA:D5="ABE",BA:D5="BBE"
WLN W ?37,"|" W:DGU="B" $J(DGDU,6),$J(DGLU,8),$J(DGLU/DGDU,7,1)," |" W:DGU'="B" ?43,"0       0      0 |" W:DGA="A" $J(DGDA,6),$J(DGLA,8),?70,$J(DGLA/DGDA,7,1) W:DGA'="A" ?66,"0       0      0"
 W ?83,"|",$J(DGTD,6),$J(DGTL,9),$J(DGTL/DGTD,7,1),?107,"|"
 S DGTWW=(DGTD-DG1DAY-DGLODC*DGWU)+(DG1DAY*DG1DAWW)+(DGHI*DGHIWW)+$S(DGLODAY>0&(DGLOTRIM>0):(DGLODAY/DGLOTRIM*DGWU),1:0),DGTT(7)=DGTWW+DGTT(7) W $J(DGTWW,9,2) S X=DGTWW*DGWWCST,X2="2" D COMMA^%DTC W $J(X,13) Q
AA S DGA="A",DGLA=$P(%,"^"),DGDA=$P(%,"^",2),DGHI=$P(%,"^",3),DGTT(3)=DGTT(3)+DGDA,DGTT(4)=DGTT(4)+DGLA Q:DGRNO=1  I '$D(^UTILITY($J,"DGBE",DGSV,DRG,"AA")) S ^UTILITY($J,"DGBE",DGSV,DRG,"AA")=% Q
 S $P(^UTILITY($J,"DGBE",DGSV,DRG,"AA"),"^")=$P(^UTILITY($J,"DGBE",DGSV,DRG,"AA"),"^")+DGLA,$P(^("AA"),"^",2)=$P(^("AA"),"^",2)+DGDA,$P(^("AA"),"^",3)=$P(^("AA"),"^",3)+DGHI Q
BA S DGU="B",DGLU=$P(%,"^"),DGDU=$P(%,"^",2),DG1DAY=$P(%,"^",4),DGLODAY=$P(%,"^",5),DGLODC=$P(%,"^",6),DGTT(1)=DGTT(1)+DGDU,DGTT(2)=DGTT(2)+DGLU Q:DGRNO=1  I '$D(^UTILITY($J,"DGBE",DGSV,DRG,"BA")) S ^UTILITY($J,"DGBE",DGSV,DRG,"BA")=% Q
 S $P(^UTILITY($J,"DGBE",DGSV,DRG,"BA"),"^")=$P(^UTILITY($J,"DGBE",DGSV,DRG,"BA"),"^")+DGLU,$P(^("BA"),"^",2)=$P(^("BA"),"^",2)+DGDU,$P(^("BA"),"^",4)=$P(^("BA"),"^",4)+DG1DAY
 S $P(^UTILITY($J,"DGBE",DGSV,DRG,"BA"),"^",5)=$P(^UTILITY($J,"DGBE",DGSV,DRG,"BA"),"^",5)+DGLODAY,$P(^("BA"),"^",6)=$P(^("BA"),"^",6)+DGLODC Q
PG S %=$S($D(IOSL):(IOSL-12),1:54) F I=$Y:1:% W !
 D BE^DGPTOD1 W !!?64,"-",DGPAG,"-",! Q
SV F DGBS=0:0 D:DGBS>0 WB S DGBS=$O(^UTILITY($J,"DGPTFR","SB",DGSV,DGBS)) Q:DGBS'>0  S DGBNM=^(DGBS) D:$Y>$S($D(IOSL):(IOSL-14),1:52) HD^DGPTODB1 W !!,DGBNM F DRG=0:0 S DRG=$O(^UTILITY($J,"DGPTFR","SB",DGSV,DGBS,DRG)) Q:DRG'>0  S Z=^(DRG) D S,LN
 Q
S S:'$D(^UTILITY($J,"DGTC",DGBNM)) ^(DGBNM,DGPAG)="" I '$D(^UTILITY($J,"DGBE",DGSV,DRG)) S ^UTILITY($J,"DGBE",DGSV,DRG)=Z Q
 F %=1:1:2 S $P(^UTILITY($J,"DGBE",DGSV,DRG),"^",%)=$P(^UTILITY($J,"DGBE",DGSV,DRG),"^",%)+$P(Z,"^",%)
 Q
WT W ?37,"|",$J(DGTT(1),6),$J(DGTT(2),8) S %=$S(DGTT(1)>0:(DGTT(2)/DGTT(1)),1:"") W $S(%]"":$J(%,7,1),1:"      0")," |"
 W $J(DGTT(3),6),$J(DGTT(4),8) S %=$S(DGTT(3)>0:(DGTT(4)/DGTT(3)),1:"") W $S(%]"":$J(%,7,1),1:"      0")," |",$J(DGTT(5),6),$J(DGTT(6),9),$J(DGTT(6)/DGTT(5),7,1)," |",$J(DGTT(7),9,2) S X=DGTT(7)*DGWWCST,X2="2$" D COMMA^%DTC W $J(X,13) Q
WM W !,DGLN,!?3,"Totals for Medical Center:" D WT,PG,TP^DGUTL Q
WS F %=1:1:7 S:DGRNO=3 DGAMT(%)=DGAMT(%)+DGTT(%) I DGRNO=2 S DGTT(%)=DGAMT(%),DGMC(%)=DGMC(%)+DGAMT(%),DGAMT(%)=0
 W !,DGLN,!?3,"Totals for Service:" D WT F %=1:1:7 S DGTT(%)=0
 Q
WB W !,DGLN2,!?3,"Totals for Specialty:" D WT F %=1:1:7 S DGAMT(%)=DGAMT(%)+DGTT(%),DGTT(%)=0
 Q
