NURA9E1 ;HIRMFO/JH,FT-CATEGORY, GENDER REPORT BY LOCATION ;11/30/89
 ;;4.0;NURSING SERVICE;**13**;Apr 25, 1997
CAT ;SERVICE CATEGORY, GENDER REPORT BY LOCATION
 D HSKEEP G QUIT:$G(NUROUT)
 D EN3^NURSAGSP G:$G(NUROUT) QUIT
 D EN1^NURSAGP0 W ! G QUIT:$G(NUROUT)
 D DEV G:POP!($D(ZTSK)) QUIT
START ;
 K ^TMP($J),^TMP("NURA",$J),^TMP("NURLOC",$J) U IO S (NMCNT,NFCNT,NURPAGE,NURSW1,NURQUIT)=0,NURSSP="",$P(NURSSP," ",132)=""
 S NURSTIL="GENDER BY LOCATION AND SERVICE CATEGORY",NURSTIL1=$E(NURSSP,1,17)_"SERVICE",NURSTIL2="NO.   LOCATION   CATEGORY   NAME"_$E(NURSSP,1,20)_"GENDER"
 S NURROU=3,NRPT=4,NURSLEV=4 W ! D EN3^NURAAGS0,CHK G:$G(NUROUT) QUIT
 D PRINT^NURAGEN G:$G(NX)="^" QUIT
 S NURSORT(1)=0,NURSORT(2)=3,NURSORT(3)="NL1(1)",NURSORT(4)="NL(2)" D GENDER^NURAGEN2,TOTALS
 G QUIT
POS ;SERVICE POSITION, GENDER REPORT BY LOCATION
 D HSKEEP G QUIT:$G(NUROUT)
 W ! D EN2^NURSAGSP G QUIT:$G(NUROUT)
 D EN1^NURSAGP0 W ! G QUIT:$G(NUROUT)
 D DEV G:POP!($D(ZTSK)) QUIT
START1 ;
 K ^TMP($J),^TMP("NURA",$J) U IO S (NURPAGE,NURSW1,NURQUIT,NMCNT,NFCNT)=0,NURSSP="",$P(NURSSP," ",132)=" "
 S NURSTIL="GENDER BY LOCATION AND SERVICE POSITION",NURSTIL1=$E(NURSSP,1,17)_"SERVICE",NURSTIL2="NO.   LOCATION   POSITION   NAME"_$E(NURSSP,1,20)_"GENDER"
 S NURROU=4,NRPT=4,NURSLEV=5 W ! D EN4^NURAAGS0,CHK G:$G(NUROUT) QUIT
 D PRINT^NURAGEN G:$G(NX)="^" QUIT
 S NURSORT(1)=0,NURSORT(2)=3,NURSORT(4)="NL1",NURSORT(3)="NL1(2)" D GENDER^NURAGEN2,TOTALS
 G QUIT
DEV ;
 S ZTDESC="Nursing Gender by Location & Service "_$S($G(ANS2)=1:"Category",1:"Position")
 S ZTRTN=$S($G(ANS2)=1:"START",1:"START1")_"^NURA9E1" D EN7^NURSUT0
 Q
QUIT K ^TMP("NURA",$J),^TMP($J) D CLOSE^NURSUT1,^NURAKILL
 Q
CHK D NODATA^NURAGEN
 Q
HSKEEP ;
 D EN1^NURSAUTL Q:$G(NUROUT)
 I NURMDSW S DIC(0)="AEQZ",NURPLSCR=1 D EN5^NURSAGSP Q:$G(NUROUT)
 I NURMDSW=0,NURPLSW=1 S NURPLSCR=1 D PRD^NURSAGSP K NURPLSCR Q:$G(NUROUT)
 W ! D EN1^NURSAGSP Q:$G(NUROUT)
 Q
TOTALS ;
 I $Y>(IOSL-6) D HEADER^NURAGEN
 W !,?47,$$REPEAT^XLFSTR("-",33)
 D EN4^NURAED3 W !!,"TOTAL NUMBER OF ASSIGNMENTS ",?47,$J(NTCT,4,0),!!,"TOTAL NUMBER OF MALES ",?47,$J(NMCNT,4,0),!!,"TOTAL NUMBER OF FEMALES ",?47,$J(NFCNT,4,0),!!,"TOTAL NUMBER OF PERSONNEL",?47,$J(NTOT,4,0)
 Q
