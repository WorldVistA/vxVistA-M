ENWOP3 ;(WASH ISC)/DLM/DH-Work Order Print ;5/8/1998
 ;;7.0;ENGINEERING;**26,35,42,53**;Aug 17, 1993
 ;
 ;  Called by ENWOME2, ENWONEW, ENWOP
 ;  Expects ENDNX (as inverse IEN for Space File - #6928)
 ;
FDAT S ENDNX=$O(^ENG(6920,"AINC",ENSHKEY,ENDNX)) I ENDNX="" S ENQUIT=1 Q
 S DA=9999999999-ENDNX G:'$D(^ENG(6920,DA)) FDAT I 'ENPMINC,$E($P(^(DA,0),U,1),1,3)="PM-" G FDAT
 I $D(^ENG(6920,DA,5)),$P(^(5),U,2)]"" K ^ENG(6920,"AINC",ENSHKEY,ENDNX) G FDAT
 S ENRDA=$P(^ENG(6920,DA,0),U,2)
 S X1=ENTOD,X2=ENRDA D ^%DTC G:X<ENDLQ FDAT
 I ENBY="EMP",ENEMP'=$P($G(^ENG(6920,DA,2)),U,2) G FDAT
 I ENBY="ROOM",ENROOM'=$P(^ENG(6920,DA,0),U,4) G FDAT
 I ENBY="LOC" S SPC=$P(^ENG(6920,DA,0),U,4) G:SPC="" FDAT S X=$$SPACE^ENEQPMS8(SPC) G:+X<0 FDAT
 I ENBY="ONR" G:'$D(^ENG(6920,DA,3)) FDAT I $P(^(3),U,4)'=ENONR G FDAT
 S ENWONX=ENWONX+1,ENWONX(ENWONX)=DA
 D FDAT4
 Q:(IOSL-ENY)'>4
 G FDAT
 ;
FDAT4 ;  Entry point for work order autoprint (optional feature)
 S ENRQR="",ENPRI="",ENTEC="",ENDPR="",ENSTAT="",ENEQ=""
 I $D(ENY)#10'=1 S ENY=""
 S EN=^ENG(6920,DA,0),ENWOR=$P(EN,U,1),ENRDA=$P(EN,U,2),ENLOC=$P(EN,U,4)
 I ENLOC=+ENLOC,$D(^ENG("SP",ENLOC,0)) S ENLOC=$P(^(0),U)
 I $D(^ENG(6920,DA,1)) S EN=^(1),ENRQR=$E($P(EN,U,3),1,15),ENDPR=$P(EN,U,2)
 I $D(^ENG(6920,DA,2)) S EN=^(2),ENPRI=$P(EN,U,3),ENTEC=$E($P(EN,U,2),1,20) I IO=IO(0),$E(IOST,1,2)="C-" W:ENPRI="E"!(ENPRI="H") IOINHI
PMN I $D(^ENG(6920,DA,3)) S ENEQ=$P(^(3),U,8)
 I $D(^ENG(6920,DA,4)) S ENSTAT=$P(^(4),U,3)
 S:ENPRI]"" ENPRI=$E($$EXTERNAL^DILFD(6920,17,"",ENPRI),1,4)
FDAT7 ;
 I ENTEC'="" I $D(^ENG("EMP",ENTEC,0))>0 S ENTEC=$P(^ENG("EMP",ENTEC,0),U,1)
WDAT D WDAT^ENWOP1
 Q
 ;ENWOP3
