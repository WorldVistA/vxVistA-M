LRMIEDZ ;DALOI/STAFF - MICROBIOLOGY EDIT ROUTINE ;05/11/09  10:44
 ;;5.2;LAB SERVICE;**295,350**;Sep 27, 1994;Build 230
 ;
 ; from LRVR and option LRMIEDZ
 ;
BEGIN ;
 S LREND=0,LRACC="",LRSS="MI",LRVT="RE"
 K ^TMP("LA7HDR",$J)
 D EN^LRPARAM,DATE
 ;
 ;
END ; from LRFAST,LRVER
 ;
 I $G(LRCSQ)'="",$O(^XTMP("LRCAP",LRCSQ,DUZ,0)) D STD^LRCAPV
 ;
 ;
ANN ;
 ;
 D:$P(LRPARAM,U,14) ANN^LRCAPV ; Force workload program
 K DD,DR,GLB,LRCAPMS,LRCDEF,LRCDEF0,LRCNT,LRCODE,LRCODEN,LRCSQ,LRCY,LRP,LRPN,LRQC,LRSSC,LRSSCX,LRSTD,LRT,LRTIME,NODE,NODE0,ZTSK
 K %,A6,AGE,C,D,D0,D1,DA,DFN,DIC,DIE,DLAYGO,DOB,DTOUT,DUOUT,DX,I,II,J,K,LRAA,LRACC,LRAD,LRANOK,LRBG0,LRBUG,LRCAPOK,LRCDT,LRDFN,LRDPF,LREAL,LREND,LRFIFO,LRI,LRIDT,LRLLOC,LRMIDEF,LRMIOTH,LRNB,LRODA,LRODIE,LRVT
 K LRODT,LRPTP,LRSAME,LRSB,LRSCREEN,LRSN,LRSPEC,LRSS,LRTEC,LRTS,LRTX,LRUNDO,LRWRD,N,PNM,POP,S,SEX,SSN,X,Y,Z
 K ^TMP("LA7HDR",$J)
 Q
 ;
 ;
DATE ;
 ;
 D ENA^LRWU4("MI") Q:LRAA<1  D:$P(LRPARAM,U,14) ^LRCAPV G:LREND ANN
 K DIC
 S DIC=60,DIC("S")="I $P(^(0),U,4)=""MI""",DIC(0)="AEMOQ",DIC("A")="Select TEST/PROCEDURE: "
 D ^DIC K DIC Q:$D(DUOUT)  S LRPTP=+Y
 I Y<1 W !,"None Preselected",!,"Accession # ",LRAN
 S LRMIDEF=$P(^LAB(69.9,1,1),U,10),LRMIOTH=$P(^(1),U,11)
 D ^LRMIEDZ2
 Q
 ;
 ;
UNDO ; from LRMIEDZ2
 ;
 S $P(^LR(LRDFN,"MI",LRIDT,0),U,9)=1,$P(^(0),U,3,4)=U,$P(^LRO(68,LRAA,1,LRAD,1,LRAN,3),U,4)=""
 S $P(^LRO(68,LRAA,1,LRAD,1,+LRAN,4,LRTS,0),U,4,5)=DUZ_U_DT,^LRO(68,LRAA,1,LRAD,1,"AD",DT,+LRAN)="",^LRO(68,LRAA,1,LRAD,1,"AC",DT,+LRAN)=""
 ;
 D UPDATE^LRPXRM(LRDFN,"MI",LRIDT)
 ;
 Q
