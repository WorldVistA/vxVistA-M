LRCAPML ;SLC/AM/DALISC/FHS/J0  -  WKLD COST REPORT BY MAJ SCTN; 2/6/91@16:04
 ;;5.2;LAB SERVICE;**59,322**;Sep 27, 1994
EN ;
 ; GET THE PARAMETERS
 S (LRSUMM,LREND)=0,LRNDFN="UNDEFINED"
 D ASKCOM^LRCAPMR2
 D ^LRCAPMR
 I 'LREND D
 . I IO'=IO(0) D LOAD Q
 . D DQ
 D CLEAN
 Q
DQ ;
 K ^TMP("LR-WL",$J)
 S (LREND,LRLOOP,LRBLDONE)=0
 S:$D(ZTQUEUED) ZTREQ="@" K ZTSK
 I 'LRIN S LRLOOP=1
 D EN^LRCAPML1
 D EN^LRCAPML2
 D:$D(ZTQUEUED) CLEAN
 Q
LOAD ;
 S ZTDESC="WKLD STATS BY MAJ SEC REP"
 S ZTRTN="DQ^LRCAPML",ZTSAVE("LR*")=""
 D ^%ZTLOAD
 Q
CLEAN ;
 W !! W:$E(IOST,1,2)="P-" @IOF D ^%ZISC,PRTCLN^LRCAPU,WKLDCLN^LRCAPU,CLNMAN^LRCAPMR1
 K ^TMP("LR-WL",$J)
 K LRANS,LRCC,LRCDT,LRCTM,LRCW,LRUW,LRWC,LRIGT,LRIGTU,LRQC,LRRPT,LRST
 K LRTC,LRMA,LRMAN,LRLSS,LRLSSN,LRUC,LRX,LRTSN,LRPAG,LRBLDONE,LRGETIN
 K LRGT,LRSTD,LRX1,LRX2,LRAGT,LRGTOTS,LRSUMM,LRNDFN,LRAA,LRTYP,LRZTSK
 K LREND,LRIN,LRINN,LRCDTB,LRCTMB,LRDT1,LRCDTE,LRCTME,LRDT2,LRLOOP,LRLAB
 K LRGTREC,LRREC,LRMAA,LRLSSA,LRLDIV,LRAPIGT,LRAPIGTU,LRCPIGT,LRCPIGTU
 K LRFIRST,LRGTU,LRCOM,LRCOMM,LRMNODE,LRSQRM,LRSTU,LRCM
 K ZTDESC,ZTIO,ZTRTN,ZTSAVE,ZTSK,ZTQUEUED,%IO,%ZIS
 K DTOUT,DUOUT,DIRUT,DIC,DIR,%DT,Y1,Y2,DX,DY,X,Y
 Q
