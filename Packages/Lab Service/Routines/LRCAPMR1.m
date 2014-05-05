LRCAPMR1 ;DALISC/J0 - WKLD STATS REPORT - STD/QC/RPT/MAN PRINT ; 4/9/93
 ;;5.2;LAB SERVICE;;Sep 27, 1994
 ;
INITMAN ;Called by: LRCAPMA1,LRCAPML1,LRRP8B
 K ^TMP("LR",$J,"GCOM")
 K ^TMP("LR",$J,"CCOM")
 K ^TMP("LR",$J,"DCOM")
 K ^TMP("LR",$J,"CCN")
 S (LRGSTND,LRGQC,LRGRPT,LRGMANL,LRGCN,LRCCN,LRDCN)=0
 Q
CLNMAN ;Called by: LRCAPMA,LRCAPML,LRRP8
 K ^TMP("LR",$J,"GCOM")
 K ^TMP("LR",$J,"CCOM")
 K ^TMP("LR",$J,"DCOM")
 K ^TMP("LR",$J,"CCN")
 K LRGSTND,LRGQC,LRGRPT,LRGMANL,LRGCN,LRCCN,LRDCN
 Q
PRNTMAN ;Called from LRCAPMA2,LRCAPML2,LRRP8C
 N LRSKIP,LRSTND,LRQC,LRRPT,LRMANL,LRCAPNUM,LRHDR,LRHDR3,LRCLHDR
 S LRHDR="WORKLOAD INPUT MANUALLY"
 S LRHDR3="[Includes all manual workload data for date range]"
 S LRCLHDR="Workload Procedure              Code      STANDARD       QC   REPEAT   MANUAL  "
 D HDR^LRCAPU
 I '((LRGSTND)!(LRGQC)!(LRGRPT)!(LRGMANL)) D
 . W !!,"  *** NO SQRM DATA FOR THIS REPORT ***",!!
 . D:$E(IOST,1,2)="C-" PAUSE^LRCAPU Q:LREND  W @IOF
 . S LRSKIP=1
 Q:$G(LRSKIP)!(LREND)
 S LRCAPNAM=""
 F  S LRCAPNAM=$O(^TMP("LR",$J,"CCN",LRCAPNAM)) Q:(LRCAPNAM="")!(LREND)  D
 . S LRSQRM=$G(^TMP("LR",$J,"CCN",LRCAPNAM,"SQRM",0))
 . S LRSTND=+$P(LRSQRM,U),LRQC=+$P(LRSQRM,U,2),LRRPT=+$P(LRSQRM,U,3)
 . S LRMANL=+$P(LRSQRM,U,4),LRCAPNUM=$P(LRSQRM,U,5)
 . Q:'(LRSTND+LRQC+LRRPT+LRMANL)
 . I $Y+6'<IOSL D NPG^LRCAPU Q:LREND
 . W $E(LRCAPNAM,1,30),?32,LRCAPNUM,?43,$J(LRSTND,7)
 . W ?52,$J(LRQC,7),?61,$J(LRRPT,7),?70,$J(LRMANL,7),!
 Q:LREND
 W !!,"Grand SQRM Totals: ",?43,$J(LRGSTND,7),?52,$J(LRGQC,7)
 W ?61,$J(LRGRPT,7),?70,$J(LRGMANL,7),!
 D:$E(IOST,1,2)="C-" PAUSE^LRCAPU Q:LREND  W @IOF
 Q
BMPMANL ;Count WKLD entered manually
 ;Called by: LRCAPMA1,LRCAPML1,LRRP8B
 S $P(^TMP("LR",$J,"CCN",LRCAPNAM,"SQRM",0),U,5)=LRCAPNUM
 S LRMNODE=$G(^LRO(64.1,LRIN,1,LRCDT,1,LRCC,"S"))
 ;Grand totals for manual stuff
 S LRGSTND=LRGSTND+$P(LRMNODE,U)
 S LRGQC=LRGQC+$P(LRMNODE,U,2)
 S LRGRPT=LRGRPT+$P(LRMNODE,U,3)
 S LRGMANL=LRGMANL+$P(LRMNODE,U,4)
 ;WKLD code totals for manual stuff
 S LRSQRM=$G(^TMP("LR",$J,"CCN",LRCAPNAM,"SQRM",0))
 S $P(LRSQRM,U)=$P(LRSQRM,U)+$P(LRMNODE,U)
 S $P(LRSQRM,U,2)=$P(LRSQRM,U,2)+$P(LRMNODE,U,2)
 S $P(LRSQRM,U,3)=$P(LRSQRM,U,3)+$P(LRMNODE,U,3)
 S $P(LRSQRM,U,4)=$P(LRSQRM,U,4)+$P(LRMNODE,U,4)
 S ^TMP("LR",$J,"CCN",LRCAPNAM,"SQRM",0)=LRSQRM
 Q
GENCOM ;Called by: LRCAPMA1,LRCAPML1,LRRP8B
 S LRCOM=0
 F  S LRCOM=$O(^LRO(64.1,LRIN,2,LRCOM)) Q:'LRCOM  D
 . S LRGCN=LRGCN+1
 . S ^TMP("LR",$J,"GCOM",LRGCN)=$G(^LRO(64.1,LRIN,2,LRCOM,0))
 Q
CAPCOM ;Called by: LRCAPMA1,LRCAPML1,LRRP8B
 S LRCC=0
 F  S LRCC=$O(^LRO(64.1,LRIN,3,LRCC)) Q:'LRCC  D
 . I $G(LRCAPS) Q:'$D(LRCAPS(LRCC))
 . S LRCAPNAM=$$WKLDNAME^LRCAPU(LRCC)
 . S ^TMP("LR",$J,"CCOM",LRCAPNAM,0)=LRCAPNUM
 . S LRCOM=0
 . F  S LRCOM=$O(^LRO(64.1,LRIN,3,LRCC,1,LRCOM)) Q:'LRCOM  D
 . . S LRCCN=LRCCN+1
 . . S ^TMP("LR",$J,"CCOM",LRCAPNAM,LRCCN)=$G(^LRO(64.1,LRIN,3,LRCC,1,LRCOM,0))
 Q
DATCOM ;Called by: LRCAPMA1,LRCAPML1,LRRP8B
 S LRCOM=0
 F  S LRCOM=$O(^LRO(64.1,LRIN,1,LRCDT,2,LRCOM)) Q:'LRCOM  D
 . S LRDCN=LRDCN+1
 . S ^TMP("LR",$J,"DCOM",LRCDT,LRDCN)=$G(^LRO(64.1,LRIN,1,LRCDT,2,LRCOM,0))
 Q
