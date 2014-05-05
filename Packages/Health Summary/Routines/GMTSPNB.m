GMTSPNB ; SLC/JER/KER - TIU Brief Progress Notes           ; 04/30/2002
 ;;2.7;Health Summary;**12,28,33,49,55**;Oct 20, 1995
 ;                   
 ; External References
 ;   DBIA 10006  ^DIC  (file #8925.1)
 ;   DBIA 10011  ^DIWP
 ;   DBIA  2902  VISIT^TIULAPIC
 ;   DBIA  2902  MAIN^TIULAPIC
 ;                    
MAIN ; Controls branching and execution
 N PN,GMTSI,GMTSJ,TIUFPRIV,TIUSTAT,TIUTYPE,X,DIWF,DIWL,DIWR,MAX
 K ^TMP("TIU",$J) S MAX=$S(+($G(GMTSNDM))>0:+($G(GMTSNDM)),1:99999)
 S TIUSTAT="COMPLETED",TIUFPRIV=1
 N X,Y S X="PROGRESS NOTES",DIC="^TIU(8925.1,",DIC(0)="X",DIC("S")="I $P($G(^(0)),U,4)=""CL""" D ^DIC S:Y>0 TIUTYPE=+Y
 D EXTIU(DFN,TIUTYPE,GMTS1,GMTS2,MAX,0) Q:'$D(^TMP("TIU",$J))
 D HEADER S GMTSI=0 F  S GMTSI=$O(^TMP("TIU",$J,GMTSI)) Q:+GMTSI'>0!$D(GMTSQIT)  D
 . S GMTSJ=0 F  S GMTSJ=$O(^TMP("TIU",$J,GMTSI,GMTSJ)) Q:+GMTSJ'>0!$D(GMTSQIT)  D
 . . D VARI(GMTSI,GMTSJ)
 . . I $D(^TMP("TIU",$J,GMTSI,GMTSJ,"ZADD")) D ADDEND(GMTSI,GMTSJ)
 . . D WRT
 K ^TMP("TIU",$J)
 Q
 ;
HEADER ; Prints header
 D CKP^GMTSUP Q:$D(GMTSQIT)  W "Prog Note DT",?16,"Title",?48,"Author",?64,"Last Corr DT",!!
 Q
 ;
VARI(GMTSI,GMTSJ) ;Sets variables for display
 S GMTSCNT=+$G(GMTSCNT)+1
 S X=$G(^TMP("TIU",$J,GMTSI,GMTSJ,1301,"I")) D REGDT4^GMTSU S PN("DATE")=X
 S PN("AUTHOR")=$G(^TMP("TIU",$J,GMTSI,GMTSJ,1202,"E"))
 S PN("DOCTYPE")=$G(^TMP("TIU",$J,GMTSI,GMTSJ,.01,"E"))
 I $L(PN("DOCTYPE"))>30 D FORMAT S PN("DOCTYPE")=^UTILITY($J,"W",1,1,0)
 S PN("CORRDT")=""
 Q
 ;
ADDEND(GMTSI,GMTSJ) ;Addenda date display
 N GMTSAD
 S GMTSAD=0
 S GMTSAD=$O(^TMP("TIU",$J,GMTSI,GMTSJ,"ZADD",GMTSAD)) Q:+GMTSAD'>0 
 S X=^TMP("TIU",$J,GMTSI,GMTSJ,"ZADD",GMTSAD,1301,"I")
 D REGDT4^GMTSU S PN("CORRDT")=X
 Q
 ;
WRT ; Writes the component data
 D CKP^GMTSUP Q:$D(GMTSQIT)
 D:GMTSNPG HEADER W PN("DATE"),?16,PN("DOCTYPE"),?48,PN("AUTHOR"),?64,PN("CORRDT"),!
 I $D(^UTILITY($J,"W",1,2,0)) D CKP^GMTSUP Q:$D(GMTSQIT)  W ?16,^UTILITY($J,"W",1,2,0),!
 K PN,^UTILITY($J)
 Q
 ;
FORMAT ; Calls ^DIWP to format Title
 N DIWF,DIWL,DIWR,X
 S DIWF="C30",DIWL=1,DIWR=30,X=PN("DOCTYPE") D ^DIWP
 Q
EXTIU(DFN,GMTST,GMTS1,GMTS2,GMTSN,GMTSX) ; Extract Patient/Visit VIA TIU
 N GMTSPV S GMTSPV=+($G(GMTSPXGO)) I GMTSPV,$L($T(VISIT^TIULAPIC)) D VISIT^TIULAPIC($G(DFN),$G(GMTST),$G(GMTS1),$G(GMTS2),$G(GMTSN),$G(GMTSX)) Q
 D MAIN^TIULAPIC($G(DFN),$G(GMTST),$G(GMTS1),$G(GMTS2),$G(GMTSN),$G(GMTSX))
 Q
