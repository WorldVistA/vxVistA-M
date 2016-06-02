LRLRRVF ;LAB REFERENCE RANGE VALUE FORMATTING - LAB UTILITY; SLC-GDU; 5/25/07 3:50pm ; 5/29/07 3:21pm
 ;;5.2;LAB SERVICE;**372**;Sep 27, 1994;Build 1
EN(RLV,RHV) ;Entry point for this routine
 ;RLV - Range low value
 ;RHV - Range high value
 ;If both are null return the low value and quit
 ;DSS/FHS - BEGIN MOD - Trap most recent normal range for vertical CUM print
 N VFDTST  ;DSS/RAF 3/19/2015 this variable moved from kill line below
 ;resolves " Could not determine UID" error for reflex tests
 K VFDLRNR,LRNRSP,VFDVAL
 I $G(I),$G(LRMH),$G(LRSH) D
 . S VFDLRNR=+$P($G(^LAB(64.5,1,LRMH,1,1,LRSH,1,I,0)),";",2) Q:VFDLRNR<1
 . S LRNRSP=+$P(^LAB(64.5,1,LRMH,1,1,LRSH,0),U,2)
 I $D(^XTMP("LRNR","U",$J,+$G(VFDLRNR),+$G(LRNRSP))) S VFDVAL=$$TSTRES^VFDLRNR(VFDLRNR,LRNRSP)
 ; If previous normal range is found set HI and Low values  VFDTST is set by the call above
 I +$G(VFDTST) D
 . S RLV=$P(VFDVAL," - "),RHV=$P(VFDVAL," - ",2)
 ;DSS/FHS - END MOD - 3/19/2015
 I RLV="",RHV="" Q RLV
 ;If only the low is defined
 I RLV'="",RHV="" D  Q RLV
 . I RLV=0 S RLV="Ref: >="_RLV Q
 . I ($E(RLV,1,1)="<")!($E(RLV,1,1)=">") S RLV="Ref: "_RLV Q
 . I (RLV?.N.".".N) S RLV="Ref: >="_RLV Q
 . S RLV="Ref: "_RLV
 ;If only the high is defined
 I RLV="",RHV'="" D  Q RHV
 . I RHV=0 S RHV="Ref: "_RHV Q
 . I ($E(RHV,1,1)="<")!($E(RHV,1,1)=">") S RHV="Ref: "_RHV Q
 . I (RHV?.N.".".N) S RHV="Ref: <="_RHV Q
 . S RHV="Ref: "_RHV
 ;If both are defined
 Q RLV_" - "_RHV
 ;;2013.1;VENDOR - DOCUMENT STORAGE SYS;**34**;05 May 2014
