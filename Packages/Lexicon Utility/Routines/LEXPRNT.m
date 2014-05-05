LEXPRNT ; ISL Print Utilities for the Lexicon      ; 09-23-96
 ;;2.0;LEXICON UTILITY;;Sep 23, 1996
 ;
XTLK ; XTLK  Display format for MTLU 
 ; Uses   XTLKH, XTLKMULT, XTLKREF0, LEXSHOW
 N LEXIFN,LEXEXP,LEXCODE,LEXSOID
 S LEXIFN=0,LEXEXP=-1 S:'$D(LEXSHOW) LEXSHOW=""
 S:'$D(LEXSUB) LEXSUB="WRD"
 S (LEXEXP,LEXIFN)=+($P(XTLKREF0,",",2)) G:+LEXIFN'>0 XTQ
 D:XTLKMULT MULTI
 D:'XTLKMULT ONE
XTQ K LEXCODE,LEXSOID,LEXIFN,LEXEXP
 Q
MULTI ; Multiple entries on the selection list 
 N LEXNUM,LEXSTR,LEXDP,LEXCCS,LEXL,LEXP
 S LEXNUM=XTLKH,(LEXSTR,LEXDP,LEXCCS)="",LEXL=70,LEXP=7
 D COMMON
 W:LEXNUM>1 ! W:LEXNUM>1&(LEXNUM#5=1) !
 W $J(LEXNUM,4),":" W:$L(LEXSTR)<(LEXL+1) ?LEXP,LEXSTR
 D:$L(LEXSTR)>LEXL LONG
 W:LEXNUM#5=0&(+($G(LEXHLPF))=0) !
 W:LEXNUM#5'=0&(LEXNUM=+($G(^TMP("XTLKHITS",$J))))&(+($G(LEXHLPF))=0) !
 Q
ONE ; One entry on the selection list
 N LEXSTR,LEXDP,LEXCCS,LEXL,LEXP
 S (LEXSTR,LEXDP,LEXCCS)="",LEXL=75,LEXP=2
 D COMMON
 W:$L(LEXSTR)<(LEXL+1) ?LEXP,LEXSTR
 D:$L(LEXSTR)>LEXL LONG
 Q
COMMON ; Parse LEXSHOW for both MULTI and ONE
 S:LEXSUB="WRD" LEXSTR=^LEX(757.01,LEXEXP,0)
 S:LEXSUB'="WRD" LEXSTR=^LEX(757.01,+(@(DIC_LEXEXP_",0)")),0)
 S LEXDP=$S($D(^LEX(757.01,$S(LEXSUB="WRD":LEXEXP,1:+(@(DIC_LEXEXP_",0)"))),3)):" *",1:"")
 I LEXSUB'="WRD" S LEXEXP=+(@(DIC_LEXEXP_",0)"))
 I $D(LEXSHOW),LEXSHOW'="" F LEXSOID=1:1:$L(LEXSHOW,"/") D
 . S LEXCODE=$P(LEXSHOW,"/",LEXSOID) N @LEXCODE S @LEXCODE=""
 . S @LEXCODE=$S(LEXSUB="WRD":$$CODE(LEXIFN,LEXCODE),1:$$CODE(LEXEXP,LEXCODE))
 . I @LEXCODE'="" S LEXCCS=LEXCCS_" ("_@LEXCODE_")"
 S LEXSTR=LEXSTR_LEXDP_LEXCCS
 Q
LONG ; Handle a long string
 N LEXOK,LEXCHR,LEXPSN,LEXSTO,LEXREM,LEXLNN,LEXOLD S LEXLNN=0,LEXOLD=LEXSTR
 F  Q:$L(LEXSTR)<(LEXL+1)  D PARSE Q:$L(LEXSTR)<(LEXL+1)
 S LEXLNN=LEXLNN+1
 W:LEXLNN>1 ! W ?LEXP,LEXSTR
 Q
PARSE ; Parse a long string into screen length strings
 S LEXOK=0,LEXCHR=""
 F LEXPSN=LEXL:-1:0 Q:+LEXOK=1  D  Q:+LEXOK=1
 . I $E(LEXSTR,LEXPSN)=" " S LEXCHR=" ",LEXOK=1 Q
 . I $E(LEXSTR,LEXPSN)="," S LEXCHR=",",LEXOK=1 Q
 . I $E(LEXSTR,LEXPSN)="/" S LEXCHR="/",LEXOK=1 Q
 . I $E(LEXSTR,LEXPSN)="-" S LEXCHR="-",LEXOK=1 Q
 I LEXCHR=" " S LEXSTO=$E(LEXSTR,1,LEXPSN-1),LEXREM=$E(LEXSTR,LEXPSN+1,$L(LEXSTR))
 I LEXCHR="," S LEXSTO=$E(LEXSTR,1,LEXPSN),LEXREM=$E(LEXSTR,(LEXPSN+1),$L(LEXSTR)) S:$E(LEXREM,1)=" " LEXREM=$E(LEXREM,2,$L(LEXREM))
 I LEXCHR="/" S LEXSTO=$E(LEXSTR,1,LEXPSN),LEXREM=$E(LEXSTR,(LEXPSN+1),$L(LEXSTR)) S:$E(LEXREM,1)=" " LEXREM=$E(LEXREM,2,$L(LEXREM))
 I LEXCHR="-" S LEXSTO=$E(LEXSTR,1,LEXPSN),LEXREM=$E(LEXSTR,(LEXPSN+1),$L(LEXSTR)) S:$E(LEXREM,1)=" " LEXREM=$E(LEXREM,2,$L(LEXREM))
 S LEXSTR=LEXREM
 S LEXLNN=LEXLNN+1
 W:LEXLNN>1 ! W ?LEXP,LEXSTO
 Q
CODE(LEXEX,LEXSO) ; Returns codes (defined in XTLK^LEXPRNT) for a Term
 N LEXMC,LEXCREC,LEXI,LEXCID S (LEXI,LEXCID)="",LEXCREC=0
 I '$D(^LEX(757.01,LEXEX)) Q LEXCID
 S LEXMC=$P(^LEX(757.01,LEXEX,1),U,1)
 I LEXSUB="WRD" D
 . F  S LEXCREC=$O(^LEX(757.02,"AMC",LEXMC,LEXCREC)) Q:+LEXCREC=0  D
 . . I $D(^LEX(757.02,"ASRC",LEXSO,LEXCREC)) D
 . . . S LEXI=$P(^LEX(757.02,LEXCREC,0),U,2)
 . . . I LEXI'="NOCODE",LEXI'?1"U"2"0"4N,LEXCID'[LEXI D
 . . . . S LEXCID=LEXCID_"/"_LEXI
 I LEXSUB'="WRD" D
 . F  S LEXCREC=$O(^LEX(757.02,"B",LEXEX,LEXCREC)) Q:+LEXCREC=0  D
 . . I $D(^LEX(757.02,"ASRC",LEXSO,LEXCREC)) S LEXI=$P(^LEX(757.02,LEXCREC,0),U,2) I LEXI'="NOCODE",LEXI'?1"U"2"0"4N,LEXCID'[LEXI S LEXCID=LEXCID_"/"_LEXI
 S:LEXCID'="" LEXCID=LEXSO_" "_$E(LEXCID,2,999)
 K LEXCREC,LEXMC,LEXI
 S LEXEX=LEXCID Q LEXEX
