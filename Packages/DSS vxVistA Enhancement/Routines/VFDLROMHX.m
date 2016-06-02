VFDLROMHX ;DSS/RAF - Data migration exception utility;01/26/2015
 ;;2013.1;VENDOR - DOCUMENT STORAGE SYS;**17**;05 May 2014;Build 2
 ;
 ; This routine was designed for programmer use when gathering more details
 ; for Lab Data Migration statistics
 ; At this time it is not intended for general users
 Q
 ;
STATEID(NODE) ; loop thru 21603 to find unique missing MRN's
 ;
 N ARRAY,CNT,NUM,NUM1
 I +$G(NODE)'>0 S NODE=59
 S NUM=0 F  S NUM=$O(^VFD(21603,NUM)) Q:'NUM  D
 . S NUM1=0 F  S NUM1=$O(^VFD(21603,NUM,10,NUM1)) Q:'NUM1  D
 . . I $G(^VFD(21603,NUM,10,NUM1,0))["STATE_ID" D
 . . . S ARRAY(+$P(^VFD(21603,NUM,10,NUM1,0),"(",4))=NUM_"^"_$P(^VFD(21603,NUM,10,NODE,0),$C(31),25)_"^"_$P(^VFD(21603,NUM,10,NODE,0),$C(31),28)
 W !!,"ARRAY(STATEID)=Exception IEN^Case Number^Last name",!
 ZW ARRAY
 S (CNT,NUM)=0 F  S NUM=$O(ARRAY(NUM)) Q:'NUM  S CNT=CNT+1
 W !,"TOTAL UNIQUES = ",CNT
 W !!,"If all the data pieces are not present you can rerun the call"
 W !,"using a parameter pass to look at 60, 59,58 or 57"
 Q NODE
 ;
BIRTHDT(NODE) ; loop thru 21603 to find unique DOB problems
 N ARRAY,CNT,DIERR,NUM,NUM1
 I +$G(NODE)'>0 S NODE=58
 S NUM=0 F  S NUM=$O(^VFD(21603,NUM)) Q:'NUM  D
 . S NUM1=0 F  S NUM1=$O(^VFD(21603,NUM,10,NUM1)) Q:'NUM1  D
 . . I $G(^VFD(21603,NUM,10,NUM1,0))["BIRTH_DT_TM" D
 . . . I $P(^VFD(21603,NUM,10,NUM1,0),"(",4)]"" D
 . . . . S ARRAY($E($P(^VFD(21603,NUM,10,NUM1,0),"(",4),1,10))=NUM_U_$$GET1^DIQ(21603,NUM,.07,"E")_" - "_$P(^VFD(21603,NUM,10,NODE,0),$C(31),23)
 W !!,"ARRAY(DOB)=Exception IEN^Last,First - stateID",!
 ZW ARRAY
 S (CNT,NUM)=0 F  S NUM=$O(ARRAY(NUM)) Q:'NUM  S CNT=CNT+1
 W !,"TOTAL UNIQUES = ",CNT
 W !!,"If all the data pieces are not present you can rerun the call"
 W !,"using a parameter pass to look at 60, 59,58 or 57"
 Q NODE
 ;
UPDPRO ; loop thru 21603 to find unique provider DUZ problems
 N ARRAY,CNT,NUM,NUM1
 S NUM=0 F  S NUM=$O(^VFD(21603,NUM)) Q:'NUM  D
 . S NUM1=0 F  S NUM1=$O(^VFD(21603,NUM,10,NUM1)) Q:'NUM1  D
 . . I $G(^VFD(21603,NUM,10,NUM1,0))["LAST_UPDATE_PROVIDER_ID" D
 . . . I $P(^VFD(21603,NUM,10,NUM1,0),"(",4)]"" D
 . . . . S ARRAY(+$P(^VFD(21603,NUM,10,NUM1,0),"(",4))=NUM
 ZW ARRAY
 S (CNT,NUM)=0 F  S NUM=$O(ARRAY(NUM)) Q:'NUM  S CNT=CNT+1
 W !,"TOTAL UNIQUES = ",CNT
 Q
NMLAST(NODE) ; loop thru 21603 to find unique last name problems
 N ARRAY,CNT,NUM,NUM1,TXT
 I +$G(NODE)'>0 S NODE=58
 S NUM=0 F  S NUM=$O(^VFD(21603,NUM)) Q:'NUM  D
 . S NUM1=0 F  S NUM1=$O(^VFD(21603,NUM,10,NUM1)) Q:'NUM1  D
 . . I $G(^VFD(21603,NUM,10,NUM1,0))["NAME_LAST" D
 . . . I $P(^VFD(21603,NUM,10,NUM1,0),"(",4)]"" D
 . . . . S ARRAY($P(^VFD(21603,NUM,10,NUM1,0),"(",4))=NUM_" - "_$P(^VFD(21603,NUM,10,NODE,0),$C(31),23)
 ; above line uses node 59 for most records, check 58 for those without STATE_ID's
 ZW ARRAY
 S CNT=0,TXT="" F  S TXT=$O(ARRAY(TXT)) Q:TXT=""  S CNT=CNT+1
 W !,"TOTAL UNIQUES = ",CNT
 W !!,"If all the data pieces are not present you can rerun the call"
 W !,"using a parameter pass to look at 60, 59,58 or 57"
 Q NODE
LR63 ; look thru zero nodes for total count of results from data migration
 K CNT,LRIDT,LRDFN,LRSB
 S CNT=0,PAT=0,IDT=0
 S LRDFN=0 F  S LRDFN=$O(^LR(LRDFN)) Q:'LRDFN  D
 . S PAT=PAT+1
 . S LRIDT=0 F  S LRIDT=$O(^LR(LRDFN,"CH",LRIDT)) Q:'LRIDT  D
 . .  S IDT=IDT+1
 . . S LRSB=1.999 F  S LRSB=$O(^LR(LRDFN,"CH",LRIDT,LRSB)) Q:LRSB<1  D
 . . . S CNT=CNT+1
 W !,"Total Test Count: ",CNT
 W !,"Total Accession Count: ",IDT
 Q
SEX(NODE) ; loop thru 21603 to find unique sex_cd problems
 N ARRAY,CNT,NUM,NUM1,NUM2,TXT
 I $G(NODE)'>0 S NODE=60
 S NUM=0 F  S NUM=$O(^VFD(21603,NUM)) Q:'NUM  D
 . S NUM1=0 F  S NUM1=$O(^VFD(21603,NUM,10,NUM1)) Q:'NUM1  D
 . . I $G(^VFD(21603,NUM,10,NUM1,0))["SEX_CD" D
 . . . S NUM2=0 F  S NUM2=$O(^VFD(21603,NUM,10,NUM2)) Q:'NUM2  D
 . . . . I $G(^VFD(21603,NUM,10,NUM2,0))["LN=" D
 . . . . . N NODE S NODE=$G(^VFD(21603,NUM,10,NUM2,0))
 . . . . . S ARRAY($P(NODE,$C(31),23))=NUM_"^"_$P(NODE,$C(31),28)_"^"_$P(NODE,$C(31),27)_"^"_$P(NODE,$C(31),29)
 W !!,"ARRAY(STATEID)=exception IEN^Last^First^Sex Code(362=F,363=M,364&755388=U)",!
 ZW ARRAY
 S CNT=0,TXT=0 F  S TXT=$O(ARRAY(TXT)) Q:TXT=""  S CNT=CNT+1
 W !,"TOTAL UNIQUES = ",CNT
 W !!,"If all the data pieces are not present you can rerun the call"
 W !,"using a parameter pass to look at 60, 59,58 or 57"
 Q NODE
