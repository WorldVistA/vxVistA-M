VFDLRPC ;DSS/FHS - GUI FAST BYPASS PROCESSOR ; 2/15/13 6:38am
 ;;2013.1;VENDOR - DOCUMENT STORAGE SYS;**16**;;Build 2
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 Q
 ;Target of GUI RPC to process laboratory test accessioning
 ;Involks LR routines:
 ; EN^LRPARAM,EXP^LRVER1,LRWLST,DT^LRX,$$LRFLDG^VFDLRPC1
 ;Involks:
 ; %ZTLOAD,CHK^DIE,VAL^DIE,$$GET1^DIQ,$$FMADD^XLFDT
 ; $$FMTE^XLFDT,$$NOW^XLFDT
EN ;
 Q
LOOK(LROUT,STR,VFDDUZ,LRXCDT) ;Input point of CPRS accession  ;DSS/RAF 8/25/2014 DUZ => VFDDUZ
 ; Called by RPC: 'VFDLRP POCR ACCESSION'
 ;Input ^OR(100,CPRS ORDER#,4),DUZ,COLLECTION TIME
 ;Return LROUT array in Out(0)
 ; LROUT(0)-1^Line #1 Error message
 ; LROUT(1)=  ^Line #......
 ; LROUT(0)=1 if succesfull
 ;
 N ANS,CNT,DIQUIET,I,LRAA,LRAD,LRAN,LRCDT
 N LRDD,LRDFN,LRDUZ,LREAL,LRGUI,LRINFO,LRLABKY,LRLLOC
 N LRNN,LRNT,LRODT,LROLLOC,LRORD,LRORDR,LRORDTIM
 N LRORIFN,LRPRAC,LRQUIET,LRSAMP,LRSN,LRSPEC
 N LRSTR,LRTEST,LRTST,LRTSTDD,LRUNITS,LRWC
 N MY,N,OUT,OUT1,STR0,STR1,STR60,STR68,X,XX,Y,YY
 ;
 N LROUTINE,LRSTATUS,LRT,OUT2
 N AGE,DFN,DOB,DOD,LRBLOOD,LRDPF,LRDT0,LREND,LRLABKY
 N LRPARAM,LRPCVSO,LRPLASMA,LRRB,LRSERUM,LRTREA
 N LRUNKNOW,LRURINE,LRVIDO,LRVIDOF,LRWRD,PNM,SEX
 N SSN,VA,VADM,VAIN
 N LRAL,LRALERT,LRBEY,LRCFL,LRLDT,LRIDT,LRM
 N LRMX,LRNAME,LRNTN,LRNX,LRPCEVSO,LRSB,LRSTOP,LRSUB
 N LRTM60,S2,STRQ
 N CONTROL,D,D0,D1,DD,DI,H8,LRACC,LRCAPLOC
 N LRIDIV,LRIN,LRIX,LRLBLBP,LRNLT,LRORU3
 N LRPR,LRSS,LRST,LRTN,LRTS,LRTSORU,LRUID,LRURG
 N LRUNQ,LRWL0,LRWLC,LRXTST,S5,OCXSEG
 ;
 ;
EN1 ;
THEO1 ;
 N LRTRAP
 S LRTRAP=$G(^XTMP("LRIN",0))
 I LRTRAP S LRTRAP=LRTRAP+1,^XTMP("LRIN",0)=LRTRAP D
 . M ^XTMP("LRIN",LRTRAP,"STR")=STR
 . M ^XTMP("LRIN",LRTRAP,"DUZ")=DUZ
 . M ^XTMP("LRIN",LRTRAP,"LRXCDT")=LRXCDT
 . M ^XTMP("LRIN",LRTRAP,"LROUT")=LROUT
 K ^TMP("LR",$J)
 I '$D(^VA(200,DUZ,0)) S LROUT(0)="-1^Not a valid user" D LOOKOUT Q
 ;S:'$D(^VA(200,+DUZ,0)) DUZ=48
 I '$$TM^%ZTLOAD S LROUT(0)="-1^TaskMan is not running  Contact Lab POC" D LOOKOUT Q
 I $D(^LAB(69.9,1,"RO")),+$H'=+^("RO") D  Q
 . ;Task ^LROLOVER routine to cleanup files.
 . S LROUT(0)="-1^Rollover has not run - Try later"
 . N ZTSK,ZTRTN,ZTIO,ZTDTH,ZTDESC
 . S ZTRTN="LROLOVER",ZTIO="",ZTDTH=$H,ZTDESC="LAB ROLLOVER GUI"
 . D ^%ZTLOAD
 . I $D(ZTSK) S LROUT(2)="^Rollover has been TASKED"
 . D LOOKOUT
 D DT^LRX
 S LRNT=$$NOW^XLFDT
 S LRGUI=1 ;Flag to indicate verification method
DATE ;Validate date/time 
 I $G(LRXCDT)'="" D  I '$G(LRXCDT) Q
 . N ANS,ERR
 . ;I $E(LRXCDT,1,12)=$E(LRNT,1,12) S $E(LRXCDT,13,14)="" ;Remove Seconds
 . D CHK^DIE(69.01,10,"H",LRXCDT,.ANS,"ERR")
 . K LRXCDT
 . I ANS'>1 D  D LOOKOUT Q
 . . S LROUT(0)="-1^Not A Valid Collection Date/Time"
 . . S LROUT(1)="^Future Date/Time Is Not Allowed"
 . S LRXCDT=ANS
 S:'$G(LRXCDT) LRXCDT=LRNT ;Use now time as collection D/T per Lynn Hoff
 ;S LRDUZ=$G(DUZ) D DUZ^XUP(LRDUZ) ; Get User Characteristics  ;DSS/RAF 8/25/2014 not needed VFDUZ passed is not used
 M LRDUZ=DUZ
 D EN^LRPARAM
 S LRLABKY="1^1^1" ;Laboratory Key
 S LROUTINE=9
 S X=$S(+$P($G(^LAB(69.9,1,0)),U,7):+$P(^(0),U,7),1:1)
 S LRTM60=9999999-$$FMADD^XLFDT(DT,-X) ;Look back date
 S (DIQUIET,LRQUIET)=1,LRORDR="P"
 ; Grab Order variables
 S LRORD=$P(STR,";"),LRODT=$P(STR,";",2),LRSN=$P(STR,";",3)
 S STR0=$G(^LRO(69,LRODT,1,LRSN,0))
 S LREAL="",LRCDT=LRXCDT
 S ^LRO(69,LRODT,1,LRSN,1)=LRCDT_U
 I 'STR0 S LROUT(0)="-1^No Order on Record for "_STR D LOOKOUT Q
GET ;
 ;Get data elements from order
 S LRDFN=$P(STR0,U)
 D PT^LRX
 S LRINFO=$G(^LR(LRDFN,.091))
 S LRSAMP=$P(STR0,U,3),LRSPEC=$P(^LAB(62,LRSAMP,0),U,2)
 S LRWC=$P(STR0,U,4),LRNT=$P(STR0,U,5)
 S LRPRAC=$P(STR0,U,6),LRLLOC=$P(STR0,U,7),LRORDTIM=$P(STR0,U,8)
 S LROLLOC=$P(STR0,U,9),LRORIFN=$P(STR0,U,11)
 ;
 S (LREND,LRXTST)=0
 F  S LRXTST=$O(^LRO(69,LRODT,1,LRSN,2,"B",LRXTST)) Q:LRXTST<1!($G(LREND))  D
 . Q:$P($G(^LAB(60,LRXTST,8,+$G(LRDUZ(2)),0)),U,2)
 . S LREND=1,LROUT(0)="-1^No accession area assigned for "_$P($G(^LAB(60,+LRXTST,0)),U)_" Test"
 I $G(LROUT) D LOOKOUT Q
 D WLST
 Q
WLST ;
 ;Accession Order
 D ^LRWLST
 K ^TMP("LR",$J)
 I $G(LRAA)="" D
 . K STRQ
 . S STRQ=$G(^LRO(69,LRODT,1,LRSN,2,1,0))
 . S LRAA=$P(STRQ,U,4),LRAD=$P(STRQ,U,3),LRAN=$P(STRQ,U,5)
 D EXP^LRVER1
 D ACC
 Q
ACC ;
 ; Gather order/test identifiers
 N ANS,X,ERR
 K LROUT,Y,CNT,LRTST
 ;
 S LRNN=7,LROUT=""
 S Y(1)="PTCOM^"_$$GET1^DIQ(63,LRDFN,.091,"E","ANS","ERR")
 S Y(2)="SPEC^"_$$GET1^DIQ(61,+$G(LRSPEC),.01,"E","ANS","ERR")
 S Y(3)="UID^"_$G(^LRO(68,LRAA,1,LRAD,1,LRAN,.3))
 S Y(4)="ACC^"_$G(^LRO(68,LRAA,1,LRAD,1,LRAN,.2))
 S Y(5)="LAB #^"_$G(^LRO(68,LRAA,1,LRAD,1,LRAN,.1))
 ;S Y(6)="RDATE^"_$$FMTE^XLFDT($$NOW^XLFDT,"1P")
 S LRTST=$O(^TMP("LR",$J,"VTO",0))
 S Y(7)="DNAME^"_$$GET1^DIQ(60,LRTST,.01,"E","ANS","ERR")
 S (CNT,LRTST)=0
 S LRIDT=$P(^LRO(68,LRAA,1,LRAD,1,LRAN,3),U,5)
 F  S CNT=$O(^TMP("LR",$J,"TMP",CNT)) Q:CNT<1  D
 .  S LRNN=$G(LRNN)+1
 .  S LRT=^TMP("LR",$J,"TMP",CNT)
 . ;DSS/FHS - BEGIN MOD - Mod to allow Multi-Normal range enhancement to return appropriate normal range value
 .  S STR60=$G(^LAB(60,LRT,1,LRSPEC,0)) D
 . . N LRNG
 . . S LRNG=STR60 D RANGE^VFDLRX(LRT,LRSPEC)
 . . S STR60=LRNG
 . ;DSS/FHS - END MOD
 .  S LRUNITS=$P($G(^LAB(60,LRT,1,LRSPEC,0)),U,7)
 .  S Y(LRNN)="RNAME^"_$P(^LAB(60,LRT,0),U)_"^"_LRT_"|"_$$GET1^DIQ(60,LRT,13,"I","ANS","ERR")_U_$G(LRUNITS)
 . S Y(LRNN)=Y(LRNN)_U_$$DD(@("^"_$P($P(Y(LRNN),U,3),"|",2)_"0)"))
 . S Y(LRNN)=Y(LRNN)
 . D REVIEW(.OUT2,LRDFN,$P(Y(LRNN),",",2),LRIDT,LRTM60)
 . I OUT2="" S Y(LRNN)=Y(LRNN)_"^"
 . I OUT2'="" S Y(LRNN)=Y(LRNN)_OUT2
 . S Y(LRNN)=Y(LRNN)_"|"_$P(STR60,U,1,5)
 M LROUT=Y
 D
 . S $P(^LRO(69,LRODT,1,LRSN,1),U)=LRCDT
 . S $P(^LRO(69,LRODT,1,LRSN,1),U,3)=LRDUZ
 . S $P(^LRO(69,LRODT,1,LRSN,1),U,4)="C"
 . S $P(^LRO(69,LRODT,1,LRSN,1),U,8)=$G(LRDUZ(2))
 . S ^LRO(69,"AA",+$G(^LRO(69,LRODT,1,LRSN,.1)),LRODT_"|"_LRSN)=""
 K LRTSTS,^TMP("LR",$J,"TMP"),LRNM,DIC,I,LRORIFN,LRBACK
 ;
LOOKOUT ;
 ;Populate data trap if activated
 I $G(LRTRAP) M ^XTMP("LRIN",LRTRAP,"OUT")=LROUT
 Q
DD(STR) ;Parse ^DD(63.04 string for field definition
 N N,X,YY,I,OUT1
 S OUT1=""
 F I=1:1:6 S YY(I)=$P(STR,"^",I)
 I $E(YY(2))="F" D  ;Free text
 . K N I $E(YY(5))="K" D 
 . . S OUT1="|F^"
 . . S N(1)=$P(YY(5),"!"),N(1)=$P(N(1),">",2)
 . . S N(2)=$P(YY(5),"!",2),N(2)=$P(N(2),"<",2),N(2)=$P(N(2),")")
 . . S OUT1=OUT1_"^"_N(2)_"^"_N(1)_"|"
 I $E(YY(2))="N" D  ;Numeric
 . K N
 . S OUT1="|N^",N=""
 . F I=1:1 S X=$E(YY(2),I) S:X>0 N=N_X Q:X=","
 . S X(1)=+$P(YY(2),",",2)
 . S OUT1=OUT1_N_":"_X(1)_"^"
 . I $E(YY(5),1,6)="S Q9=""" D
 . . S YY("X")=$P(YY(5),$E(YY(5),1,6),2),YY("Y")=$P(YY("X"),",")
 . . S YY("XX")=$P(YY("X"),",",2)
 . . S OUT1=OUT1_YY("Y")_":"_YY("XX")_"|"
 . I $E(YY(5))="|K" D
 . . S YY("XX")=+$P(YY(5),">",2)
 . . S YY("Y")=+$P(YY(5),"<",2)
 . . S OUT1=OUT1_YY("Y")_":"_YY("XX")_"|"
 I $E(YY(2))="S" D  ;Set of codes
 . S OUT1="|S^^"_YY(3)_"|"
 Q OUT1
TEST(LRORIFN) ; Used for Unit testing only
 S DATA=$G(^OR(100,LRORIFN,4))
 Q DATA
 ;
VALID(OUT,LRTST,LRTSTDD,VAL,RANGE) ;
 ; Called by RPC 'VFDLRP POCR RESULT VALIDATE'
 ;Call to validate data format
 ;OUT is return value  1 if good OUT(0)=-1^description if fails
 ;multiple error messages format will be OUT(n)="^error message
 ;LRTST = Test Name
 ;LRTSTDD = Test Id passed from LOOK tag  DD(63.04,X
 ;VAL= The data string to be checked.
 ;RANGE = SPEC^HI^LO^CRIT HI^CRTI LOW
 S OUT=$S(VAL="canc":1,VAL="comment":1,VAL="pending":1,1:0)
 I $G(OUT) Q
 N ANS,VALX,LRDD,LRMY
 S VALX=VAL S:$E(VAL,1)="<" VALX=$E(VAL,2,99)
 S:$E(VAL,1)=">" VALX=$E(VAL,2,99)
 S:$E(VAL,1,2)="<=" VALX=$E(VAL,3,99)
 S:$E(VAL,1,2)=">=" VALX=$E(VAL,3,99)
 S LRDD=$P(LRTSTDD,",",2)
 D VAL^DIE(63.04,"",LRDD,"EH",VALX,.ANS,"LRMY(1)")
THEO ;Debug point
 ;S OUT(0)="-1^LINE 1",OUT(1)="^LINE 2",OUT(2)="^LINE 3" Q
 I $D(ANS(0)) S OUT(0)=1_"^"_$$LRFLAG^VFDLRPC1(RANGE,VAL)
 I '$D(ANS(0)) S OUT(0)="-1^"_$G(^DD(63.04,$P(LRTSTDD,",",2),3))
 Q
REVIEW(ROUT,LRDFN,DATANUM,LRIDT,LRTM60) ;
 ; ROUT(DATANUM)=VAL^FLAG
 ; ROUT(DATANUM)="" IF NO RESULTS FOUND
 ; LRDFN=LAB PATIENT
 ; DATANUM=TEST DATA NUMBER (63.04,X
 ; LRIDT = RESULT INVERSE DATE
 ; LRTM60 = LOOK BACK LIMIT
 N LRSB,X,LRLDT
 K Y(6)
 I '$G(LRTM60) S X=$S(+$P($G(^LAB(69.9,1,0)),U,7):+$P(^(0),U,7),1:1) D
 . S LRTM60=9999999-$$FMADD^XLFDT(DT,-X)
 I '$O(^LR(LRDFN,"CH",LRIDT)) S (ROUT,ROUT(DATANUM))="^" Q
 S LRLDT=LRIDT,LRSTOP=0
 S LRSB=DATANUM,ROUT=""
 F  S LRLDT=$O(^LR(LRDFN,"CH",LRLDT)) Q:LRLDT>LRTM60!(LRLDT<0)!($G(LRSTOP))  D
 . I $D(^LR(LRDFN,"CH",LRLDT,LRSB)) D
 . . S (ROUT,ROUT(DATANUM))=$P(^(LRSB),U,1,2),LRSTOP=1
 . . S Y(6)="RDATE^"_$$FMTE^XLFDT($P(^LR(LRDFN,"CH",LRLDT,0),U),"1P")
 Q
