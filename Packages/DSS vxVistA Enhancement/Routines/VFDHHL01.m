VFDHHL01 ;DSS/PDW - EXTEND VFDHHLOT ;09 Mar 2011 13:31
 ;;2012.1.1;VENDOR - DOCUMENT STORAGE SYS;;24 Jun 2013;Build 136
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 06/23/15 * vxdev64v2k8_VX13 * 2012.1 T22
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 02/02/14 * vxdev64v2k8_DEVXOS * 2012.1.1T21
PARSE(VFDHOUT,VFDHIN,VFDHFLAG) ; pass in a HL7 delimited string
 ;return array VFDHRSLT(E,R,C,S) (0,1,1,1)=SEGNAME
 ; array can be passed to SEGGET^VFDHHLOT for HL7 Exchange processing
 ; VFDHRSLT -1^m error message if not processed
 K VFDHOUT
 N IN,OUT,N,SEG,ESEG,CSEG,RSEG,SSEG,ELM,REP,SC
 I $D(VFDHIN)>1 S N=0,VFDHIN=$G(VFDHIN) F  S N=$O(VFDHIN(N)) Q:N'>0  S VFDHIN=VFDHIN_VFDHIN(N)
 S SEG=VFDHIN
 N SP
 S SP="|",EC=$L(SEG,SP) ; elements count
 F LE=EC:-1:1 I $P(SEG,SP,LE)'="" Q  ; find LE last element
 F ELMN=1:1:LE S ELM=$P(SEG,SP,ELMN) D REP ; process repetitions
 S VFDHOUT("SEGMENT TYPE")=$G(VFDHOUT(1,1,1,1))
 ;N NN,SEGX
 ;S NN=0 F  S NN=$O(VFDHOUT(NN)) Q:NN'>0  M SEGX(NN-1)=VFDHOUT(NN) K VFDHOUT(NN)
 ;M VFDHOUT=SEGX
 ; return array starting 0.1.1.1
OFFSET I $G(VFDHFLAG) D
 .N VFDTMP,X
 .S X="" F  S X=$O(VFDHOUT(X)) Q:X'>0  I X>0 K VFDOUT(X-1) M VFDHOUT(X-1)=VFDHOUT(X) K VFDHOUT(X)
 D EXIT
 Q
REP ; process a repetition in ELM
 I ELM="^~\&" S VFDHOUT(ELMN,1,1,1)=ELM Q  ; adapt for MSH,FHS,BHS "^~\&"
 N SP,RC,LR,REPN S SP="~",RC=$L(ELM,SP) ; repetitions count
 F LR=RC:-1:1 I $P(ELM,SP,LR)'="" Q  ; find LR last repetition
 F REPN=1:1:LR S REP=$P(ELM,SP,REPN) D COM ; process component
 Q
COM ;process components within a repetition 'REPN of REP in 'ELMN of ELM
 N SP,LC,CC S SP="^",CC=$L(REP,SP)
 F LC=CC:-1:1 I $P(REP,SP,LC)'="" Q  ; find LC last component
 F COMN=1:1:LC S COM=$P(REP,SP,COMN) D SUB
 Q
SUB ; process sub components within a component COM COMN of REP
 N SP,SC,LS,SUBN S SP="&",SC=$L(COM,SP) ; sub components count
 F LS=SC:-1:1 I $P(COM,SP,LS)'="" Q  ; find LS last sub component
 F SUBN=1:1:LS S SUB=$P(COM,SP,SUBN) I SUB'=""  D SET
 Q
SET ; set array as in HLO return (SEG(E,R,C,S)
 S VFDHOUT(ELMN,REPN,COMN,SUBN)=SUB
 ;W ! ZW ELM
 ;D Q($NA(VFDHOUT(ELMN,REPN,COMN,SUBN)))
 Q
EXCELCAP(NUM) ; read in an excel sheet
 S:'$G(NUM) NUM=1
 S NUM=+$O(^TMP("VFDHCHUI",$J,""),-1)
 S NUM=NUM+1
 S G=$NA(^TMP("VFDHCHUI",$J,NUM))
 W !,"Loading Excel into ",G,!
 W !,"ENTER ';;END' manually or as 1st Element of the last row of data sheet",!!
 K DIR S DIR(0)="YO",DIR("A")="<CR> - Continue, '^' Exit" D ^DIR
 Q:X["^"
 S LC=1 W !
 F  R XX:20 Q:XX[";;END"  Q:'$T  S @G@(LC)=XX S LC=LC+1 W !
 W !,LC-1,?10,"rows in ",G
 Q
EXCELDEL ; delete excel captures
 K ^TMP("VFDHCHUI",$J)
 W !,"Excel Captures killed"
 Q
CHUILOAD ; LOAD TMP by reading in through CHUI R X:30
 ; auto-incremenets
 N LC,XX,G
 S G=$NA(^TMP("VFDHCHUI",$J)) K @G
 W !,"Loading CHUI into ",G,!
 W !!!,"ENTER "";;END"" manually or as 1st Element of the last row of data sheet",!!
 K DIR S DIR(0)="YO",DIR("A")="<CR> - Continue, '^' Exit" D ^DIR
 Q:X["^"
 S LC=1 W !
 F  R XX:20 Q:XX[";;END"  Q:'$T  S @G@(LC,0)=XX S LC=LC+1 W !
 W !,LC-1,?10,"rows in ",G
 ;clear data items of excess spaces
SPACEDEL N T,P,LN,L,LL
 S G=$NA(^TMP("VFDHCHUI",$J))
 S LL=$O(@G@("A"),-1),T=$C(9) ;TAB character
 F LN=1:1:LL S X=@G@(LN,0) D  S @G@(LN,0)=X
 . S P=$L(X,T)-1 F I=1:1:P D
 .. S Y=$P(X,T,I),L=$L(Y)
 .. F K=L:-1:1 S C=$E(Y,K) Q:C'=" "  ;find last character
 .. S Y=$E(Y,1,K),$P(X,T,I)=Y
 .. ;code to set site/sample
 Q
SETVARS(VFDHSYS,SEGNM) ;manually input segment field element variables
 S VFDHSYS=$G(VFDHSYS),SEGNM=$G(SEGNM)
 K XVAR
 ;as per HL7 Exchange definitions 
 ;cycle through each element of segment
 N DEF,XX,IEN,VARS,DEF,GG
 N SEGPATH,SEGIEN,SEGPADNM
 W !!,"Use ` to precede input of a raw HL7 Value and skip any transform",!!
 I VFDHSYS'=+VFDHSYS S VFDHSYS=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 I 'VFDHSYS Q
 D SEGDEF^VFDHHLOT(VFDHSYS,SEGNM,.DEF)
 D SEGPATH(.SEGPATH,VFDHSYS,SEGNM) ; of the form 21625.0106^146,1,
 S SEGIEN=+$P(SEGPATH,U,2)
 ;S GG=$NA(^VFDH(21625.01,VFDHSYS,10,SEGIEN,10,"AD")) ; or the form
 ; "AD","  1.  1.  0.  0",21)="" :: "AD","  1.  2.  0.  0",22)=""  
 ;S SEGPADNM="" F  S SEGPADNM=$O(@GG@(SEGPADNM)) Q:SEGPADNM=""  S IEN=$O(@GG@(SEGPADNM,0))  D
 S IEN=0 F  S IEN=$O(DEF(IEN)) Q:IEN'>0  D
 . S XX=DEF(IEN,.08) I '$L(XX) Q
 . K @XX
 . ;checking for repetative field elements 
 . I DEF(IEN,.07)="YES" D  Q  ;loading repetative field element completed
 . . W !!,DEF(IEN,.01),"  ",?15,DEF(IEN,.04),?25,DEF(IEN,.02),?50
 . . K @XX W !!,"Repeating Field:"
 . . N RR F RR=1:1 D  Q:'$D(@XX@(RR))
 . . .;B
 . . .K @XX@(RR)
 . . .W !!,"REP: "_RR_" value for "_XX_" "_DEF(IEN,.02)
 . . .S Y=$$VAREDIT(DEF(IEN,.04)) ; uses DEF(IEN,.04)
 . . . W !,Y
 . . .;K DIR S DIR(0)="FO",DIR("A")="Rep: "_RR_" Value for "_XX D ^DIR
 . . .I (Y="")!($G(DIRUT))!($G(DTOUT)) Q
 . . .I $E(Y)="`" S Y=$E(Y,2,99),@XX@(RR,"HL")=Y,VARS(XX,RR,"HL")=Y Q 
 . . .S @XX@(RR)=Y,XVAR(XX,RR)=Y
 . W !!,DEF(IEN,.01),?15,DEF(IEN,.04),?25,DEF(IEN,.02)
 . S Y=$$VAREDIT(DEF(IEN,.04)) ; uses DEF(IEN,.04)
 . ;K DIR S DIR(0)="FO",DIR("A")="Value for "_XX_" "_DEF(IEN,.02) D ^DIR
 . I (Y="")!($G(DIRUT))!($G(DTOUT)) Q
 . I $E(Y)="`" S Y=$E(Y,2,99),@XX@("HL")=Y,VARS(XX,"HL")=Y Q
 . S @XX=Y,XVAR(XX)=@XX
 Q
VAREDIT(TYPE) ;$$ edit variable mannually being set by DEF(IEN,.04)
 ;use TYPE for table or ask free text
 N YY,Y I $D(VFDHSYS),$D(TYPE) D TABPATH(.RSLT,VFDHSYS,TYPE) I +RSLT K RSLT D  Q YY
 . D TABVALLK^VFDHHLOT(.RSLT,VFDHSYS,TYPE) S YY=$P($G(RSLT(0)),U,2)
 I TYPE="TS" K DIR S DIR(0)="DO" D ^DIR Q Y
 K DIR S DIR(0)="FO^1:30",DIR("A")="Enter External Value: " D ^DIR
 S YY=Y
 Q YY
TABVALEX() ;$$ return the EMR external value of the file being pointed to in
 ;field .07 vxVista file (lookup)
 N VFDFLDA,VFDEMR
 S VFDFLDA=$$GET1^DIQ(21625.0105,D1_","_D0_",",.07,"I")
 I 'VFDFLDA Q ""
 S VFDEMR=$$GET1^DIQ(21625.0106,D2_","_D1_","_D0_",",.03)
 Q $$GET1^DIQ(VFDFLDA,VFDEMR,.01)
 ;
TABVALTR(VFDHSYS,TAB,VAL,FMHL,VFDTFLD) ; given value return the counter value FM<==>HL7
 ; some tables may have a local EHR mapping vaule that is a mapping element
 ; where external client definitions are not used locally in database
 ; if Local map Value exists "D" index will exist
 ; if table & FMHL="HLT" returns HL7^text
 ; if $G(VFDTFLD) return the value of the field VFDTFLD from the table
 I VAL="" Q VAL
 N XX,IEN,IENS,INDX,FLD,XXX,DA,TABDA,YYY,X,SCR
 S XX="-1^Table: "_TAB_" VALUE: "_VAL_"Not Found"
 ; find table
 I VFDHSYS'=+VFDHSYS S VFDHSYS=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 I 'VFDHSYS S XX="-1^SYSTEM NOT FOUND" Q XX
 N DA S DA(1)=VFDHSYS S IENS=$$IENS^DILF(.DA)
 I TAB=+TAB S TABDA=TAB I 1
 E  S TABDA=$$FIND1^DIC(21625.0105,IENS,"XM",TAB)
 I TABDA'>0 Q XX
 ;check for executible code
 ;executable code will need to use X, VFDHSYS, TAB, FMHL in the local partition
 S DA=TABDA,DA(1)=VFDHSYS S IENS=$$IENS^DILF(.DA)
 ;DSS/PWD Test for one to many return FMHL="M" 21 JUNE 2012
 I FMHL="M" N VAL S VAL=$$ONE2MANY^VFDHHLNW Q VAL
 ;DSS/PDW 21 JUNE 2012
TEX ;test for executable code
 I FMHL="FM" S YYY=$$GET1^DIQ(21625.0105,IENS,.03) I $L(YYY) D  Q XX
 . S YYY=$TR(YYY,"|","^"),X=VAL ;X passses VAL into the executable call
 . X YYY ; sets variable X=value of code executed
 . I '$L(X) Q
 . S XX=X
 ;test for executable code
 I FMHL'="FM" S YYY=$$GET1^DIQ(21625.0105,IENS,.04) I $L(YYY) D  Q XX
 . S YYY=$TR(YYY,"|","^"),X=VAL ;X passses VAL into the executable call
 . X YYY ; sets variable X=value of code executed
 . I '$L(X) Q
 . S XX=X
 ;setup new feature add screen code from fields, .03, .04 if desired
 ; code to set SCR="I ..." and call tag screen
SCR ;
 S SCR=$G(SCR) ;add screen code , executes and exits at SCRQ
 ; setup INDEX and return field to be used as requested and presense of EHR values
 ; find exact entry
 ; Check to see if there is local EHR Mappings "D" index and .03 fields exist
 ;I FMHL="FM" S FLD=.02,INDX="B^C^NM" I $D(^VFDH(21625.01,VFDHSYS,30,TABDA,10,"D")) S FLD=.03 ;emr VALUE MAPPED
 ;I FMHL'="FM" S FLD=.01,INDX="C^D^NM"
T0 I FMHL["FM" S FLD=.02,INDX="B^C^NM" I $D(^VFDH(21625.01,VFDHSYS,30,TABDA,10,"D")) S FLD=.03 ;emr VALUE MAPPED
 I FMHL'["FM" S FLD=.01,INDX="C^D^NM"
T K DA S DA(2)=VFDHSYS,DA(1)=TABDA S IENS=$$IENS^DILF(.DA)
 S DA=$$FIND1^DIC(21625.0106,IENS,"XM",VAL,INDX,SCR)
 ;return for no find D SCR^VFDHH01 of QUESTSCR^VFDHHLQ2 of table screen
 I $L(SCR),DA'>0 S X=XX Q
 Q:'DA XX
 S DA(2)=VFDHSYS,DA(1)=TABDA S IENS=$$IENS^DILF(.DA)
 ; return appropriate value
 I $G(VFDTFLD) S FLD=VFDTFLD
 S XXX=$$GET1^DIQ(21625.0106,IENS,FLD)
 I '$L(XXX) Q XX
 ;DSS/PDW FMHL flag [ "T" return text of definition with returned information
 ; if table & FMHL["T" returns HL7^text
 I FMHL["T" S XXX=XXX_"^"_$$GET1^DIQ(21625.0106,IENS,.02)
 ;no longer used:additional info I FMHL=FM get INFO 1 (fld.05) if avalable and append to Value XXX
 ;I FMHL="FM" N INF1 S INF1=$$GET1^DIQ(21625.0106,IENS,.05) S:$L(INF1) XXX=XXX_U_INF1
 ;end DSS/PDS 20131119
SCRQ I $L(SCR) S X=XXX Q  ;looping , this not a $$ exit call
 Q XXX
TABPATH(VFDHRSLT,VFDHSYS,VFDHTAB) ; return file number^IENS
 ; needs VFDHSYS, VFDHTAB
 N VFDHSDA,VFDHTDA,G
 S G=$NA(^TMP("VFDH",$J))
 K @G N DA,VFDHSDA,VFDHTDA
 K VFDHRSLT S VFDHRSLT=""
 S VFDHSDA=VFDHSYS
 I VFDHSYS'=+VFDHSYS S VFDHSDA=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 I 'VFDHSDA S @G@(0)="-1^SYSTEM NOT FOUND: "_VFDHSYS Q
 N DA S DA(1)=VFDHSDA S IENS=$$IENS^DILF(.DA)
 S VFDHTDA=VFDHTAB
 I VFDHTDA'=+VFDHTDA S VFDHTDA=$$FIND1^DIC(21625.0105,IENS,"XM",VFDHTAB)
 I VFDHTDA'>0 S @G@(0)="-1^TABLE NOT FOUND: "_VFDHTAB Q
 K DA S DA(1)=VFDHSDA,DA=VFDHTDA S IENS=$$IENS^DILF(.DA)
 S @G=21625.0106_U_IENS
 S VFDHRSLT=@G
 Q
SEGPATH(VFDHRSLT,VFDHSYS,VFDHSEG) ; return file number^IENS
 ; needs VFDHSYS, VFDHSEG
 N VFDHSDA,VFDHTDA,G,VFDHSGA
 S G=$NA(^TMP("VFDH",$J))
 K @G N DA,VFDHSDA,VFDHTDA
 K VFDHRSLT S VFDHRSLT=""
 S VFDHSDA=VFDHSYS
 I VFDHSYS'=+VFDHSYS S VFDHSDA=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 I 'VFDHSDA S @G@(0)="-1^SYSTEM NOT FOUND: "_VFDHSYS Q
 N DA S DA(1)=VFDHSDA S IENS=$$IENS^DILF(.DA)
 S VFDHSEG=VFDHSEG
 I VFDHSEG'=+VFDHSEG S VFDHSGA=$$FIND1^DIC(21625.0101,IENS,"XM",VFDHSEG)
 I VFDHSGA'>0 S @G@(0)="-1^SEGMENT NOT FOUND: "_VFDHSEG Q
 K DA S DA(1)=VFDHSDA,DA=VFDHSGA S IENS=$$IENS^DILF(.DA)
 S @G=21625.0106_U_IENS
 S VFDHRSLT=@G
 K @G
 Q
EXIT ;
 K CC,COM,COMN,EC,ELMN,ESEG,LC,LE,LR,LS,RC,REPN,SUB,SUBN
 K POP,DTOUT,DIRUT,%ZIS
 Q
INTFORM(VFDHRSLT,VFDHIN,VFDHFORM) ;(.VFDHRSLT,.VFDHIN decrement subscript to move INT array
 ; from (1.1.1.1)=seg to (0.1.1.1)=SEG otherwise SEG.ELM notation
 ; (0,1,1,1) is used to upload to
 I VFDHFORM=1 G INTFORM1
 K VFDHRSLT
 N I S I=""
 F  S I=$O(VFDHIN(I)) Q:I'>0  M VFDHRSLT(I-1)=VFDHIN(I)
 Q
INTFORM1 ; increment subscript to move INT array
 ; from (0,1,1,1)=SEG to (1,1,1,1)=SEG otherwise element pieces
 K VFDHRSLT
 N I S I=""
 F  S I=$O(VFDHIN(I)) Q:I'>0  M VFDHRSLT(I+1)=VFDHIN(I)
 Q
Q(VAL) ; DISPLAY VAR WITH $Q
 N X,X1 S (X,X1)=VAL
 Q:'$D(@VAL)
 I $E(X,$L(X))=")" S X1=$E(X,1,$L(X)-1)
 I $D(@X)#10 U IO W !,X,"=",@X
 F  S X=$Q(@X) Q:X'[X1  U IO W !,X,"=",@X
 Q
SPLIT(WP,SEG,LENGTH) ;(.WP,SEG) split a long segment cleanly 
 ;into array WP(n) lengths<230
 S LENGTH=$S($G(LENGTH):LENGTH,1:230)
 N L,N,SEGL,FC,XX,SEGT,SEGX,I
 S SEGX=SEG
 F I=1:1 D  Q:'$L(SEGX)
 .S SEGL=$L(SEGX) I SEGL<LENGTH S WP(I)=SEGX_"|",SEGX="" Q
 .S SEGT=$E(SEGX,1,LENGTH) S FC=$L(SEGT,"|")-1
 .I FC=0 S WP(I)=SEGT,SEGX=$E(SEGX,LENGTH+1,999) I 1
 .E  S WP(I)=$P(SEGX,"|",1,FC)_"|",SEGX=$P(SEGX,"|",FC+1,999)
 Q
SPLIT16(WP,SEG,LENGTH) ;(.WP,SEG) split a long HL16 segment cleanly
 ;generate the HL16 split array format WP(1), WP(1,1-N) for long segments 
 D SPLIT(.WP,SEG,LENGTH)
 ;W !,SEG,!
 ;ZW WP
 N NWP,I M NWP=WP
 K WP
 S WP(1)=NWP(1)
 F I=2:1 Q:'$D(NWP(I))  S WP(1,I-1)=NWP(I)
 ;ZW WP
 Q
COMB(SEG,WP) ;(.SEG,.WP) combine array elements WP=x,WP(N)=Nval into one variable SEG
 N N
 S SEG=$G(WP) I $D(WP)>1 S N="" F  S N=$O(WP(N)) Q:N=""  S SEG=SEG_WP(N)
 Q
READMSG ;
 ; ask exchange (loop)
 ; read in message
 ; ask printer (loop) (to allow browser)
 W @IOF,!
READ2  W !,"1) Select Client System",!,"2) Load Message",!
 W !,"End message input with <cr>",!
SYS ;select system
 K DIC
 S DIC=21625.01,DIC(0)="AEQMZ" D ^DIC I Y'>0 Q
 S VFDHSYS=+Y
 D MSGLOAD(.XXX) ;I XXX=-1 G SYS
 G ZIS
MSGLOAD(XXX) ;
 K XXX
 K DIR S DIR(0)="YO",DIR("A")="READY TO CONTINUE Y/N ",DIR("B")="Y"
 D ^DIR I Y'>0 S XXX=-1 Q XXX
 K XXX
 W !!,"Paste message in now. Complete loading with <CR> or "";;END""",!!
 F I=1:1 R Y:40 W !! Q:Y="::END"  Q:Y=""  S XXX(I)=Y
 Q
ZIS D ^%ZIS I POP W !!,"Allowing a second message " G SYS
 N VFDHSGNM,VFDHTSEG
 F I=1:1 Q:'$D(XXX(I))  S VFDHSGNM=$E(XXX(I),1,3),VFDHTSEG=XXX(I) D
 . N INT,XXX,I
 . D SEGPRINT(VFDHSYS,VFDHTSEG)
 D ^%ZISC
 W !!,"ALLOWING SECOND PRINT <accomodating DB HL7 Seg changes>"
 G ZIS
 ;
SEGPRINT(VFDHSYS,VFDHTSEG) ; ignore wether or not the variable exists, report location and value
 ;Displays atom items that have either have a value or a variables
 ; Creates an array XVAR("local variable name")=""
 ;S:'$D(SEG("FIELD SEPARATOR")) SEG("FIELD SEPARATOR")="|"
 S VFDHSEG("FIELD SEPARATOR")="|"
 I VFDHSYS'=+VFDHSYS S VFDHSYS=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 I 'VFDHSYS Q
 K DEF N DEF,DTYP,LOC
 D PARSE^VFDHHLOT(.VFDHSEG,VFDHTSEG)
 S VFDHSGNM=$P(VFDHTSEG,"|")
 ;++++++++++++++++++++++++++ NEW SEGPRINT
 ;++++++++++++++++++++++++++ NEW SEGPRINT
 ;move VFDHSEG(x) to VFDMSEG(x-1) to align with exchange locations
 ;N VFDMSEG
 K VFDMSEG
 S E=0 F  S E=$O(VFDHSEG(E)) Q:E'>0  M VFDMSEG(E-1)=VFDHSEG(E)
 ;locat SEG in HL7 Exchange
 N DA,IENS,ELM,IEN,ADX,ORD,V,X,DEFDA
 S DA(1)=VFDHSYS,(VFDSEGDA,DA)=$O(^VFDH(21625.01,DA(1),10,"B",VFDHSGNM,0))
 Q:'DA
 S VFDMIENS=$$IENS^DILF(.DA)
 ;N VFDVCIX,VFDVDA,VFDVLOC,VFDV
 K VFDVCIX,VFDVDA,VFDVLOC,VFDV
 ;build VARV(E,S,C)=DA for segment defined partition variables
SEGV D SEGVBLD ;build variable locations VFDV(E,C,S)=DA
WALK ;
 ;walk VFDMSEG(E,R,C,S) print elements' values and variable definitions
 ; VFDLSTLC contains location last worked with
 N VFDLSTLC S VFDLSTLC=0
 S E=0 F  S E=$O(VFDMSEG(E)) Q:E=""  D
 .S R=0 F  S R=$O(VFDMSEG(E,R)) Q:R'>0  D
 ..S C=0 F  S C=$O(VFDMSEG(E,R,C)) Q:C'>0  D
 ...S S=0 F  S S=$O(VFDMSEG(E,R,C,S)) Q:S'>0  D WALKPRT
 S VFDVLC=VFDLSTLC
 F  S VFDVLC=$O(VFDV(VFDVLC)) Q:VFDVLC'>0  D
 .S VFDITMDA=VFDV(VFDVLC)
 .D ITMPRT(VFDITMDA,"")
 Q
SEGVBLD ;entry to build index array
 ;VFDVCIX("PID(""PTID"")",3)=""
 ;VFDVCIX("PID(""PTLNAME"")",49)=""
 ;VFDVCIX("PID(""PTMARSTAT"")",170)=""
 M VFDVCIX=^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"C")
 S VFDVNM="" F  S VFDVNM=$O(VFDVCIX(VFDVNM)) Q:VFDVNM=""  D VARVECS
 ; finished with data, look for and print remaining VFDV(location) items
 Q
VARVECS ;use C index to get variables
 ;return E,C,S and set VARV((E*100,000,000)+(C*10,000)+S)=VFDVDA
 ;VFDVCIX is the C index of variables
 ; a computed comparable index
 ;VFDV(500010001)=49
 ;VFDV(500010002)=50
 N E,C,S,VFDVDA,VFDVLOC,X
 S VFDVDA=$O(VFDVCIX(VFDVNM,0)),VFDVLOC=$P(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,VFDVDA,0),U)
 S X=VFDVLOC,E=+$P(X,".",1),C=+$P(X,".",2),S=+$P(X,".",3)
 S:C=0 C=1 S:S=0 S=1
 S VFDV((E*100000000)+(C*10000)+S)=VFDVDA ; looks like 10300230011
 Q
WALKPRT ;compare last and current calculated location of VFDMSEG(E,C,S) of message
 ;to VARV(E**+C*+S) of defined variables
 ;if variable definitions exist between the VFDLSTLC
 ;and current data location print the variable definitions
 ; otherwise print segment def
 ; set variable loc start to last know locations
 N VFDITMDA,VFDVLC
 S E=+$G(E),C=+$G(C),S=+$G(S)
 S VFDCURLC=(100000000*E)+(10000*C)+S
 S VFDVLC=VFDLSTLC
 F  S VFDVLC=$O(VFDV(VFDVLC)) Q:VFDVLC'>0  Q:VFDVLC'<VFDCURLC  D
 .S VFDITMDA=VFDV(VFDVLC)
 .D ITMPRT(VFDITMDA,"")
 ;now print imported segment def with value
 N VFDMLOC,VFDITMDA,VFDVAL
 S:'C C=0 S:'S S=0
 S VFDVAL=VFDMSEG(E,R,C,S),VFDMLOC=E_"."_C_"."_S_".0"
 S VFDITMDA=0
ITMDA ;in HLO parse E.R.1.1 is equivalent to E.R.1.0 and E.R.0.0
 ;VFDMLOC="E.C.S.0" uses E,C,S
 I $D(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"B",VFDMLOC)) D
 .S VFDITMDA=$O(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"B",VFDMLOC,0))
 I 'VFDITMDA,S=1 S VFDMLOC=E_"."_C_"."_"0.0" I $D(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"B",VFDMLOC)) D
 .S VFDITMDA=$O(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"B",VFDMLOC,0))
 I 'VFDITMDA,S=1,C=1 S VFDMLOC=E_".0.0.0" I $D(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"B",VFDMLOC)) D
 .S VFDITMDA=$O(^VFDH(21625.01,VFDHSYS,10,VFDSEGDA,10,"B",VFDMLOC,0))
  ;capture/set last Location printed
 S VFDLSTLC=(100000000*E)+(10000*C)+S
 D ITMPRT(VFDITMDA,VFDVAL,VFDMLOC)
 Q
ITMPRT(VFDITMDA,VFDVAL,VFDMLOC) ; (computed location,value)
 ;print variable definition without value
 N VFDV0,VFDVIENS,VFDEFA,VFDEF,VFDMSG
 ; test for item not in definitions
 I 'VFDITMDA D  Q
 .W !,VFDMLOC,?40,VFDVAL
 S VFDVIENS=VFDITMDA_","_VFDSEGDA_","_VFDHSYS_","
 ;.01-LOC;.02-DEF;.04-TYPE;.07-REP;.08-VAR; .05-REQ
 D GETS^DIQ(21625.0102,VFDVIENS,".01;.02;.04;.05;.07;.08",,"VFDEFA","VFDMSG")
 M VFDEF=VFDEFA(21625.0102,VFDVIENS)
 W !,VFDEF(.01),?10,$E(VFDEF(.02),1,19),?30,VFDEF(.04),?37,VFDVAL D
 . W ?49,$E(VFDEF(.07)),?55,VFDEF(.08),?77,$E(VFDEF(.05))
 Q
E N DIR S DIR(0)="EO",DIR("A")="<CR> - Continue" D ^DIR
 Q
GET1(FILE,IENS,FLD) ;
 ;$$GET1^DIQ(FILE,IENS,FIELD,FLAGS,TARGET_ROOT,MSG_ROOT)==>
 ;==>GET^DSICDDR0(DSICP,FILE,IENS,FIELD,FLAGS)
 N RSLT,XXX
 ;D GET^DSICDDR0(.RSLT,FILE,IENS,FLD) S XXX=$P(@RSLT@(1),U,4) K @RSLT
 Q XXX
FIND1(FILE,IENS,FLAGS,VALUE,INDEXES,SCREEN,MSGROOT) ;DIc parameters
 ;calls to this point are now calling
 ;FIND1^DIC(FILE,IENS,FLAGS,VALUE,INDEXES,SCREEN,no>MSGROOT<)
 ;==>FIND^DSICDDR0(DSICP,FILE,IENS,FIELD,NUMBER,VALUE,INDEX,SCREEN,FLAGS) ;
 N XX,X,RSLT
 S XX="FILE,IENS,FIELD,NUMBER,VALUE,INDEXES,SCREEN,FLAGS" F I=1:1 S X=$P(XX,",",I) Q:X=""  S @X=$G(@X)
 S DA=$$FIND1^DIC(FILE,IENS,FLAGS,VALUE,INDEXES,SCREEN)
 Q DA
BRIEF(VFDHVAL) ; $$RETURN VAL with AEIOU removed
 N TXT,I,Y,X,C S TXT="AEIOUaeiou",X=VFDHVAL,Y=$E(X,1) F I=2:1:$L(X) S C=$E(X,I) D
 . S:(TXT'[C) Y=Y_C
 . S:C=" " I=I+1,C=$E(X,I),Y=Y_C
 . S:C=":" I=I+1,C=$E(X,I),Y=Y_C
 Q Y
PATH(SFILE,FIELD,FB) ;$$ RETURN BRIEF PATH GIVEN SUBFILE & FIELD
 ;+FB=0 BRIEF +FB >0 FULL PATH
 N LEV,BPATH,PATH,FLD,LEVN,PFILE,LEVI,FILENM,FLDNM,I
 S LEVI=1
 S FB=$G(FB)
 I '$D(^DD(SFILE,FIELD)) S BPATH="-1^NO DD(SFILE,FIELD)" Q BPATH
 S BPATH="",FLD=FIELD
 I +$P(^DD(SFILE,FLD,0),U,2) S BPATH="-1^A Multiple Picked" Q BPATH
 S FLDNM=$P(^DD(SFILE,FLD,0),U) I ($D(^DD(SFILE,FLD,0,"NM"))) S FLDNM=^DD(SFILE,FLD,"NM")
 S FILENM=$P(^DD(SFILE,0),U) I $D(^DD(SFILE,0,"NM")) S FILENM=$O(^DD(SFILE,0,"NM",""))
 S LEV(1,"FLDNM")=FLDNM,LEV(1,"FILENM")=FILENM
 I $D(^DD(SFILE,0,"UP")) S PFILE=^DD(SFILE,0,"UP") G PFILE
 ;no upper levels - return BPATH
 S PATH=LEV(1,"FILENM")_":"_LEV(1,"FLDNM")
 Q:FB PATH
 S BPATH=$$BRIEF^VFDHHLOT(PATH)
 Q BPATH
PFILE ;loop DD upward ; RETURN BPATH, called from PATH
 S LEVI=LEVI+1
 S FLD=$O(^DD(PFILE,"SB",SFILE,""))
 S SFILE=PFILE
 S FLDNM=$P(^DD(SFILE,FLD,0),U) I ($D(^DD(SFILE,FLD,0,"NM"))) S FLDNM=^DD(SFILE,FLD,"NM")
 S FILENM=$P(^DD(SFILE,0),U) I $D(^DD(SFILE,0,"NM")) S FILENM=$O(^DD(SFILE,0,"NM",""))
 S LEV(LEVI,"FLDNM")=FLDNM,LEV(LEVI,"FILENM")=FILENM
 I $D(^DD(SFILE,0,"UP")) S PFILE=^DD(SFILE,0,"UP") G PFILE
 ;no more upper levels , Gather Path from LEV(LEVI)
 S PATH=""
 F I=LEVI:-1:1 S PATH=PATH_LEV(I,"FILENM")_":"
 S PATH=PATH_LEV(1,"FLDNM")
 Q:FB PATH
 S BPATH=$$BRIEF^VFDHHLOT(PATH)
 Q BPATH
NEWCODE ;AREA
WALKCNT ; S VFDLSTLC=(100000000*E)+(10000*C)+S
 Q
 S X=VFDLSTLC
 S E=78,C=88,S=98
 S X=(100000000*E)+(10000*C)+S
 S E=X\100000000,X=X-(E*100000000) ;ZW E,X
 S C=X\10000,X=X-(C*10000) ;ZW E,C,X
 S S=S ;ZW E,C,S
 Q
HISTORY ;
HST1 ;READMSG allow more than 245 characters
HST2 ;QUEST MAPPING OF SPEC TO SITE
HST3 ;15OCT2010 PARSE VFDHFLAG move INT(1,1,1,1) to INT(0,1,1,1)
HST4 ;10Dec2010 TAVALDA changed to 'B' index only
HST5 ;diagram of HL7 message corrected 4 Jan 11
 ;14 Jan 11 moved calls BUILD,TABLIST, TABVALDA to VFDHHL01
 ;15 APR 11 added required to segment print analysis
 ; 16 June 11 added VFDTFLD to TRANS,TABVALTR, also in VFDHHL01 ;
 ;    return the table column field entered
 ;17 MAY 2013 >PARSE+12 *$G(VFDHRSLT(1,1,1,1))
 ;30 SEP 2013 added "T"-text function to "FM" ==> "FMT" both "HLT" & "FMT" work
