VFDHHLOT ;DSS/PDW - EDI TRANSFORMS FM <-|-> HL7 DATA AS PER FMHL VALUE 'FM' OR 'HL' ;09 Mar 2011 13:31
 ;;2012.1.1;VENDOR - DOCUMENT STORAGE SYS;;24 Jun 2013;Build 136
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 06/23/15 * vxdev64v2k8_VX13 * 2012.1 T22
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 02/02/14 * vxdev64v2k8_DEVXOS * 2012.1.1T21
 ; variables with 'INT' as a part of the name are referring to the intermediate
 ; array structure as used by HLO
 ;SEGSET(VFDHSYS,VFDHSGNM,.VFDHINT) build .VFDHINT from local variables
 ;SEGGET(VFDHSYS,VFDHSGNM,.VFDHINT) ;pull HL7 data into local FM variables from HL7 VFDHSEG
 ;PARSE(VFDHRSLT,VFDHSEG,VFDHFLAG) ; pass in a HL7 delimited string, create an array @VFDHRSLT@(E,R,C,S)=val
 ;BUILD(VFDHRSLT,.VFDHINT) ; BUILD/ASSEMBLE SEGMENT FROM VFDHINT(E,R,C,S)=VAL
 ;internal call points
 ;if table & FMHL="HLT" returns HL7^text for following:
 ;$$TRANS(VFDHSYS,DTYP,VAL,FMHL,VFDTFLD) ;$$transform data FM<-|->HL7 per FMHL flag "FM" or "HL"
 ;$$TABVALTR(VFDHSYS,TAB,VAL,FMHL,VFDTFLD) ;$$ transform table value FM<==>HL7 if FMHL="HLT" returns HL7^text
 ;TABLIST(.VFDHLIST,VFDHSYS,HL7TAB,NUM,FR,SCREEN) ; RPC list table entries (packed)  LIST^DIC
 ;TABVALDA(.VALIEN,VFDHSYS,HL7TAB,VALUE) ; test /return table VALIEN of entry VALUE
 ;TABHL7DA(VFDHSYS,HL7TAB,VALUE) ;;$$RETURN IEN/DA FOR SYS,TAB,VAL
 ;TABPATH(.VFDHRTN,VFDHSYS,VFDHTAB) return file number^IENS to reach a HL7X table
 ;SEGPATH(.VFDHRTN,VFDHSYS,VFDHSEG) return file number^IENS to reach a HL7X SEGMENT
 ;INTTFORM(.VFDHRSLT,.VFDHIN,VFDHFLAG) ;FLAG=+-1 array INT=>(+-1,1,1,1)
 ;SPLIT(.VFDHWP,VFDHSEG,VFDHLEN) ; split a long segment cleanly into array(n)
 ;SPLIT16(VFDHWP,VFDHSEG,VFDHLEN) ;(.VFDHWP,VFDHSEG,VFDHLEN) split a long segment cleanly
 ;COMB(.VFDHSEG,.VFDHWP) ;combine array(n) into a segment
 ;ROWTABED(VFDHSYS,VFDHTAB) ;pass in system and table to edit a table row
 ;READMSG ; Read in a message, parse, transform and write out the message
 ;ADDHL16(HL7TMP,HL16) ; add segment array to HL7 message storage location in HL7TMP
 ;TABVALLK(VFDHRSLT,VFDHSYS,VFDHTAB) ; ask user to select an entry from a table
 ;HLWPSPLIT(VFDHSEG,VFDHWP) ; split a HL 1.6 type of WP storage
 ;TBROWADD(VFDHRSLT,VFDHSYS,VFDHTAB,VFDHVAL) ;VAL(I)=FIELD I ;.01-COL 1;.02-COL 2;.03-COL 3 ...
 ;BRIEF(VALUE) ;$$ Brief returns the Value with AEIOU removed
 ;PATH(VFDHSFIL,VFDHFLD) ;$$return brief path of SFILE:FIELD
 ;VIMRPC(.VFDRXLT,VFDVIMDA) ;RPC return demographics of a V IMMUNIZATION
SEGSET(VFDHSYS,VFDHSGNM,VFDHSEG) ;Uses designated variables in partition and
 ; Programmer can pre-set VAR("HL")=HL7 value and it will be used.
 N LOC,VAR,TYP,F,R,C,SC,V,VALUE,REP,X,DEF,VALIEN,VFDSYSNM
 I VFDHSYS'=+VFDHSYS S VFDHSYS=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 I 'VFDHSYS S VALIEN="-1^SYSTEM NOT FOUND" Q
 K DEF N DEF
 D SEGDEF(VFDHSYS,VFDHSGNM,.DEF)
 D SET^HLOAPI(.VFDHSEG,VFDHSGNM,0,1,1,1)
 S IEN=0 F  S IEN=$O(DEF(IEN)) Q:IEN'>0  D
 . ;pull location #.01, DataType #.04, variable name #.08
 . S VAR=DEF(IEN,.08),LOC=DEF(IEN,.01),TYP=DEF(IEN,.04),REP=DEF(IEN,.07)
 . ;pull field, component, subcomponent, repeat locations from table location
 . S F=+$P(LOC,"."),C=+$P(LOC,".",2),SC=+$P(LOC,".",3),R=+$P(LOC,".",4)
 . S:'F F=1 S:'C C=1 S:'SC SC=1 S:'R R=1
 . ; check for ALPHA
 . I TYP["*" S VALUE=$P(TYP,"*") D SET^HLOAPI(.VFDHSEG,VALUE,F,C,SC,R) Q
 . ; check for variable
 . I $L(VAR)=0 Q  ;no variable named
 . I '$D(@VAR) Q  ;variable not populated
 . K V M V=@VAR
 . ; check for variable("HL") means HL7 value provided no transform needed
 . ;Check for repetativeness REP=YES
 . I REP="YES" D  Q  ;repetative elements loaded
 . .N R S R=0 F  S R=$O(V(R)) Q:R'>0  D
 . .. I $D(V(R,"HL")) S VALUE=V(R,"HL") D SET^HLOAPI(.VFDHSEG,VALUE,F,C,SC,R) Q
 . .. S X=V(R) S VALUE=$$TRANS(VFDHSYS,TYP,X,"HL") Q:VALUE=""
 . .. D SET^HLOAPI(.VFDHSEG,VALUE,F,C,SC,R)
 . I $D(V)=0 Q
 . I $D(V("HL")) S VALUE=V("HL") D SET^HLOAPI(.VFDHSEG,VALUE,F,C,SC,R) Q
 . S X=V S VALUE=$$TRANS(VFDHSYS,TYP,X,"HL") Q:VALUE=""
 . D SET^HLOAPI(.VFDHSEG,VALUE,F,C,SC,R)
 Q
SEGGET(VFDHSYS,VFDHSGNM,VFDHSEG) ;pull HL7 data into local FM variables
 ; .VFDHSEG is in the HLO parsed array format
 ; Creates local variables as per HL7 Exchange Segment Definition,
 ; Also Creates an array XVAR("local variable name")=""
 S VFDHSEG("FIELD SEPARATOR")="|"
 I VFDHSYS'=+VFDHSYS S VFDHSYS=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 I 'VFDHSYS Q
 K DEF N DEF
 D SEGDEF(VFDHSYS,VFDHSGNM,.DEF)
 ;.01=Location,.02=Definition,.04=DataType,.08=variable,.07=repetative 'YES'
 K XVAR
 S IEN=0 F  S IEN=$O(DEF(IEN)) Q:IEN'>0  S VAR=DEF(IEN,.08) I $L(VAR) D
 .S LOC=DEF(IEN,.01),DTYP=DEF(IEN,.04) I $E(DTYP,$L(DTYP))="*" S DTYP="ST" ;accomodate SETCHAR* in message
 .S F=$P(LOC,"."),C=$P(LOC,".",2),S=$P(LOC,".",3),R=$P(LOC,".",4)
 .;Check for field repetative
 .I DEF(IEN,.07)="YES" D  Q  ; repetative processed into VAR(R)=value
 .. N R S R=0 F  S R=$O(VFDHSEG(F+1,R)) Q:R'>0  D
 ...S VAL=$$GET^HLOPRS(.VFDHSEG,F+1,C,S,R)
 ...S VAL=$$TRANS(VFDHSYS,DTYP,VAL,"FM")
 ...S @VAR@(R)=VAL,XVAR(VAR,R)=VAL
TGET .S VAL=$$GET^HLOPRS(.VFDHSEG,F+1,C,S,R)
 .;W ! ZW F,C,S,R,VAL M DEFIT=DEF(IEN) ZW DEFIT,VFDHSEG(F+1) B  ;***
 .S VAL=$$TRANS(VFDHSYS,DTYP,VAL,"FM")
 .S @VAR=VAL,XVAR(VAR)=VAL
 K VFDHSEG("FIELD SEPARATOR")
 Q
DTM(DTM,FMHL) ; transform time depending on desired output flag FMHL=FM or HL
 I '$D(HLECH) N HLECH S HLECH="^~\&"
 Q $S(FMHL="FM":$$FMDATE^HLFNC(DTM),FMHL="HL":$$HLDATE^HLFNC(DTM),1:"DATA TRANSFORM ERROR")
 ;
NAME(NM,FMHL) ;transform name depending on desired output flag FMHL=FM or HL
 I '$D(HLECH) N HLECH S HLECH="^~\&"
 Q $S(FMHL="FM":$$FMNAME^HLFNC(NM),FMHL="HL":$$HLNAME^HLFNC(NM),1:"DATA TRANSFORM ERROR")
 ;
DR(DR,FMHL) ;transform date range dt1/dt2 depending on output flag FMHL
 I '$D(HLECH) N HLECH S HLECH="^~\&"
 N X1,X2
 I FMHL="FM" D  Q DR
 . S X1=$P(DR,"/"),X2=$P(DR,"/",2)
 . S X1=$$DTM(X1,"FM"),X2=$$DTM(X2,"FM")
 . S DR=X1_"/"_X2
 I FMHL="HL" D  Q DR
 . S X1=$P(DR,"/"),X2=$P(DR,"/",2)
 . S X1=$$DTM(X1,"HL"),X2=$$DTM(X2,"HL")
 . S DR=X1_"/"_X2
 Q "DATA TRANSFORM ERROR"
 ;
TRANS(VFDHSYS,DTYP,VAL,FMHL,VFDTFLD) ;$$ transform data FM<-|->HL7 per FMHL flag
 ;
 ; VFDHSYS   The HL7 exchange IEN, name, or synonym
 ;
 ; DTYP      The translation table name or synonym
 ;           The Data type DT, TS, ST, DR, EI, ID, NM if not a table
 ;
 ; VAL       The value to be translated.
 ;
 ; FMHL      "FM" bring back the FM or vxVistA value
 ;           "FMT" bring back the FM or vxVistA value and _"^"_title
 ;           "HL" bring back the HL7 value.
 ;           "HLT" bring back the HL7 value "^" title from the table.
 ;
 ; VFDTFLD   The field of interest in the table, bring back that value.
 ;
 ;Test for table type is done by seeing if +DTYP=DTYP if YES its a table
 ;also checks table mnemonic/acronym
 ;if table & FMHL="HLT" returns HL7^text
 ; if $G(VFDTFLD) return the value of the field VFDTFLD from the table
 S VFDTFLD=$G(VFDTFLD)
 N IENS,HL7TAB,I,FIELD,NUMBER,IEN,IEN60,VFDSYSDA,VFDSYSNM
 S VFDSYSDA=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 I 'VFDSYSDA,'$D(^VFDH(21625.01,+VFDHSYS)) Q "-1^SYSTEM NOT FOUND"
 I 'VFDSYSDA S VFDSYSDA=+VFDHSYS ;T15
 I 'VFDSYSDA Q "-1^SYSTEM NOT FOUND" ;T15
 S VFDHSYS=VFDSYSDA
 S VFDSYSNM=$$GET1^DIQ(21625.01,VFDSYSDA,.01)
 N DA
 S DA(1)=VFDHSYS S IENS=$$IENS^DILF(.DA)
 ;DSS/PDW 25 June 2012 mod to get ONE2MANY
 I FMHL="M" S X=$$ONE2MANY^VFDHHLNW(VFDHSYS,DTYP,VAL,FMHL) Q X
 ; end ONE2MANY enhancement
 I DTYP=+DTYP S X=$$TABVALTR(VFDHSYS,DTYP,VAL,FMHL,VFDTFLD) Q X
 I DTYP'=+DTYP S HL7TAB=$$FIND1^DIC(21625.0105,IENS,"XM",DTYP)
 I $G(HL7TAB) S X=$$TABVALTR(VFDHSYS,HL7TAB,VAL,FMHL,VFDTFLD) Q X
 I DTYP="" Q "DATA TRANSFORM ERROR"
 I '$D(^VFDH(21625.01,VFDHSYS,20,"B",DTYP)) Q "-1^"_DTYP_" DATA TYPE NOT FOUND IN "_VFDHSYS
 N VFDHTPDA S VFDHTPDA=$O(^VFDH(21625.01,VFDHSYS,20,"B",DTYP,0))
 N DA,X,IENS,VFDHFLD,XCODE
 S DA(1)=VFDHSYS,DA=VFDHTPDA S IENS=$$IENS^DILF(.DA)
 S FMHL=$E(FMHL,1,2)
 S VFDHFLD=$S(FMHL="FM":.03,FMHL="HL":.04,1:"")
 N XCODE S XCODE=$$GET1^DIQ(21625.0103,IENS,VFDHFLD),XCODE=$TR(XCODE,"|","^")
 S X=VAL X XCODE
 Q X
 ;
SEGDEF(VFDHSYS,VFDHSGNM,DEF) ;build Segment ELEMENT definitions into DEF(IEN,FLD)=Value
 ;.01=Location,.02=Definition,.04=DataType,.08=variable
 D SEGDEFN(VFDHSYS,VFDHSGNM,.DEF) Q
 ;
 I VFDHSYS'=+VFDHSYS S VFDHSYS=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 N DA,IENS,ELM,IEN
 S DA(1)=VFDHSYS,DA=$O(^VFDH(21625.01,DA(1),10,"B",VFDHSGNM,0))
 Q:'DA "-1^SEG NOT FOUND"
 S IENS=$$IENS^DILF(.DA)
 ; MOVE return ELM(FILE,IENS,FLD)  to ELM(IEN,FLD)=value
 K ELM D GETS^DIQ(21625.0101,IENS,"**","","ELM","ER")
 S IENS=""  F  S IENS=$O(ELM(21625.0102,IENS)) Q:IENS=""  M DEF(+IENS)=ELM(21625.0102,IENS)
 S IEN=0 F  S IEN=$O(DEF(IEN)) Q:IEN'>0  S DEF("B",DEF(IEN,.01),IEN)="",DEF("NM",DEF(IEN,.02),IEN)=""
 Q
TABVALTR(VFDHSYS,TAB,VAL,FMHL,VFDTFLD) ; given value return the transformed value FM<==>HL7
 ; 30JAN11 VFDERR added upon $G(VFDERR), error in computing return NULL
 ; some tables may have a local EHR mapping value that is a mapping element
 ; where external client definitions are not used locally in database
 ; if Local map Value exists "D" index will exist
 ; if table & FMHL="HLT" returns HL7^text
 ; if VFDTFLD it will return that field from the table
 S VFDTFLD=$G(VFDTFLD)
 N XX
 S XX=$$TABVALTR^VFDHHL01(VFDHSYS,TAB,VAL,FMHL,VFDTFLD)
 I $G(VFDERR),$E(XX,1,3)="-1^" S XX=""
 Q XX
 ; 
TABLIST(VFDHLIST,VFDHSYS,HL7TAB,NUM,FR,SCREEN) ; list table packed entries LIST^DIC
 ;Find sys
 K LIST S LIST=""
 S NUM=$G(NUM) I '$D(FR) S FR=""
 S SCREEN=$G(SCREEN)
 D TABLIST^VFDHHL02(.VFDHLIST,VFDHSYS,HL7TAB,NUM,.FR,SCREEN)
 Q
TABVALLK(VFDHRSLT,VFDHSYS,VFDHTAB) ; ask user to select an entry from a table
 N DA,TABDA,TABNM,TABROOT,DIC
 K VFDHRSLT
 I VFDHSYS'=+VFDHSYS S VFDHSYS=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 I 'VFDHSYS S VFDHRSLT="-1^SYSTEM NOT FOUND" Q
 N DA S DA(1)=VFDHSYS S IENS=$$IENS^DILF(.DA)
 I VFDHTAB=VFDHTAB S TABDA=VFDHTAB
 I VFDHTAB'=+VFDHTAB S TABDA=$$FIND1^DIC(21625.0105,IENS,"XM",VFDHTAB)
 S IENS=TABDA_IENS
 S TABNM=$$GET1^DIQ(21625.0105,IENS,.01)
 S DA(2)=VFDHSYS,DA(1)=TABDA S IENS=$$IENS^DILF(.DA)
 S TABROOT=$$ROOT^DILFD(21625.0106,IENS)
 K DIC S DIC=TABROOT,DIC(0)="AEQMZ"
 S DIC("W")="W ?15,$P(^(0),U,2),?40,$P(^(0),U,3,6)"
 D ^DIC
 ;S TABROOT=$E(TABROOT,2,999)
 ;K DIR S DIR(0)="PO^"_TABROOT_":AEQMZ",DIR("A")=TABNM D ^DIR
 M VFDHRSLT=Y
 ;D Q("VFDHRSLT")
 Q
TABVALDA(VFDHIEN,VFDHSYS,HL7TAB,VALUE) ; test /return table VALIEN of entry VALUE
 N IENS,DA,VALIEN
 S VALIEN=$G(VALIEN),VFDHSYS=$G(VFDHSYS),HL7TAB=$G(HL7TAB),VALUE=$G(VALUE)
 D TABVALDA^VFDHHL02(.VFDHIEN,VFDHSYS,HL7TAB,VALUE)
 Q
TABHL7DA(VFDSYS,HL7TAB,VALUE) ;;$$RETURN IEN FOR SYS,TAB,VAL
 N VFDHIEN S VFDIEN="-1^NOT FOUND"
 D TABVALDA(.VFDHIEN,VFDSYS,HL7TAB,VALUE)
 Q $G(VFDHIEN)
PAD(XXX) ;$$ pad location value 2.3.4.1 for alpha viewing
 N Y,X S Y="" F I=1:1:4 S X=$P(XXX,".",I) S X=$E("  ",$L(X),2)_X S $P(Y,".",I)=X
 Q Y
ZW(VARXX) ; view array in a ZW manner legally with $Q
 N X,X1 S (X,X1)=VARXX
 Q:'$D(@VARXX)
 I $E(X,$L(X))=")" S X1=$E(X,1,$L(X)-1)
 I $D(@X)#10 W !,X,"=",@X
 F  S X=$Q(@X) Q:X'[X1  W !,X,"=",@X
 Q
EXIT ;
 K FLD,IEN,INDX,TABDA,XXX,YYY
 Q
SHOWVAR ; display XVAR
 Q:'$D(XVAR)
 N XX S XX="" F  S XX=$O(XVAR(XX)) Q:XX=""  D ZW(XX)
 Q
CLEARVAR ; display XVAR
 Q:'$D(XVAR)
 N XX S XX="" F  S XX=$O(XVAR(XX)) Q:XX=""  K @XX
 K XVAR
 Q
QUITIN ; receive HL7 message with no processing.
 Q
SEGPRS(VFDHOUT,VFDHIN) ; call from DSIC
 D PARSE(.VFDHOUT,VFDHIN,1)
 Q
ROWTABED(VFDHSYS,VFDHTAB) ;pass in system and table to edit a table row
 N SYSDA,TABDA,FLDS,TABDA,ROWDA,SYSDA,LINE,FLD,CC,CVALS,IENS,VCUR,FLD
 N DA,IENS,DR,DIE,DIC,DIR
 I VFDHSYS'=+VFDHSYS S SYSDA=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 I 'SYSDA W !,"CLIENT SYSTEM NOT FOUND" D E Q
 K DA
 S DA(1)=SYSDA S IENS=$$IENS^DILF(.DA)
 S TABDA=VFDHTAB
 I TABDA'=+VFDHTAB S TABDA=$$FIND1^DIC(21625.0105,IENS,"XM",VFDHTAB)
 I TABDA'>0 W !,"CLIENT SYSTEM NOT FOUND" D E Q
 S ROWDA=$$SELROW(SYSDA,TABDA) Q:ROWDA'>0
 I '$D(LINE) S $P(LINE,"=",79)=""
 K DA
 S DA=TABDA,DA(1)=SYSDA S IENS=$$IENS^DILF(.DA)
 S CC=$$GET1^DIQ(21625.0105,IENS,.06)
 W !,"System:",?10,VFDHSYS,?40,"Table:",?50,VFDHTAB
 I $L(CC) W !,LINE,!,"Alternate Item Names:",!,CC
 K DA
 S DA=ROWDA,DA(1)=TABDA,DA(2)=SYSDA S IENS=$$IENS^DILF(.DA)
 S (DR,FLDS)=".01;.02;.03;.04;.05"
 D ROWDISP
LOOPED K DIC,DIE S DIE=$$ROOT^DILFD(21625.0106,IENS)
 S (DR,FLDS)=".01;.02;.03;.04;.05" D ^DIE
 D ROWDISP
 W ! K DIR S DIR(0)="YO",DIR("A")=" Is the above correct ? Y/N " D ^DIR
 I +Y=1 Q
 G LOOPED
ROWDISP ;
 K FLDS
 I '$D(LINE) S $P(LINE,"=",79)=""
 K DA
 S DA=TABDA,DA(1)=SYSDA S IENS=$$IENS^DILF(.DA)
 S CC=$$GET1^DIQ(21625.0105,IENS,.06)
 I $L(CC) W @IOF,!,LINE,!,"Alternate Field Names:",!,CC
 W !,LINE,!,LINE
 K DA
 S DA=ROWDA,DA(1)=TABDA,DA(2)=SYSDA S IENS=$$IENS^DILF(.DA)
 S FLDS=DR
 K CVALS
 D GETS^DIQ(21625.0106,IENS,FLDS,"","CVALS","MSG")
 K VCUR
 ;display blanks if entry deleted
 I $D(CVALS) M VCUR=CVALS(21625.0106,IENS) I 1
 E  N I W !,?25,">Entry Deleted<" F I=1:1 S FLD=$P(FLDS,";",I) Q:FLD'>0  S VCUR(FLD)=""
 S FLD=0 F  S FLD=$O(VCUR(FLD)) Q:FLD'>0  D
 . D FIELD^DID(21625.0106,FLD,"","LABEL","NM")
 . W !,NM("LABEL"),?30,$G(VCUR(FLD))
 W !,LINE
 Q
SELROW(X,X1) ;EP SELECT TABLE ROW GIVEN SYSTEM & TABLE
 N DA,DIC,Y,IENS
 S DA=X1,DA(1)=X,IENS=$$IENS^DILF(.DA)
 W !,!,"Transport: ",$$GET1^DIQ(21625.01,X_",",.01)
 W !,"Table: ",?12,$$GET1^DIQ(21625.0105,IENS,.01)
 S IENS=","_IENS
 S DIC=$$ROOT^DILFD(21625.0106,IENS),DIC(0)="AEQML",DIC("W")="W ?40,$P(^(0),U,2)"
 K DA S DA(2)=X,DA(1)=X1
 D ^DIC
 Q +Y
E K DIR S DIR(0)="EO",DIR("A")="<CR> - Continue" D ^DIR K DIR Q
 ;
PARSE(VFDHRSLT,VFDHSEG,VFDHFLAG) ; pass in a HL7 delimited string, create an array @VFDHOUT@(E,R,C,S)=VAL
 ;return array VFDHRSLT(E,R,C,S)
 ;if VFDHFLAG=0 zero the parsing starts with 0,1,1,1=SEGNAME
 ;and the array will be built by SET^HLOAPI for $$ADDSEG^HLOAPI
 ;otherwise the array can be passed to SEGGET^VFDHHLOT for HL7 Exchange processing
 ; VFDHRSLT -1^error message if not processed
 N SEG
 S SEG("FIELD SEPARATOR")="|" ; an oddity that this has to be set for processing MSH
 S VFDHFLAG=$G(VFDHFLAG)
 D PARSE^VFDHHL01(.VFDHRSLT,VFDHSEG,VFDHFLAG)
 Q
BUILD(VFDHRSLT,VFDHINT,VFDHFLAG) ; BUILD/ASSEMBLE SEGMENT FROM VFDH.INT(E,R,C,S)=VAL
 ; VFDHRSLT = ONE STRING OR -1^ERROR MESSAGE)
 ; VFDHFLAG(offset)=1 ; build the segment according to subscripting HL7, not HLO with -1
 ; using subscripts equal to the HL7 location notation. E,R,C,S = VALUE
 S VFDHFLAG=$G(VFDHFLAG)
 D BUILD^VFDHHL02(.VFDHRSLT,.VFDHINT,VFDHFLAG)
 Q
SPLIT(VFDHWP,VFDHSEG,VFDHLEN) ;(.VFDHWP,VFDHSEG,VFDHLEN) split a long segment cleanly
 S VFDHLEN=+$G(VFDHLEN)
 D SPLIT^VFDHHL01(.VFDHWP,VFDHSEG,.VFDHLEN)
 Q
SPLIT16(VFDHWP,VFDHSEG,VFDHLEN) ;(.VFDHWP,VFDHSEG,VFDHLEN) split a long segment cleanly
 N I
 S VFDHLEN=+$G(VFDHLEN)
 D SPLIT16^VFDHHL01(.VFDHWP,VFDHSEG,.VFDHLEN)
 Q
COMB(VFDHSEG,VFDHWP) ; combine array(n) into one segment
 D COMB^VFDHHL01(.VFDHSEG,.VFDHWP)
 Q
TABPATH(VFDHRSLT,VFDHSYS,VFDHTAB) ; Return file number^IENS
 D TABPATH^VFDHHL01(.VFDHRSLT,VFDHSYS,VFDHTAB) Q
SEGPATH(VFDHRSLT,VFDHSYS,VFDHSEG) ; Return file number^IENS
 D SEGPATH^VFDHHL01(.VFDHRSLT,VFDHSYS,VFDHSEG) Q
SETVARS(VFDHSYS,VFDHSGNM) ;manually input segment field element variables
 S VFDHSYS=$G(VFDHSYS),VFDHSGNM=$G(VFDHSGNM)
 D SETVARS^VFDHHL01(VFDHSYS,VFDHSGNM)
 Q
READMSG D READMSG^VFDHHL03 Q
MSGLOAD(XXX) ;$$ return with array XXX
 S XXX=$$MSGLOAD^VFDHHL03(.XXX)
 Q XXX
ADDHL16(HL7TMP,HL16) ; add segment array to HL7 message storage location in HL7TMP
 ; HL16 IS HL16(1)="TEXT 1", HL16(1,1)="MORE OF THE SEGMENT", HL16(1,2)=CONTINUING SEGMENT
 N LN,LC,LNS
 S:'$D(@HL7TMP) @HL7TMP@(0)=""  ; originate storage
 S LN=$O(@HL7TMP@("A"),-1)
 I $L($G(HL16(1)))'>0 Q  ;empty
 ;I LN'=1 Q  ; not sequenced properly
 S LN=LN+1
 M @HL7TMP@(LN)=HL16(1) ;set begining of segment
 ; investigate long segment
 Q
BRIEF(VFDHVAL) ; $$RETURN VAL with AEIOU removed
 Q $$BRIEF^VFDHHL01(VFDHVAL)
WPSPLIT(VFDHSEG,VFDHWP) ; split a HL 1.6 type of WP storage 
 ;into distinct long segments
 N LC,LL,NSEG S LC=0,LL=$O(VFDHWP("A"),-1),NSEG=1
 F LC=1:1:LL S VFDHSEG(NSEG)=$G(VFDHSEG(NSEG))_VFDHWP(LC) I VFDHWP(LC)="" S NSEG=NSEG+1
 Q
MVINT(SEGT) ; MOVE int(F,R,C,S) array -1
 ;used to set a freshley parsed HL7 segment array into position for a call to SEGGET
 N NN,SEGX
 S NN=0 F  S NN=$O(SEGT(NN)) Q:NN'>0  M SEGX(NN-1)=SEGT(NN) K SEGT(NN)
 M SEGT=SEGX
 Q
TBROWADD(VFDHRSLT,VFDHSYS,VFDHTAB,VFDHVAL) ;VAL(I)=FIELD I ;.01-COL 1;.02-COL 2;.03-COL 3 ...
 N PATH,FILE,FDA,MSG,VIEN
 S VFDHRSLT="-1^DID NOT FILE"
 D TABPATH^VFDHHLOT(.PATH,VFDHSYS,VFDHTAB)
 S FILE=$P(PATH,U),IENS="+1,"_$P(PATH,U,2)
 M FDA(FILE,IENS)=VFDHVAL
 D UPDATE^DIE("S","FDA","VIEN",.MSG)
 D Q("FDA"),Q("VIEN"),Q("MSG")
 I $D(MSG),$E(IOST)="C" D Q("MSG")
 I '$D(MSG) S VFDHRSLT=1
 Q
Q(VAL) ; DISPLAY VAR WITH $Q
 N X,X1 S (X,X1)=VAL
 Q:'$D(@VAL)
 I $E(X,$L(X))=")" S X1=$E(X,1,$L(X)-1)
 I $D(@X)#10 W !,X,"=",@X
 F  S X=$Q(@X) Q:X'[X1  W !,X,"=",@X
 Q 
SEGDEFN(VFDHSYS,VFDHSGNM,DEF) ;build Segment ELEMENT definitions into DEF(IEN,FLD)=Value
 ;.01=Location,.02=Definition,.04=DataType,.08=variable
 I VFDHSYS'=+VFDHSYS S VFDHSYS=$$FIND1^DIC(21625.01,"","XM",VFDHSYS)
 N DA,IENS,ELM,IEN,ADX,ORD,V,X,DEFDA
 S DA(1)=VFDHSYS,DA=$O(^VFDH(21625.01,DA(1),10,"B",VFDHSGNM,0))
 Q:'DA
 S IENS=$$IENS^DILF(.DA)
 ; MOVE return ELM(FILE,IENS,FLD)  to ELM(+IENS,FLD)=value
 K ELM D GETS^DIQ(21625.0101,IENS,"**","","ELM","ER")
 S IENS=""  F  S IENS=$O(ELM(21625.0102,IENS)) Q:IENS=""  M DEFDA(+IENS)=ELM(21625.0102,IENS) ;restore by DA
 ;generate reorder array and reoder values by V(ord)=IEN
 M ADX=^VFDH(21625.01,DA(1),10,DA,10,"AD") ;grab ordered index
 S X="ADX" F I=1:1 S X=$Q(@X) Q:X=""  S ORD(I)=+$P(X,",",2) ;create order by location
 S ORD=0 F  S ORD=$O(ORD(ORD)) Q:ORD'>0  M DEF(ORD)=DEFDA(ORD(ORD)) ;reorder by V(ord)=ien
 S IEN=0 F  S IEN=$O(DEF(IEN)) Q:IEN'>0  D
 .S:DEF(IEN,.02)="" DEF(IEN,.02)="missing ITEM name "_IEN
 .S DEF("B",DEF(IEN,.01),IEN)="",DEF("NM",DEF(IEN,.02),IEN)=""
 Q
PATH(VFDHFILE,VFDHFLD,VFDHFULL) ;$$return brief path of sfile,field
 ;+VFDHFB=0 BRIEF PATH +FB>0 FULL PATH
 S VFDHFULL=+$G(VFDHFULL)
 ;"2:.02" acceptable in 1st par
 I +$G(VFDHFILE),+$G(VFDHFLD) Q $$PATH^VFDHHL01(VFDHFILE,VFDHFLD,VFDHFULL)
 I VFDHFILE[":" S VFDHFLD=+$P(VFDHFILE,":",2),VFDHFILE=+VFDHFILE I FLD,FILE Q $$PATH^VFDHHL01(VFDHFILE,VFDHFLD,VFDHFULL)
 Q "-1^"_VFDHFILE_","_VFDHFLD_" NOT ACCEPTABLE"
VIMRPC(VFDRSLT,VFDVIMDA) ;
 S VFDRSLT=""
 ;return demographics of a V IMMUNIZATION entry
 D VIMRPC^VFDZARAVVXU2(.VFDRSLT,VFDVIMDA)
 Q
DOCQ ;
 N XX,X S XX="E,C,S,R,DEF(IEN),VAL,VAR"
 W XX
 F I=1:1 S X=$P(XX,",",I) Q:X=""  D Q(X)
 Q
NEW ;new test code area
HISTORY ;
HST1 ;23 Nov 09 move to VFDHHL
 ;14 JAN 11 BUILD,TABLIST,TABVALDA calls moved to VFDHHL02
 ;30Jan11 added VFDERR $G(VFDERR) to TABVALTR to return null on error
 ;4Feb11 corrected X to XX in TABVALTR for VFDERR=1
 ;6 APR 11 VFDHFLAG parameter offset added to BUILD to use subscript notations
 ;  equal to the HL7 location notations of items (E,R,C,S)=VALUE
 ; 16 June 11 added VFDTFLD to TRANS,TABVALTR, also in VFDHHL01 ;
 ;    return the table column field entered
 ;T14 6 OCT 11 SEGGET call to $$GET^HLOPRS(.int,F,C,S,R) changed,
 ;    it now uses HL7 (not $P) subscript references
 ;T15 29OCT11 TRANS needs +VFDHSYS
 ;25 June 2012 increased functionality ONE2MANY
 ;18 June 2013 MSGLOAD moved to VFDHHL03
 ;30 Sep 2013 added "FMT" format for $$TRANS call ; FM, FMT, HL, HLT formats
