VFDHHL60 ;DSS/PDW -HL7X LAB 60 STAGING   ; 02 Mar 2012 13:04
 ;;2012.1.1;VENDOR - DOCUMENT STORAGE SYS;;24 Jun 2013;Build 136
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 06/23/15 * vxdev64v2k8_VX13 * 2012.1 T22
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 02/02/14 * vxdev64v2k8_DEVXOS * 2012.1.1T21
EN ;
 W !,"= = = = ="
 W !," 1) FM (Fileman)"
 W !," 2) Load/Merge Excel into scratch file"
 W !," 3) Load/Link Panels & Atomics"
 W !," 4) Populate Lab Files NA-New Atomics, NP-New Panels(call Franko )"
 W !," 5) Populate Singular Orderable IENs in staging file"
 W !," 6) Move Entries to HL7 exchange"
 W !," 7) Auto Instrument File Load"
 W !," 8) Populate a LAB SHIPPING CONFIGURATION" ;(8)
 K DIR S DIR(0)="NO^1:8" D ^DIR
 I Y=1 D FM G EN
 I Y=2 D LOAD^VFDHHL6B G EN
 I Y=3 D LINKPNAT G EN
 I Y=4 D LABPOP G EN
 I Y=5 D SOPOP G EN
 I Y=6 D MOVE G EN
 I Y=7 D AILOAD^VFDHHL6B G EN
 I Y=8 D LSCF^VFDHHL62 G EN
 Q
FM ;chose a template with a call to D DISP2601^VFDHHL60
 D Q^DI
 ;N DIC S DIC="^VFD(21626.01," D 1^DIB
 Q
DISP2601 ;display items from Scratch file and from Lab (60)
 I '$D(DA),$D(D0) N DA S DA=D0
 D XBNEW("ITEM2601^VFDHHL60(DA)","DA")
 Q
ITEM2601(DA) ; includes linked LAB 60 entry
 S IOP="BROWSER CACHE/NT",DDBDMSG="SCRATCH & FILE 60"
 ;D ^%ZIS
 U IO
 K FLDS
 I '$D(LINE) S $P(LINE,"=",79)=""
 W !,LINE,!,LINE
 ;refresh error check
 S VFDFILE=21626.01
 N LRISEQ,LRTAG,LRERR S LRISEQ=DA,LRTAG=""
 D IN^VFDHHL61(LRISEQ) ;clear & test errors
 I 'LRERR D ERR^VFDHHL61(LRISEQ,LRERR,LRTAG) ;set error
 S IENS=DA_","
 S FLDS=".01;.02;.03;.04;.05;.06;.07;.08;.09;.1;.12;.13;.14;.15;.16;.17;.18;.19;.31;.32;.2;.41;.42;.43;.44;.45;10*;20*"
 K CVALS
 D GETS^DIQ(21626.01,IENS,FLDS,"","CVALS","MSG")
 K VCUR,FLDSEQ
 ;display blanks if entry deleted
 I $D(CVALS) M VCUR=CVALS(21626.01,IENS) I 1
 E  N I F I=1:1 S FLD=$P(FLDS,";",I) Q:FLD'>0  S VCUR(FLD)="" ;nulls
 N ROW,ITEMC1,ITEMC2,ROWLAST
 S FLD=0 F I=1:1 S FLD=$O(VCUR(FLD)) Q:FLD'>0  S FLDSEQ(I)=FLD
 S I=I-1,ROWLAST=(I\2)+(I#2)
TEST ;S FLD=0 F  S FLD=$O(VCUR(FLD)) Q:FLD'>0  D
 W !,"IEN: ",+IENS
 F ROW=1:1:ROWLAST D
 . S ITEMC1=ROW*2-1,FLD=FLDSEQ(ITEMC1) ;ROW COLUMN 1
 . D FIELD^DID(21626.01,FLD,"","LABEL","NM")
 . W !,$E(NM("LABEL"),1,14),":",?17,$E($G(VCUR(FLD)),1,32)
 . S ITEMC2=ROW*2 I '$D(FLDSEQ(ITEMC2)) Q  ; ROW COLUMN 2
 . S FLD=FLDSEQ(ITEMC2)
 . D FIELD^DID(21626.01,FLD,"","LABEL","NM")
 . W ?40,$E(NM("LABEL"),1,14),":",?57,$E($G(VCUR(FLD)),1,32)
 W !,"Brief of Vendor Name: ",$$BRIEF^VFDHHLOT(VCUR(.02))
 ;error check reveal
 D XBNEW("PRTERRDA^VFDHHL60(DA)","DA") ; reveal error check
 ;write out any subfiles
SF2601 I $O(CVALS(21626.01)) D  ;D ITEMSF(21626.01)
 . W !,LINE,!,"Atomic(s):"
 . N G,PDA,MDA,X,LABDA,LABNM S G=$NA(^VFD(21626.01,+IENS,10))
 . S MDA=0 F  S MDA=$O(@G@(MDA)) Q:MDA'>0  S PDA=+@G@(MDA,0),X=^VFD(21626.01,PDA,0) D
 .. S LABDA=$P(X,U,3),LABNM=$$GET1^DIQ(60,LABDA_",",.01)
 ..W !,MDA,?3,$P(X,U,1),?15,$E($P(X,U,2),1,18),?35,$E(LABNM,1,27) D
 ..W ?59,$P(X,U,3),?66,$P(X,U,4),?69,$P(X,U,5),?72,$$GET1^DIQ(21626.01,PDA_",",.32)
 S LAB60=$$GET1^DIQ(21626.01,IENS,.03,"I")
ATM2601 ;display PANELS
 I $D(^VFD(21626.01,"ATM",DA)) D
 . N ATDA,X,LABDA,LABNM W !,LINE,!,"Atomic of Panel(s): or Alternate Code to:"
 .S ATDA=0 F  S ATDA=$O(^VFD(21626.01,"ATM",DA,ATDA)) Q:ATDA'>0  S X=^VFD(21626.01,ATDA,0) D
 ..S LABDA=$P(X,U,3),LABNM=$$GET1^DIQ(60,LABDA_",",.01)
 ..W !,$P(X,U,1),?15,$E($P(X,U,2),1,18),?35,$E(LABNM,1,27),?65,$P(X,U,3),?70,$P(X,U,4),?75,$P(X,U,5)
 D:$G(LAB60) ITEM60(LAB60)
 D ^%ZISC
 Q
PRTERRDA(IEN) ;
 N XX S XX=$$CHKIEN^VFDHHL61(IEN)
 I +XX W !,"Computed CHECKER: PASS" Q
 W !,"Computed CHECKER: ",XX
 Q
ITEM60(LABDA) ;
 N CVALS,IENS,FLDS,NM,VCUR,LINE
 S $P(LINE,"=",79)=""
 S IENS=LABDA_","
 S FLDS=".01;3;4;10;51;7;8;56;100*;200*;300*;400"
 D GETS^DIQ(60,IENS,FLDS,"","CVALS","MSG")
 I $D(CVALS) M VCUR=CVALS(60,IENS) I 1
 E  N I F I=1:1 S FLD=$P(FLDS,";",I) Q:FLD'>0  S VCUR(FLD)=""
 W !,LINE,!,"Lab IEN: ",LABDA
 S FLD=0 F  S FLD=$O(VCUR(FLD)) Q:FLD'>0  D
 . D FIELD^DID(60,FLD,"","LABEL","NM")
 . W !,NM("LABEL"),?30,$G(VCUR(FLD))
ITEM60M I '$O(CVALS(60)) G ITEM60Q
 S SF=60 D ITEMSF(60) Q
 N SF,VCUR,NM
 S SF=60 F  S SF=$O(CVALS(SF)) Q:SF'>0  D ITEM60SF
 W !,LINE
ITEM60Q Q
ITEM60SF ;display sub files 100*,300* or what is in DR as fld*
 W !,"*",$O(^DD(SF,0,"NM","")),"  >Multiple<"
 S IENS="" F I=1:1 S IENS=$O(CVALS(SF,IENS)) Q:IENS=""  D
 .M VCUR=CVALS(SF,IENS)
 .;S FLD=0 F  S FLD=$O(VCUR(FLD)) Q:FLD'>0  D
 .S FLD=.01 D
 .. D FIELD^DID(60,FLD,"","LABEL","NM")
 .. W !,?2,I,?5,FLD,?10,NM("LABEL"),?35,$G(VCUR(FLD))
 Q
GET(REC,DLM,XX) ; where XX = VAR_"="_I  ex: XX="PATNM=1"
 ; Set VAR = piece I of REC using delimiter DLM
 N Y,I S Y=$P(XX,"="),I=$P(XX,"=",2),@Y=$P(REC,DLM,I)
 Q
SET(REC,DLM,XX) ; where XX = VAR_U_I  ex: XX="1=PATNUM"
 ; Set VAR into piece I of REC using delimiter DLM
 N Y,I S I=$P(XX,"="),Y=$P(XX,"=",2)
 I Y'=+Y,Y'="" S $P(REC,DLM,I)=$G(@Y) I 1
 E  S $P(REC,DLM,I)=YOUT ;
 Q
E N DIR S DIR(0)="EO",DIR("A")="<CR> - Continue" D ^DIR
 Q
XB(XBRET) ; IHS/ADC/GTH - NESTING OF DIE ; [ 02/07/97   3:02 PM ]
 ;;3.0;IHS/VA UTILITIES;;FEB 07, 1997
 ;
 ;  PROGRAMMERS:  DO NOT USE THE FIRST LINE FOR ENTRY.
 ;                USE LABEL XBNEW() FOR ENTRY.
 ;
 ; XBNEW("TAG^ROUTINE","variable list")
 ;
 ; Variable list has the form "AGDFN;AGINS;AGP*".
 ; Wild card * allowed.
 ; 
 ; XBRET has the form "TAG^ROUTINE:VAR;NSVAR*"
 ;
 ; This allows for the nesting of die calls by
 ;
 ; 1. Building and executing an exclusive new from preselected
 ;    kernel variables and any local variables &/or name
 ;    spaces identified by the calling parameter.
 ;
 ; 2. After executing the new (....) XBNEW performs a DO call
 ;    to the program entry point identified by the calling
 ;    parameter.  The entry point passed should build the
 ;    variables and execute the DIE call to be nested.
 ;
 ; 3. As XBNEW quits to return to the calling program it pops
 ;    the variable stack.
 ;
 ;
 NEW XB,XBNS,XBN,XB,XBY,XBL,XBKVAR
 G S
 ;
XBNEW(XBRT,XBNS) ;PEP XBRT=TAG^ROUTINE  XBNS=varialbe list ";" with * allowed
 NEW XB,XBN,XB,XBY,XBL,XBKVAR,XBRET
 S XBRET=XBRT_":"_XBNS
S ;
 I XBRET'[":" S XBRET=XBRET_":"
 S XBN="XBRET",XBKVAR=$P($T(XBKVAR),";;",2),XBNS=$P(XBRET,":",2)
 I XBNS="" G RETURN
 F XBI=1:1 S (XB,XBY)=$P(XBNS,";",XBI) Q:XB=""  D
 . I XB'["*" S XBN=XBN_","_XB Q
 . S (XB,XBY)=$P(XB,"*"),XBN=XBN_","_XB,XBL=$L(XB)
 . F  S XBY=$O(@XBY) Q:((XBY="")!(XB'=$E(XBY,1,XBL)))  S XBN=XBN_","_XBY
 .Q
RETURN ;
 S XBN="("_XBN_","_XBKVAR_")",$P(XBRET,":",2)=XBN
NEW ;
 NEW @($P(XBRET,":",2))
 D @($P(XBRET,":",1))
 Q
 ;
END ;--------------------------------------------------------------
 ; the following taken from the variable list in KILL^XUSCLEAN from  KERNEL
XBKVAR ;;DUZ,DTIME,DT,DISYS,IO,IOF,IOBS,IOM,ION,IOSL,IOST,IOT,IOS,IOXY,U,XRTL,ZTSTOP,ZTQUEUED,ZTREQ,VFDINST
 ;-------------------------------------------------------------- 
 Q
ITEMSF(SF) ;SF IS PARENT
 I '$O(CVALS(SF)) G ITEMSFQ
 N VCUR,NM
 F  S SF=$O(CVALS(SF)) Q:SF'>0  D ITEMSFWR
 W !,LINE
ITEMSFQ Q
ITEMSFWR ;display sub files 100*,300* or what is in DR as fld*
 W !,"*",$O(^DD(SF,0,"NM","")),"  >Multiple<"
 S IENS="" F I=1:1 S IENS=$O(CVALS(SF,IENS)) Q:IENS=""  D
 .M VCUR=CVALS(SF,IENS)
 .;S FLD=0 F  S FLD=$O(VCUR(FLD)) Q:FLD'>0  D
 .S FLD=.01 D
 .. D FIELD^DID(60,FLD,"","LABEL","NM")
 .. W !,?2,I,?5,FLD,?10,NM("LABEL"),?35,$G(VCUR(FLD))
 Q
 ;
LINKPNAT ;
 ;link panels to atomics
 W !!,?10,"Load/Link Panels to atomics"
 W !!,?10,"Build the table to be loaded:"
 W !!,?10,"Order<tab>Result"
 S G=$NA(^TMP("VFDHCHUI",$J)) K @G
 K DIR S DIR(0)="SO^H:HFS;X:EXCEL",DIR("A")="Select Source:" D ^DIR
 Q:Y=""  I "HX"'[Y Q
 I Y="H" D HFSLNKPA Q
 D CHUILOAD^VFDHHL01 ;RETURNS DATA IN ^TMP("VFDHCHUI",$J,LN,0)
 N LN S LN=0 F  S LN=$O(@G@(LN)) Q:LN'>0  S @G@(LN)=@G@(LN,0) K @G@(LN,0)
 D LNKGPNAT(G) ;PASS G TO LINK PANEL TO ATOMIC FILER
 Q
HFSLNKPA ;use HFS to link panels to atomics.
 D HFSLOAD^VFDHHLUT
 S G=$NA(^TMP("VFDHHLUT",$J))
 D LNKGPNAT(G)
 Q
LNKGPNAT(G) ; ENTRY FROM hfsload^VFDHHLUT ;S G=$NA(^TMP("VFDHCHUI",$J))
 I '+$O(@G@("A"),-1) W !,"Nothing to Load" D E Q
 N T,R,VFDPNPAT S T=$C(9)
 S R=0 F  S R=$O(@G@(R)) Q:R'>0  D
 .S ROW=@G@(R),PN=$P(ROW,T),AT=$P(ROW,T,2) S VFDPNAT(PN,AT)=""
 ;PUT ENTRIES INTO SCRATCH FILE
 S CC=0
PNLOOP S PN="" F  S PN=$O(VFDPNAT(PN)) Q:PN=""  D
 . I '$D(^VFD(21626.01,"B",PN)) W !,PN,?15,"NOT FOUND" Q
 . D FILEPNAT
 Q
FILEPNAT ;
 N AT
 S PNDA=$O(^VFD(21626.01,"B",PN,0))
 S AT="" F  S AT=$O(VFDPNAT(PN,AT)) Q:AT=""  D
 .I '$D(^VFD(21626.01,"B",AT)) W !,PN,?15,AT,?35," NOT FOUND" Q
 .K VIEN,MSG
 .K FDA
 .S FDA(21626.0101,"?+1,"_PNDA_",",.01)=AT
 .D UPDATE^DIE("SE","FDA","VIEN","MSG")
 .I $D(MSG) W ! D  D E
 ..F Y="PN","AT","MSG","FDA",$NA(^VFD(21626.01,PNDA)),"VIEN" D Q(Y)
 .;W !! ZW PN,AT,MSG,FDA,^VFD(21626.01,PNDA),VIEN D E
 Q
LABPOP ;
 W !,"Reviewing  Entries"
 N VFDINST ;for selecting for Acession area.
 N NA M FLG1=^VFD(21626.01,"FLG1","NA")
 N OK,NOT,UPDATE
 F YY="NA","NP","UPDATE" D
 . N FLG1
 . M FLG1=^VFD(21626.01,"FLG1",YY)
 . W !,$S(YY="NA":"NEW ATOMICS",YY="NP":"NEW PANELS",1:"UPDATES")
 . S IEN=0 F I=0:1 S IEN=$O(FLG1(IEN)) Q:IEN'>0  D CHK60(IEN,YY)
 W !!,"Summary"
 W ?15,"OK",?20,"NOT OK"
 W !,"New Atomics:",?15,+$G(OK("NA")),?20,+$G(NOT("NA"))
 W !,"New Panels:",?15,+$G(OK("NP")),?20,+$G(NOT("NP"))
 W !,"UPDATES:",?15,+$G(OK("UPDATE")),?20,+$G(NOT("UPDATE"))
 K YES
 I +$G(OK("NA")) K DIR S DIR(0)="YO",DIR("A")="Proceed with NA" D ^DIR D
 .I Y=1 S YES("NA")=""
 I +$G(OK("NP")) K DIR S DIR(0)="YO",DIR("A")="Proceed with NP" D ^DIR D
 .I Y=1 S YES("NP")=""
 I +$G(OK("UPDATE")) K DIR S DIR(0)="YO",DIR("A")="Proceed with UPDATE" D ^DIR D
 .I Y=1 S YES("UPDATE")=""
 I '$D(YES) W !,"NONE SELECTED" Q
 W !,"Responses were: " W:$D(YES("NA")) "NA  " W:$D(YES("NP")) "NP  " W:$D(YES("UPDATE")) "UPDATE"
 K DIR S DIR(0)="YO",DIR("A")="Is the above correct",DIR("B")="N" D ^DIR
 I Y'=1 W !,"QUITING" Q
 I $D(YES("NA")) W !,"NA Run" D NA^VFDHHL61
 I $D(YES("NP")) W !,"NP Run" D NP^VFDHHL61
 I $D(YES("UPDATE")) W !,"UPDATE Run" ;D UPDATE^VFDLRU1
 D E
 Q
CHK60(IEN,YY) ;used in preview of adding entries to file 60
 N XX,LRTESTX,FDA
 S YY=$G(YY)
 S:YY="" YY=0
 ;$$CHKIEN^VFDHHL61 will return LRTESTX(NPIEN,NAIEN)="" as needed
 N LRTESTX
 S XX=$$CHKIEN^VFDHHL61(IEN) I +XX S OK(YY)=$G(OK(YY))+1 Q  ;passed
 S IENS=IEN_"," N FDA,VIEN,MSG
 S FDA(21626.01,IENS,.16)="ERR_"_YY
 I XX["^" S XX=$TR(XX,"^","|")
 S FDA(21626.01,IENS,.2)=XX
 ;list but do not record errors when updating
 D:(YY'="UPDATE") UPDATE^DIE("S","FDA","VIEN","MSG")
 I $D(MSG) F Y="FDA","VIEN","MSG" D Q(Y)
 D DIQDISP(21626.01,IENS,".01;.02;.16;.2;9"),Q("XX")
 S NOT(YY)=$G(NOT(YY))+1
 ;the checker returns atomics with errors in LRTESTX
 I '$D(LRTESTX) Q
 W !," ATOMIC ERRORS"
 D Q("LRTESTX")
 ; recursivelly
 N IEN,OK,NOT
 S IEN=0
 F  S IEN=$O(LRTESTX(IEN)) Q:IEN'>0  D CHK60(IEN,"NA")
 Q
STUFF60 ;pRESET SUGGESTED gen 60 name suggestions FLAG 1="NA"
 S G="^VFD(21626.01,""FLG1"",""NA"")"
 S R="^VFD(21626.01)"
 S IEN=0 F  S IEN=$O(@G@(IEN)) Q:IEN'>0  D
 .S NM=$P(@R@(IEN,0),U,2),NM60=$$BRIEF^VFDHHLOT(NM)
 .W !,$L(NM),?5,NM,?35,NM60,?60,$L(NM60),?65,+$D(^LAB(60,"B",NM60))
 .;S $P(@R@(IEN,1),U,2)=NM
 .;W ! ZW @R@(IEN)
 Q
MOVE ; move entries to HL7X ; G and TBL passed into TABMERGE of VFDHHLOE
 W !,"Using FLAG 1 to move items into HL7 Exchange files"
 N INDX,VAL,DA,DIR,G,IEN,LC,NZ,TBL
 S INDX="FLG1"
 K DIR S DIR(0)="SO^NA:DONE NA;NP:DONE NP;SO:DONE SO" D ^DIR
 Q:Y=""
 I "SO,NA,NP"'[Y Q
 S VAL=Y(0),TBL=Y ;
 W !,"The destination table should be: " I "NA"=Y W ">> VXLRESULT <<" I 1
 E  W ">> VXLRORDER <<"
 W !,"FLAG 1",?30,"Value ",VAL
 W !,"Showing FIRST 5 Entries to move"
 D ^%ZIS
 U IO S DA=0 F I=1:1:5 S DA=$O(^VFD(21626.01,INDX,VAL,DA)) Q:DA'>0  D DISP2601
 D ^%ZISC
 K DIR
 S DIR(0)="YO",DIR("A")="Is the above correct? " D ^DIR
 I Y'=1 G MOVE
 ; make an array look like an excel table of values to load with software call
 W !,"Loading the entries"
 S G=$NA(^TMP("VFDHHL60",$J)),T=$C(9) K @G
 S IEN=0,LC=0 F  S IEN=$O(^VFD(21626.01,INDX,VAL,IEN)) Q:IEN'>0  D
 .S LC=LC+1
 .;save pieces 1-5 as tab delimited for filing as excel table
 .S NZ=$P(^VFD(21626.01,IEN,0),U,1,5),NZ=$TR(NZ,U,T)
 .S @G@(LC,0)=NZ
 .W !,NZ
 .;update flag with VAL_HL7X to show moved to HL7X
 .N FDA,VIEN,MSG
 .S FDA(21626.01,IEN_",",.16)=VAL_" HL7X",VIEN(1)=IEN
 .D UPDATE^DIE("S","FDA","VIEN","MSG")
 .I $D(MSG) D Q("MSG"),E
 D TABMERGE^VFDHHLOE ; entry with 'G'lobal table defined
 Q
SOPOP ;Single Orderable Pop Loop & Populate Singular Orderables with the Atomic's IEN
 W !!,"This uses the FLAG 1 reference set to 'SO'" D E
 I '$D(^VFD(21626.01,"FLG1","SO")) W !,"NO 'SO' Flags set. Quiting" Q
 N SODA,ATDA,L60IEN,FDA,Z,MSG
 W !!,"Entry ^ Vendor Name",?50,"Lab Test"
 S SODA=0 F  S SODA=$O(^VFD(21626.01,"FLG1","SO",SODA)) Q:SODA'>0  D SOPOP1(SODA)
 W !
 Q
SOPOP1(SODA) ;Singular Orderable /descendant atomic
 S ATDA=+$G(^VFD(21626.01,SODA,10,1,0))
 I 'ATDA W !,$P(^VFD(21626.01,SODA,0),U,1,2),?20," has no atomic." D E Q
 I $O(^VFD(21626.01,SODA,10,1)) W !,$P(^VFD(21626.01,SODA,0),U,1,2)," has several atomics " D E Q
 S L60IEN=$P(^VFD(21626.01,ATDA,0),U,3) ;grab LAB 60 IEN
 I 'L60IEN W !,$P(^VFD(21626.01,SODA,0),U,1,2),!,?10,$P(^VFD(21626.01,ATDA,0),U,1,2),?40," has no LAB Test" D E Q
 S Z(.03)=L60IEN,Z(.16)="DONE SO"
 M FDA(21626.01,SODA_",")=Z K MSG
 D UPDATE^DIE("S","FDA","","MSG")
 I $D(MSG) D Q("MSG") D E
 W !,$P(^VFD(21626.01,SODA,0),U,1,2),?50,$$GET1^DIQ(21626.01,SODA,.03)
 Q
Q(VAL) ; DISPLAY VAR WITH $Q
 N X,X1 S (X,X1)=VAL
 Q:'$D(@VAL)
 I $E(X,$L(X))=")" S X1=$E(X,1,$L(X)-1)
 I $D(@X)#10 W !,X,"=",@X
 F  S X=$Q(@X) Q:X'[X1  W !,X,"=",@X
 Q
DIQDISP(FILE,IENS,FLDS) ;display
 K CVALS
 D GETS^DIQ(FILE,IENS,FLDS,"N","CVALS","MSG")
 K VCUR,FLDSEQ
 ;display blanks if entry deleted
CVALS I $D(CVALS) M VCUR=CVALS(FILE,IENS)
 N ROW,ITEMC1,ITEMC2,ROWLAST
VCUR S FLD=0 F I=1:1 S FLD=$O(VCUR(FLD)) Q:FLD'>0  S FLDSEQ(I)=FLD
 S I=I-1,ROWLAST=(I\2)+(I#2)
 F ROW=1:1:ROWLAST D
 . S ITEMC1=ROW*2-1,FLD=FLDSEQ(ITEMC1) ;ROW COLUMN 1
 . D FIELD^DID(FILE,FLD,"","LABEL","NM")
 . W !,$E(NM("LABEL"),1,14),":",?17,$E($G(VCUR(FLD)),1,32)
 . S ITEMC2=ROW*2 I '$D(FLDSEQ(ITEMC2)) Q  ; ROW COLUMN 2
 . S FLD=FLDSEQ(ITEMC2)
 . D FIELD^DID(FILE,FLD,"","LABEL","NM")
 . W ?40,$E(NM("LABEL"),1,14),":",?57,$E($G(VCUR(FLD)),1,32)
 Q
CHANGES ;
 ;25May10 no laygo on LAB Field of Staging file
 ;changed DATANAME to DESIRED DATANAME
 ; update flag value of DONE 'NA'NP'SO' with HL7X when item added to HL7X tables 
 ; modified MOVE to be restricted to choices OF DONE NA, DONE NP
 ;28 corrected display of panels
 ;22 Jun MOVE +10 FLG=>FLG1
 ;22 Jun MOVE+29 TYPO UPDATE^DIE(",S" => UPDATE^DIE("S",
 ;19 Mar 2012 pulled the lab file creation routine in as VFDHHL61 from VFDLRU1
 ;27 Mar 2012 added menu to Stuff Lab Shipping Configuration file
 ;02 Apr 2012 worked over the handling of the .41 VA WKLD test error
 ;02 Apr 2012 worked over the display of fields (not all were showing up) 
