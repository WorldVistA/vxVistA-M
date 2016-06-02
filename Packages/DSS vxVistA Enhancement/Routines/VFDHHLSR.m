VFDHHLSR ;DSS/PDW - SAVE/RESTORE SITE VALUES - ;22 FEB 2012
 ;;2012.1.1;VENDOR - DOCUMENT STORAGE SYS;;24 Jun 2013;Build 136
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 06/23/15 * vxdev64v2k8_VX13 * 2012.1 T22
 ;DSS/BUILD - VFDHHL HL7 EXCHANGE 2012.1.1 * 02/02/14 * vxdev64v2k8_DEVXOS * 2012.1.1T21
 ;^TMP("VFDHLIO",$J,"OUT",VFDLN,0)
 Q
 ;VFDHHLSR Function Flow
 ;1. XMLDD ;; holds the programmers architecture of files, fields, and selections 
 ;   to be saved and restored
 ;2. XMLLOAD will refer to XMLDD+i and build an array VFDVROW of the architecture instructions.
 ;3. D SAVE^VFDHHLSR(acronym,.VFDVROW) with acronym being selected by the programmer as desired
 ;    and used in building an ^XTMP global. The acronym is not linked to anything else and is used
 ;    only as a subscript in the ^XTMP global.
 ;    The return array VFDROW is the 
 ;   It will store the DDs, files, fields and values in XTMP(@acronum).
 ;   This call is best used in a preinit.
 ;4. D RESTORE(acronym,VFDSHOW) will restore the files and fields saved. It is best used in a post init.
 ; The XTMP Global is saved for five days.
 ;
 ; Copy XMLLOAD and XMLDD subroutines into your preinit-postinit routine
 ; Modify XMLDD to match the component items in your kids
 ; Preinit:
 ;      D XMLLOAD^your routine in a preinit - builds array VFDVROW
 ;      D SAVE^VFDHHLSR("acronym",.VFDVROW) - Saves Items : fields
 ; Postinit:
 ;      D RESTORE^VFDHHLSR("acronym")
 ;
 ;5. Building the XMLDD rows of file and field notations for Exporting/preserving.
 ; a. Each row is structured as: 
 ;;1^771^@;.01;2;3;4;7^N X S X=$P(&(0),U) I $E(X,1,4)="VFDH" I 1
 ; | |   |             \ The selection logic to be used to filter the entries
 ; | |   \ The list of fields prefixed by an '@' (needed for the FM API)
 ; | \ The file/sub-file number
 ; \ The file DD level of data being addressed
 ;
 ; b. A "~" can be used as a continuation character at the end and beginning of rows to be joined.
 ;;1^870^@;.01;.02;2;3;4.5;14;100.01;200.02;200.021;200.05;200.08;~
 ;;~400.01;400.02;400.03;400.04;400.05;400.08;400.09^N X S X=$P(&(0),U) I $E(X,1,4)="VFDH"
 ; 
SAVE(VFDBUILD,VFDVROW,VFDSHOW) ; Save into XTMP the DDs and Values of selected files, multiples, and fields
 ; VFDVROW - an array as built by formatted  XMLDD
 ;
 ; called by a Kids instal preinit must be available
 S VFDBUILD=$G(VFDBUILD),VFDSHOW=$G(VFDSHOW)
 S:$L(VFDBUILD)'>0 VFDBUILD="VFDHHLSR"
 S VFDSHOW=1
 N RUNROW,ROWV,VFDLEV,VFDROWN,LAST,VFDGL,VFDLN,X,VFDSB,VFDSE,VFDRB,VFDRE
 N VFDENTRY,VFDDATA,VFDFILE,VFDIENS,VFDAIENS,VFDATA,VFDFLDS,VFDFLGS,VFDFR
 N VFDIDENT,VFDIENLN,VFDLSTR,VFDMSG,VFDINDX,VFDNDX,VFDNLEV,VFDNROW,VFDNUM,VFDPART
 N VFDSCR,VFDTAR,ROW
 S VFDGL=$NA(^XTMP(VFDBUILD,$J))
 I $D(@VFDGL) D
 . S XX=@VFDGL@(1)
 . I $P(XX,U,1),$P(XX,U,2),$P(XX,U,3),$P(XX,4) Q  ;previous run completed
 . W !,"SOMETHING BROKE" D  Q
 . S X=@VFDGL@(1)
 . S VFDSB=$P(X,U,1),VFDSE=$P(X,U,2),VFDRB=$P(X,U,3),VFDRE=$P(X,U,4)
 . W !,"Save Began",?15,$$FMTE^XLFDT(VFDSB)
 . W !,"Save Ended",?15,$$FMTE^XLFDT(VFDSE)
 . W !,"Restore Began",?15,$$FMTE^XLFDT(VFDRB)
 . W !,"Restore Ended",?15,$$FMTE^XLFDT(VFDRE)
 . S XPDABORT=1 ;abort installation
 K @VFDGL
 S @VFDGL@(0)=$$NOW^XLFDT_U_$$FMADD^XLFDT(DT,+5)_U_"VFDHL SAVE AND RESTORE"
 S X=$$NOW^XLFDT D STASH(X) ;store begin time for save, VFDLN=1
 I '$D(VFDVROW) D XMLLOAD ; read DD Rows into VFDVROW()
 S LAST=$O(VFDVROW("A"),-1)
 K RUNROW
 ; find top level file rows
 F I=1:1:LAST I +VFDVROW(I)=1 S RUNROW(I)=""
 ;process each top level file row
 S ROWV="" F  S ROWV=$O(RUNROW(ROWV)) Q:ROWV'>0  S VFDLEV=1,VFDROWN=ROWV K VFDIENS D PULL
 ;store end time of save
 S X=@VFDGL@(1),$P(X,U,2)=$$NOW^XLFDT,@VFDGL@(1)=X
 I '$D(ZTQUEUED),$E(IOST)="C" W !!,"Selected Client data lines stored are ",VFDLN\2,!
 Q
PULL ;(VFDNLEV,VFDNROWN,VFDNIENS) variables needed; Recursive walker, VFDN (N denotes Next instance)
 ;newing moved down into the iterative D PULL calls
 N VFDFILE,VFDFLDS,VFDFLGS,VFDNUM,VFDFR,VFDPART,VFDNDX,VFDSCR,VFDIDENT,VFDTAR,VFDMSG
 S (VFDFILE,VFDFLDS,VFDFLGS,VFDNUM,VFDFR,VFDPART,VFDNDX,VFDSCR,VFDIDENT,VFDTAR,VFDMSG)=""
 S VFDAIENS=""
 S VFDIENS(VFDLEV)=""
 ;walk upward from current level to build IENs from present IENS
 ; including IEN for current level
 F I=VFDLEV:-1:1 S VFDAIENS=$G(VFDAIENS)_$G(VFDIENS(I))_"," ; build ",DA(L-1),DA(L-2),. . ."
 ;get ready to enter next lower level of file
 S VFDVROW=VFDVROW(VFDROWN)
 S VFDFILE=$P(VFDVROW,U,2)
 S VFDFLDS=$P(VFDVROW,U,3)
 ;dss/pdw 18jul12  test fields for existence
 ;DSS/PDW protect if no field FIELD^DID
 N XX,J,I
 S XX=VFDFLDS,VFDFLDS="@;",J=1
 F I=2:1 S VFDFLD=$P(XX,";",I) Q:VFDFLD=""  D
 .N VFDFLG,VFDATTRB,VFDTAR,VFDMSG
 .S (VFDFLG,VFDATTRB)=""
 .;set values accordingly 
 .S VFDFLAG="N",VFDATTRB="LABEL"
 .K VFDTAR,VFDMSG
 .D FIELD^DID(VFDFILE,VFDFLD,VFDFLG,VFDATTRB,"VFDTAR","VFDMSG")
 .I $D(VFDMSG)>1 W !,?2,"Not Found ",?17,"File:  ",VFDFILE,?30,"Field:  ",VFDFLD Q
 .S J=$G(J)+1,$P(VFDFLDS,";",J)=VFDFLD
 ;setup internal and packed LIST call
 S VFDFLGS="IP"
 ;setup screen
 S VFDSCR=$P(VFDVROW,U,4)
 I VFDSCR="" S VFDSCR="I 1"
 ;      ;S VFDSCRTX=$P(VFDVROW,U,5)
 ;      ;I $L(VFDSCRTX) S VFDSCR="I ("_VFDSCR_")!(VFDSCRTX[""-""_$P(^(0),U)_""-"""
 ;screen may have used "^" which was changed to "&" ;may needed to be changed
 S:VFDSCR["&" VFDSCR=$TR(VFDSCR,"&","^")
 S VFDTAR=$NA(^TMP("VFDXPDXML",$J,VFDLEV)) K @VFDTAR
 K VFDMSG,VFDENTRY
 ;W ! ZW VFDFILE,VFDIENS
LIST ;
 D LIST^DIC(VFDFILE,VFDAIENS,VFDFLDS,VFDFLGS,VFDNUM,VFDFR,VFDPART,VFDNDX,VFDSCR,VFDIDENT,VFDTAR,"VFDMSG")
 M VFDENTRY=^TMP("VFDXPDXML",$J,VFDLEV,"DILIST")
 ; recursion TEST next row when no multiple to process 
 I $D(VFDMSG) D Q("VFDMSG") D E Q
 I '$D(VFDENTRY(1,0)) D  Q  ; if nothing at this level  check for further levels or back out
 . ; TEST recursion next row: no multiple to work, find next level indicator
 . S VFDNROW=VFDROWN+1,VFDNLEV=+$G(VFDVROW(VFDNROW))
 . I VFDNLEV<VFDLEV Q  ;if same or deeper level continue pulling
 . I VFDNLEV=1 Q  ; backout, hit a new DD file as Level =1
 . ;stack variables for recursive call
 . N VFDLEV,VFDROWN,VFDIENLN,VFDENTRY,VFDFILE,VFDIEN,I,II
 . S VFDLEV=VFDNLEV,VFDROWN=VFDNROW
 . N VFDNLEV,VFDNROW
 . D PULL ; recursion next row as nothing found here
 K VFDATA(VFDLEV)
 M VFDATA(VFDLEV)=VFDENTRY
 ;W ! ZW VFDATA(VFDLEV),VFDFILE,VFDAIENS B
 ;process lines of return rows from LIST call
 S VFDIENLN=0 F  S VFDIENLN=$O(VFDENTRY(VFDIENLN)) Q:VFDIENLN'>0  D  I $G(VFDNLEV),$G(VFDNLEV)<VFDLEV Q
 .S VFDIENS(VFDLEV)=+VFDENTRY(VFDIENLN,0),VFDAIENS=""
 .N I
 .F I=VFDLEV:-1:1 S VFDAIENS=$G(VFDAIENS)_$G(VFDIENS(I))_"," ; build ",DA(L-1),DA(L-2),. . ."
 .; build full IEN path "DA,DA(L-1),DA(L-2),. . ."
 .S X="<|ITEM|/>" D STASH(X)
 .; VFDENRTY(0,"MAP") is list of fields
 .S X="<|VFD_DD|/>"_VFDLEV_U_VFDFILE_U_VFDENTRY(0,"MAP")_"</|VFD_DD|>" D STASH(X) ;W !!,X
 .;need to work in pointer fields external values here
 .S X="<|VFD_VALS|/>"_VFDLEV_U_VFDFILE_U_VFDAIENS_U_VFDENTRY(VFDIENLN,0)_"</|VFD_VALS|>" D STASH(X) ;W !,X
 .S X="</|ITEM|>" D STASH(X)
 .;W !,ROWV,?5,VFDROWN
 .I VFDSHOW W:VFDLEV=1 !,?(VFDLEV*2),VFDFILE,?(15+(VFDLEV*2)),$$GET1^DIQ(VFDFILE,VFDAIENS,.01)
 .;check each GETS line entry to see if it has descendants 
 . D
 .. ; TEST recursion next row
 .. S VFDNROW=VFDROWN+1,VFDNLEV=+$G(VFDVROW(VFDNROW)) ; pull level of next row
 .. I VFDNLEV<VFDLEV Q  ; if next level less than current back out
 .. I VFDNLEV=1 Q  ; backout
 .. ;stack variables for recursive call
 .. N VFDLEV,VFDROWN,VFDIENLN,VFDENTRY,VFDFILE,VFDIEN,I,II
 .. S VFDLEV=VFDNLEV,VFDROWN=VFDNROW
 .. N VFDNLEV,VFDNROW
 .. D PULL ; recursion next row 
 Q
RESTORE(VFDBUILD,VFDSHOW) ;
 S VFDBUILD=$G(VFDBUILD),VFDSHOW=$G(VFDSHOW)
 S:$L(VFDBUILD)'>0 VFDBUILD="VFDHHLSR"
 I VFDBUILD="VFDHHLSR" S VFDSHOW=1
 N RUNROW,VFDVROW,ROWV,VFDLEV,VFDROWN,LAST,VFDGL,VFDLN,X,VFDSB,VFDSE,VFDRB,VFDRE
 N VFDFLDS,VFDIENS,VFDFLDS
 N VFDLL
 S VFDGL=$NA(^XTMP(VFDBUILD,$J))
 I '$D(@VFDGL) W !,"Nothing Found ??" Q
 S X=@VFDGL@(1)
 S VFDSB=$P(X,U,1),VFDSE=$P(X,U,2),VFDRB=$P(X,U,3),VFDRE=$P(X,U,4)
 I (('VFDSB)!('VFDSE)!(VFDRB)!(VFDRE)) W "SOMETHING BROKE"  D  Q
 . W !,"Save Began",?15,$$FMTE^XLFDT(VFDSB)
 . W !,"Save Ended",?15,$$FMTE^XLFDT(VFDSE)
 . W !,"Restore Began",?15,$$FMTE^XLFDT(VFDRB)
 . W !,"Restore Ended",?15,$$FMTE^XLFDT(VFDRE)
 S X=@VFDGL@(1),$P(X,U,3)=$$NOW^XLFDT,@VFDGL@(1)=X
 S VFDLL=$O(@VFDGL@("A"),-1)
 F VFDLN=2:1:VFDLL S R=@VFDGL@(VFDLN) D
 . I R["<|VFD_DD|/>" K VFDFLDS D DDFLDS(.VFDFLDS,R) Q
 . I R["<|VFD_VALS|/>" S XX=$$DDVALSOK(R) ;I 'XX W 1/0 ;return 1 if filed properly
 S X=@VFDGL@(1),$P(X,U,4)=$$NOW^XLFDT,@VFDGL@(1)=X
 I '$D(ZTQUEUED),$E(IOST)="C" W !!,"Selected Client data lines restored are ",VFDLN\2,!
 Q
DDFLDS(VFDFLDS,VFDDATA) ;return VFDFLDS, pull out fields to be stuffed
 ;"<|VFD_DD|/>2^101.01^IEN^.01I^2I^3I^4I^5I^6I^11I^12I^13I^14I</|VFD_DD|>"
 K VFDFLDS N VFDAT
 S VFDAT=$P(VFDDATA,">",2),VFDAT=$P(VFDAT,"<",1),VFDAT=$P(VFDAT,"^",4,999)
 F I=1:1 S X=$P(VFDAT,U,I) Q:X=""  S VFDFLDS(I)=+X
 ;W !,VFDDATA,! ZW VFDFLDS
 Q
DDVALSOK(VFDDATA) ;$$RETURN 1 if filed OK
 ;"<|VFD_VALS|/>2^101.01^1,4498,^1^4328^^^^^^^^^</|VFD_VALS|>"
 ; fields with code segments where "^" is replaced with "&#94;" 
 N VFDFDA,Z,I,VFDVAL,VFDIENS,VFDFILE,VFDOK,VFDMSG,VFDLEV
 S VFDOK=0
 S VFDVAL=$P(VFDDATA,">",2),VFDLEV=+VFDVAL,VFDVAL=$P(VFDVAL,"<",1),VFDVAL=$P(VFDVAL,"^",5,999)
 S VFDFILE=$P(VFDDATA,U,2),VFDIENS=$P(VFDDATA,U,3)
 S LL=$O(VFDFLDS("A"),-1)
 ;load Z from VFDFLDS array and VFDDATA, FM may have subbed "&#94;"
 F I=1:1:LL S X=$P(VFDVAL,U,I) S:X["&#94;" X=$P(X,"&#94;")_U_$P(X,"&#94;",2) S Z(VFDFLDS(I))=X
 M VFDFDA(VFDFILE,VFDIENS)=Z
 ;W ! ZW Z,VFDFILE,VFDIENS,LL,VFDVAL,VFDFDA
 S VFDOK=1
 D UPDATE^DIE(,"VFDFDA","VFDIENS","VFDMSG")
 ;if entry was deleted by install use ?+1 to search and put it back
 I $D(VFDMSG) D
 .K VFDMSG
 .S $P(VFDIENS,",",1)="?+1"
 .K VFDFDA M VFDFDA(VFDFILE,VFDIENS)=Z
 .D UPDATE^DIE(,"VFDFDA","VFDIENS","VFDMSG")
 I VFDLEV=1 W !,?2,VFDFILE,?15,$$GET1^DIQ(VFDFILE,VFDIENS,.01)
 I $D(VFDMSG) W !,?10,">> Could not re-file: ",R," << contact IT Staff"
 I $D(VFDMSG) S VFDOK=0 I $E(IOST)="C" D Q(VFDMSG) D E
 W ":"
 Q VFDOK
Q(VAL) ; DISPLAY VAR WITH $Q
 N X,X1 S (X,X1)=VAL
 Q:'$D(@VAL)
 I $E(X,$L(X))=")" S X1=$E(X,1,$L(X)-1)
 I $D(@X)#10 W !,X,"=",@X
 F  S X=$Q(@X) Q:X'[X1  W !,X,"=",@X
 Q
STASH(X) S VFDLN=$G(VFDLN)+1 S @VFDGL@(VFDLN)=X W "."
 Q
E ;returns VFDSTOP=1 if "^" entered
 I $E(IOST)="C",'$D(ZTQUEUED) K DIR,STOP
 S DIR(0)="E",DIR("A")="<CR> - Continue """"^"""" - STOP" W ! D ^DIR K DIR
 S:Y=0 VFDSTOP=1
 Q
CLEAR K ^XTMP("VFDHHLSR",$J) Q
XMLLOAD ;pull in the DDs and Fields
 K VFDVROW,VFDROW
 S ROW=0
 F I=1:1 S X=$P($T(XMLDD+I),";;",2) Q:((X="END")!(X=""))  S ROW=ROW+1,VFDVROW(ROW)=X,VFDLSTR=I D
 .I $E(X,$L(X))'="~" Q  ;check for continuation 
 .N X S I=I+1 S X=$P($T(XMLDD+I),";;",2)
 .S VFDVROW(ROW)=VFDVROW(ROW)_X,VFDLSTR=I
 .S VFDVROW(ROW)=$TR(VFDVROW(ROW),"~","") ;remove "~"
 .I $E(X,$L(X))'="~" Q  ;check for 2nd continuation 
 .N X S I=I+1 S X=$P($T(XMLDD+I),";;",2)
 .S VFDVROW(ROW)=VFDVROW(ROW)_X,VFDLSTR=I
 .S VFDVROW(ROW)=$TR(VFDVROW(ROW),"~","") ;remove "~"
 Q
XMLDD ;; information for preserving site values . . ;;DD Level^DD^FLDS^$T SCREEN
 ;;1^101^@;.01;15;20;770.1;770.11;770.14;770.2;770.3;770.4;770.5;770.6;770.7;770.8;~
 ;;~770.9;770.95;771;772;773.1;773.2;773.3;773.4;773.5;774^I ("VFDH,VFDZ"[$E(&(0),1,4))!($P(&(0),U)="VFD PATIENT CLINIC ENROLL")
 ;;2^101.0775^@;.01^I 1
 ;;2^101.01^@;.01;2;3;4;5;6;11;12;13;14^I 1
 ;;1^870^@;.01;.02;2;3;4.5;14;100.01;200.02;200.021;200.05;200.08;~
 ;;~400.01;400.02;400.03;400.04;400.05;400.08;400.09^N X S X=$P(&(0),U) I $E(X,1,4)="VFDH"
 ;;1^771^@;.01;2;3;4;7^N X S X=$P(&(0),U) I $E(X,1,4)="VFDH" I 1
 ;;
 ;;END
 ; Building the XMLDD rows of file and field notations for preserving site values.
 ; a. Each row is structured as: 
 ;;1^771^@;.01;2;3;4;7^N X S X=$P(&(0),U) I $E(X,1,4)="VFDH" I 1
 ; | |   |             \ The selection logic to be used to filter the entries
 ; | |   |               Any "^" needed in the selection code must be substitued
 ; | |   |               with "&" as "^" is used in the piecing of the row.
 ; | |   \ The list of fields prefixed by an '@' (needed for the FM API)
 ; | \ The file/sub-file number
 ; \ The file DD level of data being addressed
 ;
 ; b. A "~" can be used as a continuation character at the end and beginning of rows to be joined.
 ;;1^870^@;.01;.02;2;3;4.5;14;100.01;200.02;200.021;200.05;200.08;~
 ;;~400.01;400.02;400.03;400.04;400.05;400.08;400.09^N X S X=$P(&(0),U) I $E(X,1,4)="VFDH"
 ; 
VFDHHL ; sub for call by VFDHHLSR
 N VFDVROW
 D XMLLOAD
 D SAVE(,,1)
 Q
HISTORY ; 02 APR 2012 added VFDVROW as a passing reference into the 
 ;ZR  ZL VFDHHLSR K ^XTMP("VFDHHLSR") D VFDHHL
 ;call as D SAVE^VFDHHLSR(VFDBUILD,.VFDVROW,VFDSHOW)
 ;Set of Restore start time enabled; 
 ;18 Jul 2012 DD Field scan protection added
 ; The developer might specify fields that the client does not have.
 ;18 jUNE 13 many comments place into routine
