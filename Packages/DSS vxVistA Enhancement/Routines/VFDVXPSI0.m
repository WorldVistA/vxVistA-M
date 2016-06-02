VFDVXPSI0 ;DSS/TFF - PHARMACY IMPORT OPTIONS;04/08/2015
 ;;1.0;VENDOR - DOCUMENT STORAGE SYS;**37**;08 Apr 2015;Build 3
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ;
 Q
 ;
ENW(SLNT) ; Main Entry Point
 ;
 ; wholesale price.txt
 ; ===================
 ; UPDATE_DATE^FACILITY_CODE^NDC^PACKAGE_DESCRIPTION^PACKAGE_BRAND_NAME^PACKAGE_QUANTITY
 ; PACKAGE_TYPE^PACKAGE_SIZE^PACKAGE_SIZE_MEASURE^BILLING_PRICE^ACTUAL_PRICE
 ;
 N ROOT,DLM,RI,LN,DATE,FDA,IEN,MSG,VIEN,IENS
 Q:'$$FILESET("wholesale price.txt",,"PRICE","R")
 S ROOT=$NA(^XTMP("VFDVXPSI-WHOLESALE PRICE",$$NOW^XLFDT)),DLM=$C(9)
 S CT=0 F RI=1:1 R LN:DTIME Q:$$STATUS^%ZISH  D
 . K DATE,IEN,DIV,VIEN
 . D:'$G(SLNT)&('(RI#1000)) DOTS(,"PRICE")
 . S DATE=$$DT^VFDXLF($$PC(LN,1)) Q:RI=1&(DATE="")
 . S $P(@ROOT,U)=$P($G(@ROOT),U)+1           ; *** TOTAL RECORDS
 . I DATE="" D ERR("Could not convert date/time;1",1) Q
 . S FDA(21678,"?+1,",.01)=DATE
 . D UPDATE^DIE(,"FDA","IEN") K FDA I '$G(IEN(1)) D ERR("Filer Error;2",1) Q
 . S DIV=$$DIV($$PC(LN,2)) I DIV="" D ERR("Could not locate facility;3",1) Q
 . S FDA(21678.01,"?+1,"_IEN(1)_",",.01)=DIV
 . D UPDATE^DIE(,"FDA","VIEN") K FDA I '$G(VIEN(1)) D ERR("Filer Error;4",1) Q
 . S IENS="?+1,"_VIEN(1)_","_IEN(1)_","
 . S FDA(21678.011,IENS,.01)=$$PC(LN,3)      ; NDC
 . S FDA(21678.011,IENS,.02)=$$PC(LN,10)     ; BILLING PRICE
 . S FDA(21678.011,IENS,.03)=$$PC(LN,11)     ; ACTUAL PRICE
 . S FDA(21678.011,IENS,.04)=$$PC(LN,5)      ; PACKAGE BRAND NAME
 . S FDA(21678.011,IENS,.05)=$$PC(LN,4)      ; PACKAGE DESCRIPTION
 . S FDA(21678.011,IENS,.06)=$$PC(LN,8)      ; PACKAGE SIZE
 . S FDA(21678.011,IENS,.07)=$$PC(LN,9)      ; PACKAGE SIZE MEASURE
 . S FDA(21678.011,IENS,.08)=$$PC(LN,6)      ; PACKAGE QTY
 . S FDA(21678.011,IENS,.09)=$$PC(LN,7)      ; PACKAGE TYPE
 . D UPDATE^DIE(,"FDA",,"MSG") K FDA
 . I $D(MSG) K MSG D ERR("Filer Error;5",1) Q
 . S $P(@ROOT,U,2)=$P($G(@ROOT),U,2)+1       ; *** TOTAL SUCCESS
 D CLOSE^%ZISH("PRICE")
 Q
 ;
PC(LN,PC) ;
 Q $$TRIM^XLFSTR($P(LN,DLM,PC))
 ;
DIV(DV) ; Convert to INSTITUTION (#4) Pointer
 Q $O(^DIC(4,"D",(11100+DV),""))
 ;
 ; --------------------------- Interactive Modules ----------------------------
 ;
FILESET(FILENAME,PATH,TAG,ACT) ; File Setup
 N FILELOC,FILE,POP
 S FILELOC=$$PATH($G(PATH)) Q:FILELOC="" 0
 S FILE=$$ASK("Enter filename",FILENAME) Q:FILE="" 0
 D OPEN^%ZISH(TAG,FILELOC,FILE,ACT)
 I POP W !!,"Could not open file.",!! Q 0
 D USE^%ZISUTL(TAG)
 Q 1_U_FILE_U_FILELOC
 ;
PATH(PATH) ; Default Path
 S:$G(PATH)="" PATH=$S($$OS^%ZOSV["NT":"C:\HFS\",$$OS^%ZOSV["UNIX":"/hfs/")
 Q $$ASK("Enter directory name or path",PATH)
 ;
ASK(MSG,VAL) ; Ask Prompter
 N DIR,X,Y,DIRUT
 S:$G(MSG)'="" DIR("A")=MSG
 S DIR(0)="FOr^1:255",DIR("B")=$G(VAL) D ^DIR Q:$D(DIRUT) ""
 Q Y
 ;
 ; ------------------------------ FILE SUPPORT --------------------------------
 ;
DOTS(MSG,TAG) ; Write Dots to the Screen
 N WR D SWITCH("|TNT|") S WR="W "_$S($D(MSG):"!,MSG,!",1:""".""")
 X WR D SWITCH($G(TAG,"PARSE"))
 Q
 ;
SWITCH(TAG,FLG) ; Switch the Device and Return the Current
 N IEN,NEW
 I '$G(FLG),TAG["|TNT|" S NEW=$P(IO("HOME"),U,2) D HOME^%ZIS Q
 K IEN S IEN=$O(^TMP("XUDEVICE",$J,"B",TAG,"")) Q:IEN=""
 S IO=$G(^TMP("XUDEVICE",$J,IEN,"IO")) D USE^%ZISUTL(TAG)
 Q
 ;
 ; ------------------------------ ERROR HANDLER -------------------------------
 ;
ERR(MSG,SEV) ; Interface Exception Handler
 N VMSG S VMSG(2)="LN"
 D XCPT^VFDXX($$NOW^XLFDT,"WHOLESALE PRICE LOAD",MSG,"",$G(SEV,1),MSG,.VMSG)
 S $P(@ROOT,U,3)=$P($G(@ROOT),U,3)+1         ; *** TOTAL EXCEPTIONS
 Q
 ;
 ; --------------------------------- DISPLAY ----------------------------------
 ;
 ; ^XTMP("VFDVXPSI-<APP>",DATE)=total^success^exceptions
 ;
HEADER(NAM) ; Dashboard Header
 N ROOT,DATE Q:$G(NAM)=""
 S ROOT=$NA(^XTMP("VFDVXPSI-"_NAM)) S DATE=$O(@ROOT@(""),-1) Q:DATE=""
 S:'$D(@ROOT) @ROOT@(0)=$$FMADD^XLFDT(DT,365)_U_$$NOW^XLFDT_"^VFDVXPSI"
 S ROOT=$NA(@ROOT@(DATE))
 W #,$$REPEAT^XLFSTR("=",79),!,$$CJ^XLFSTR(" PHARMACY IMPORT - "_NAM,79)
 W !,$$REPEAT^XLFSTR("-",79),!
 W !,"   Total Records: "_$P($G(@ROOT),U)
 W !,"         Success: "_$P($G(@ROOT),U,2)
 W !,"      Exceptions: "_$P($G(@ROOT),U,3)
 W !,$$REPEAT^XLFSTR("=",79),!
 Q
