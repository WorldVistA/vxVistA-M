VFDVXFI0 ;DSS/MKN - Pre/Post NewCrop 3.0 KID Build;04/08/2015
 ;;1.0;VENDOR - DOCUMENT STORAGE SYS;**37**;17 Mar 2014;Build 3
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
PRE  ; Pre-Installation routine
 Q
 ;
POST  ; Post-installation routine
 ; This POST routine will set up new Internal and External Pharmacy Order Checks
 N ERR,FDA,IEN19,IEN8994,S,VFDRT
 ;D FDBID^VFDVXFI9   ; MOVE FSN FIELD
 ;D POST1^VFDVXFI9   ; Move GCNSEQNO
 ;D POST2^VFDVXFI9   ; LOAD NDCS
 ;D POST3^VFDVXFI9   ; SET GCNSEQNO
 ;D EN^VFDVXFI8      ; LOAD NDCS FOR OMH
 ;D LFFHC^VFDVXFI7   ; SAME AS ABOVE
 ;D POST^VFDVXFI5    ; Update OTC/RX Indicator field
 N FDA,FDAIEN,FILE,HANDLE,PATH,REF,X,X1,X2,X3
 K FDA,VFDRT D POST4^VFDVXFI9  ; fire off cross-references
 K ^XTMP("VFDVXFI6")
 D POST7^VFDVXFI9    ; delete bogus index "AIU,"
 ;
 ; The following code will add the RPC "VFDVX RET PICKUP" to option "VFDVX CPRS GUI CHART"
 S IEN19=$O(^DIC(19,"B","VFDVX CPRS GUI CHART","")) I IEN19 D
 . S IEN8994=$O(^XWB(8994,"B","VFDVX RET PICKUP","")) I IEN8994 D
 . . I $O(^DIC(19,IEN19,"RPC","B",IEN8994))="" D
 . . . K FDA S FDA(19.05,"+1,"_IEN19_",",.01)=IEN8994 D UPDATE^DIE("S","FDA",,"ERR") I $D(ERR) D
 . . . . D BMES^XPDUTL("Error while attempting to add RPC ""VFDVX RET PICKUP"" to option ""VFDVX CPRS GUI CHART""")
 . . . . S S="ERR" F  S S=$Q(@S) Q:S=""  D BMES^XPDUTL(S_""=""_@S)
 ; Following code will add MOCHA Duplicate Drug Class item to ORDER CHECKS (#100.8) file.
 I $O(^ORD(100.8,"B","DUPLICATE DRUG CLASS ORDER",""))="" D
 . S FDA(100.8,"+1,",.01)="DUPLICATE DRUG CLASS ORDER" D UPDATE^DIE("S","FDA","IENS")
 . K WP
 . S WP(1)="Triggered at order acceptance for existing meds and at"
 . S WP(2)="conclusion of ordering session against meds ordered within that session. If"
 . S WP(3)="is currently receiving a med that is in the same Therapeutic "
 . S WP(4)="Category, the order check occurs."
 . D WP^DIE(100.8,IENS(1)_",",2,"","WP","MSG")
 ;
RUN ; VFD PHARM CONTROLLED SUB RUN (19.2 ENTRY)
 N IPT,DLAYGO S DLAYGO=19.2
 S IPT(19.2,"?+1,",.01)="VFD PHARM CONTROLLED SUB RUN"
 S IPT(19.2,"?+1,",3)="NULL"
 S IPT(19.2,"?+1,",6)="D@0800"
 S IPT(19.2,"?+1,",9)="Persistent"
 D UPDATE^DIE("E","IPT") K IPT
 Q
