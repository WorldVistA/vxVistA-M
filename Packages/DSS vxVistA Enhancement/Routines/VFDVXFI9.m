VFDVXFI9 ;Post-install Add standard file references for OMH;04/08/2015
 ;;1.0;VENDOR - DOCUMENT STORAGE SYS;**37**;17 Mar 2014;Build 3
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
 ;
ERR  ; report error message
 D MES^XPDUTL("Error in updating files for:  "_X)
 Q
 ; 
FDBID  ; move FSN field values to VA PRODUCT (#50.68) file, field 21601.03
 N CT,DISP,FSN,GEN,IEN50,IEN5068,R
 S DISP=0
 S U="^",(CT,IEN50)=0 F  S IEN50=$O(^PSDRUG(IEN50)) Q:'IEN50  S FSN=$P($G(^PSDRUG(IEN50,0)),U,6) I FSN]"" D
 . S IEN5068=$P($G(^PSDRUG(IEN50,"ND")),U,3) I IEN5068 S GEN=$P($G(^PSNDF(50.68,IEN5068,21601)),U,3) I GEN'=FSN D
 . . K FDA,MSG S FDA(50.68,IEN5068_",",21601.03)=FSN D FILE^DIE("S","FDA","MSG")
 . . I $D(MSG("DIERR")) D BMES^XPDUTL("Error filing Generic Med ID:  "_IEN5068)
 . . E  S CT=CT+1
 W !,CT," Record",$s(CT>1!(CT=0):"s",1:"")," updated"
 Q
 ;
SETFDBG ; This code is called by a trigger on field #6 (FSN) of file #50
 ;
 N GEN,IEN50,IEN5068,R
 S IEN50=+$G(DIIENS) I IEN50 D
 . S IEN5068=$P($G(^PSDRUG(IEN50,"ND")),U,3) I IEN5068 S GEN=$P($G(^PSNDF(50.68,IEN5068,21601)),U,3) I GEN'=X D
 . . K FDA,MSG S FDA(50.68,IEN5068_",",21601.03)=X D UPDATE^DIE("S","FDA","","MSG") ;I $D(MSG("DIERR")) W ! ZW FDA W ! ZW MSG W !
 Q
 ;
POST1  ; Move GCNSEQNO to PRODUCT(#50.68) file
 N DIR,ERR,FDA,FN,GCNSEQ,GENMED,IEN5068,REC5068,X
 S FN="FDB Drugs 2013-12 Actives 2.TXT" D DEFDIR^%ZISH(.DIR)
 D OPEN^%ZISH("GCN",DIR,FN,"R") I POP U 0 W !,"Unable to open file ",FN Q
 U IO R X:DTIME ;Get past header
 S U="^" F  U IO R X:DTIME Q:$$STATUS^%ZISH  S GENMED=$P(X,U,2),GCNSEQ=$P(X,U,3) I GENMED?1.N,GCNSEQ?1.N D
 . S IEN5068=0 F  S IEN5068=$O(^PSNDF(50.68,"VFDFDBG",GENMED,IEN5068)) Q:'IEN5068  D
 . . S REC5068=$G(^PSNDF(50.68,IEN5068,1)) I $P(REC5068,U,5)="" D
 . . . K ERR,FDA S FDA(50.68,IEN5068_",",11)=GCNSEQ D UPDATE^DIE("S","FDA",,"ERR")
 . . . I $D(ERR) D BMES^XPDUTL("Error filing GCNSEQ for VA PRODUCT #:  "_IEN5068) Q
 D CLOSE^%ZISH("GCN")
 Q
 ;
POST2  ; load NDC's from synonym sub-file and fixed NDC (#31) field in DRUG (#50) file into NDC/UPN (#50.67) file.
 N D0,ID,PSNDF,SID,VFDFDA
 S ID=0 F  S ID=$O(^PSDRUG(ID)) Q:'ID  D
 . S D0=$G(^PSDRUG(ID,"ND")) I 'D0 D MES^XPDUTL("DRUG ID:  "_ID_" not mapped to PRODUCT FILE") Q  ; no tie to NDF files?
 . S PSNDF=$P(D0,U,3) I 'PSNDF D MES^XPDUTL("DRUG ID:  "_ID_" not mapped to PRODUCT FILE") Q  ; no tie to NDF files?
 . S SID=0 F  S SID=$O(^PSDRUG(ID,1,SID)) Q:'SID  D
 . . S X=$P(^PSDRUG(ID,1,SID,0),U,2) Q:'X  ; no synonym found
 . . I X,'$D(^PSNDF(50.67,"NDC",X)) D
 . . . S VFDFDA(50.67,"+1,",.01)=$P(^PSNDF(50.67,0),U,3)+1
 . . . S VFDFDA(50.67,"+1,",1)=X
 . . . S VFDFDA(50.67,"+1,",5)=PSNDF
 . . . S VFDFDA(50.67,"+1,",8)=$$GET1^DIQ(50,ID,23,"I")
 . . . S VFDFDA(50.67,"+1,",9)=$$GET1^DIQ(50,ID,24,"I")
 . . . D UPDATE^DIE(,"VFDFDA")
 . . S X=$P($G(^PSDRUG(ID,2)),U,4)  ; check fixed NDC code
 . . I X,'$D(^PSNDF(50.67,"NDC",X)) D
 . . . S VFDFDA(50.67,"+1,",.01)=$P(^PSNDF(50.67,0),U,3)+1
 . . . S VFDFDA(50.67,"+1,",1)=X
 . . . S VFDFDA(50.67,"+1,",5)=PSNDF
 . . . S VFDFDA(50.67,"+1,",8)=$$GET1^DIQ(50,ID,23,"I")
 . . . S VFDFDA(50.67,"+1,",9)=$$GET1^DIQ(50,ID,24,"I")
 . . . D UPDATE^DIE(,"VFDFDA")
 Q
 ;
POST3  ; set GCNSEQNO in VA PRODUCT (#50.68) file given Generic Med ID
 ; uses "AHFS GCN GenMedID MedID.txt" file downloaded from FDB by Gwen Pike on 03/21/2014
 N FILENAME,PATH,POP,CNT,FLG,LINE,FFLG,GCNNO,HANDLE,L,LINE,P5068,TARG,X,Y
 S FILENAME="AHFS GCN GenMedID MedID.txt"
 S PATH=$$DEFDIR^%ZISH
 S (FFLG,POP)=0
 K ^TMP($T(+0))
 S I=$$FTG^%ZISH(PATH,FILENAME,$NA(^TMP($T(+0),$J,0)),3)
 I I<1 D BMES^XPDUTL("Unable to open source file:  "_FILENAME)
 S FILE=$NA(^TMP($T(+0),$J))
 S I=0 F  S I=$O(@FILE@(I)) Q:'I  D
 . S LINE=@FILE@(I)
 . S GCNNO=$P(LINE,$C(9),2),GENMED=$P(LINE,$C(9),3) Q:GENMED']""
 . S P5068=$O(^PSNDF(50.68,"VFDFDBG",GENMED,0)) Q:'P5068  ; no entry in Product file
 . K FDA S FDA(50.68,P5068_",",11)=GCNNO D FILE^DIE(,"FDA")
 K ^TMP($T(+0),$J)
 Q
 ;
POST4  ; fire off cross-references for NDF files
 N DA,DIK
 D BMES^XPDUTL("Re-indexing Va PRODUCT KEY")
 S DIK="^PSDRUG(",DIK(1)="22" S DA=0 F  S DA=$O(^PSDRUG(DA)) Q:'DA  D IX1^DIK  ; PSNDF VA PRODUCT NAME ENTRY
 D BMES^XPDUTL("Re-indexing GCNSEQNO")
 S DIK="^PSNDF(50.68,",DIK(1)="11" S DA=0 F  S DA=$O(^PSNDF(50.68,DA)) Q:'DA  D EN1^DIK  ; GCNSEQNO
 D BMES^XPDUTL("Re-indexing FDB fields")
 S DIK="^PSNDF(50.68," F DIK(1)=21601.01,21601.02,21601.03 S DA=0 F  S DA=$O(^PSNDF(50.68,DA)) Q:'DA  D EN1^DIK  ; FDB Numbers
 ;D BMES^XPDUTL("Re-indexing NDC/UPN file.")
 ;S DIK="^PSNDF(50.67," S DA=0 F  S DA=$O(^PSNDF(50.67,DA)) Q:'DA  D EN1^DIK  ; NDC/UPN file
 Q
 ;
STRIP(X)  ; strip off leading spaces
 Q $TR(X," ","")
 ;
CONVDIV(VDV)   ; convert to INSTITUTION (#4) pointer
 Q $O(^DIC(4,"D",(11100+VDV),0))
 ; 
CONVDATE(DOS)  ; convert McKesson date in FILEMAN date
 N D,E,F,MON,NEWD,NEWDD,X,Y,Z
 S Y=$P(DOS," ",1),Z=$P(DOS," ",2)
 S D=$P(Y,"-",1),E=$P(Y,"-",2),F=$P(Y,"-",3)
 S MON=$E(100+$S(E="JAN":1,E="FEB":2,E="MAR":3,E="APR":4,E="MAY":5,E="JUN":6,E="JUL":7,E="AUG":8,E="SEP":9,E="OCT":10,E="NOV":11,E="DEC":12),2,3)
 S NEWD=MON_"/"_$E(D+100,2,3)_"/"_(F+2000)
 S X=NEWD D ^%DT S NEWDD=Y_"."_$E(1000000+$TR(Z,".",""),2,7)
 Q NEWDD
 ;
DEL(FILE) ; -- delete file
 S GL=$P(^DIC(FILE,0,"GL"),",")_")"
 S DA=0
 F  S DA=$O(@GL@(DA)) Q:DA=""  D
 . S DIK=$P(GL,")")_"," D ^DIK
 Q
 ;
POST7  ; delete bogis cross-reference from DRUG (#50) file
 K ^PSDRUG("AIU,")
 Q
