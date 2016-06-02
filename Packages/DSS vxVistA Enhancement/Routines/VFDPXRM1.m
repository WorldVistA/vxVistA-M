VFDPXRM1 ;DSS/RAF - PXRM COMPUTED FINDINGS UTILITY ; 11/15/11 3:07pm
 ;;2011.1.2;DSS,INC VXVISTA OPEN SOURCE;;11 Jun 2013;Build 164
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;
 ;ICR#   Supported Description
 ;----   ---------------------
 ; 2051  ^DIC: FIND1
 ; 2056  ^DIQ: GET1, GETS
 ;10103  $$FMDIFF^XLFDT
 ;10061  IN5^VADPT
 ;      
LMP(DFN,TEST,DATE,VALUE,TEXT) ; check length of time since last menstrual period
 ; DFN = PATIENT IDENTIFIER
 ; TEST = [requuired] return 1 or zero based on true or false
 ; DATE = [required] FM date
 ; VALUE = [optional] display value for reminder
 ; TEXT = [optional] display text
 ; In order to interactively test a reminder you must replace TEST with another name
 ; for example, ZTEST or XXX. Somehow, when triggered from the reminder computed finding
 ; the implicit new of TEST doesn't seem to effect the value of TEST being set in this routine
 ; if PXRM*1.5*12 were installed I should probably use EN^GMRVPXRM(.DATA,DAS) call
 ; DBIA 3647
 S (VALUE,TEST)=0,(DATE,TEXT)=""
 N VDATE,IEN
 S VDATE=+$O(^PXRMINDX(120.5,"IP",23,DFN,""),-1) I VDATE>0 D
 .S IEN=$O(^PXRMINDX(120.5,"IP",23,DFN,VDATE,""))
 .S DATE=$$GET1^DIQ(120.5,IEN,1.2,"I",)  ;date entered as RATE
 .S VALUE=$$FMDIFF^XLFDT(DT,DATE,1)
 .I +$G(VALUE)>0 S TEST=1
 Q
 ;
ADMITDT(DFN,TEST,DATE,VALUE,TEXT)  ; find last admission date/time
 ; DFN = PATIENT IDENTIFIER
 ; TEST = [requuired] return 1 or zero based on true or false
 ; DATE = [required] returns FM date
 ; VALUE = [optional] display value for reminder
 ; TEXT = [optional] display text
 ; VAIP(13) = IEN of admission in patient movement file
 ; VAIP(13,1) = admission date/time (FM) in 1st piece
 ; VAIP(17) = IEN of discharge in patient movement file
 ; VAIP(17,1) = discharge date/time (FM) in 1st piece
 N VAIP
 S VAIP("D")="LAST",(VALUE,TEST)=0,(DATE,TEXT)=""
 D IN5^VADPT
 S DATE=$P(VAIP(13,1),U) I +DATE>0 S TEST=1,TEXT=$P(VAIP(13,1),U,2)
 I DATE="" S TEST=0
 Q
 ;
DISCHARG(DFN,TEST,DATE,VALUE,TEXT)  ; find the last discharge date/time
 ; DFN = PATIENT IDENTIFIER
 ; TEST = [requuired] return 1 or zero based on true or false
 ; DATE = [required] returns FM date
 ; VALUE = [optional] display value for reminder
 ; TEXT = [optional] display text
 ; VAIP(13) = IEN of admission in patient movement file
 ; VAIP(13,1) = admission date/time (FM) in 1st piece
 ; VAIP(17) = IEN of discharge in patient movement file
 ; VAIP(17,1) = discharge date/time (FM) in 1st piece
 N VAIP
 S VAIP("D")="LAST",(VALUE,TEST)=0,(DATE,TEXT)=""
 D IN5^VADPT
 S DATE=$P(VAIP(17,1),U) I +DATE>0 S TEST=1,TEXT=$P(VAIP(17,1),U,2)
 I DATE="" S TEST=0
 Q
 ;
ENROADT(DFN,TEST,DATE,VALUE,TEXT,OTHER)  ;find most recent enrollment ADMISSION date for passed in location
 ; COMPUTED FINDING PARAMETER is being used to pass in the location IEN
 ; in variable FINDPA(15)
 ;      FINDPA(15) should be reliable because the 15 comes from the 
 ;                 field number in file 811.9 (REMINDER DEFINITION)
 S TEST=0  ;test is being passed data from Reminders and needs to be reset
 N ENRIEN,ENTRY,IENS,LASTIEN,LOC,VFDLST
 S ENRIEN=$$FIND1^DIC(2.001,","_DFN_",","QOX",+$G(FINDPA(15)),"B","","MSG")
 I +$G(ENRIEN)'>0 S TEST=0 Q  ;added 11/1/11
 S LASTIEN=$O(^DPT(DFN,"DE",ENRIEN,1," "),-1)  ; added 7/28/2011 to get most recent
 S IENS=ENRIEN_","_DFN_","
 D GETS^DIQ(2.001,IENS,"**","IE","VFDLST")
 S LOC=$G(VFDLST(2.001,ENRIEN_","_DFN_",",.01,"E"))
 S ENTRY=LASTIEN_","_ENRIEN_","_DFN_","
 S DATE=$G(VFDLST(2.011,ENTRY,.01,"I")),VALUE=$G(VFDLST(2.011,ENTRY,.01,"E"))
 S TEXT="Most recent enrollment Admission Date for "_$G(LOC)
 S TEST=$S(+$G(DATE)>0:1,1:0)
 Q
 ;
ENRODDT(DFN,TEST,DATE,VALUE,TEXT,OTHER)  ;find most recent enrollment DISCHARGE date for passed in location
 ; COMPUTED FINDING PARAMETER is being used to pass in the location IEN
 ; in variable FINDPA(15)
 ;      FINDPA(15) should be reliable because the 15 comes from the 
 ;                 field number in file 811.9 (REMINDER DEFINITION)
 S TEST=0  ;test is being passed data from Reminders and needs to be reset
 N ENRIEN,ENTRY,IENS,LASTIEN,LOC,VFDLST
 S ENRIEN=$$FIND1^DIC(2.001,","_DFN_",","QOX",+$G(FINDPA(15)),"B","","MSG")
 I +$G(ENRIEN)'>0 S TEST=0 Q  ;added 11/1/11
 S LASTIEN=$O(^DPT(DFN,"DE",ENRIEN,1," "),-1)  ; added 7/28/2011 to get most recent
 S IENS=ENRIEN_","_DFN_","
 D GETS^DIQ(2.001,IENS,"**","IE","VFDLST")
 S LOC=$G(VFDLST(2.001,ENRIEN_","_DFN_",",.01,"E"))
 S ENTRY=LASTIEN_","_ENRIEN_","_DFN_","
 S DATE=$G(VFDLST(2.011,ENTRY,3,"I")),VALUE=$G(VFDLST(2.011,ENTRY,3,"E"))
 S TEXT="Most recent enrollment Discharge Date for "_$G(LOC)
 S TEST=$S(+$G(DATE)>0:1,1:0)
 Q
