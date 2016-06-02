MAGVORDR ;WOIFO/RRB/BT - MAGV Order Lookup ; 11 Jul 2012 3:13 PM
 ;;3.0;IMAGING;**118**;Mar 19, 2002;Build 4525;May 01, 2013
 ;; Per VHA Directive 2004-038, this routine should not be modified.
 ;; +---------------------------------------------------------------+
 ;; | Property of the US Government.                                |
 ;; | No permission to copy or redistribute this software is given. |
 ;; | Use of unreleased versions of this software requires the user |
 ;; | to execute a written test agreement with the VistA Imaging    |
 ;; | Development Office of the Department of Veterans Affairs,     |
 ;; | telephone (301) 734-0100.                                     |
 ;; | The Food and Drug Administration classifies this software as  |
 ;; | a medical device.  As such, it may not be changed in any way. |
 ;; | Modifications to this software may result in an adulterated   |
 ;; | medical device under 21CFR820, the use of which is considered |
 ;; | to be a violation of US Federal Statutes.                     |
 ;; +---------------------------------------------------------------+
 ;;
 ;
 ;
 ; Lookup the patient/study in the imaging service's database
 ; The imaging service, IMGSVC (RAD or CON), and case number, CASENUMB(accession)
 ; are required variables that must be passed to the LOOKUP subroutine.
 ; 
 ; 
 ; Output will be in the form of a string:
 ; 
 ;     Happy case example: 0~DFN~SITE (0~12345~660
 ;     
 ;     Incorrect accession # format or not present: -1~BAD CASE #
 ;     
 ;     No case on file: -1~NO CASE #
 ;     
 ;
 Q
 ;
LOOKUP(CASENUMB,IMGSVC) ; MAGV Order Lookup
 ;
 N RADATA
 ;
 I IMGSVC'="RAD",IMGSVC'="CON" Q "-1,INVALID IMAGE SERVICE"
 I $G(CASENUMB)="" Q "-1~BAD CASE #"
 ; 
 I IMGSVC="RAD" D
 . S RADATA=$$RADLKUP(CASENUMB)
 . Q
 I IMGSVC="CON" D
 . I CASENUMB'?1"GMRC-".1N.N S RADATA="-1~BAD CASE #" Q
 . S RADATA=$$CONLKUP(CASENUMB)
 . Q
 ;
 Q RADATA
 ;
RADLKUP(CASENUMB) ; Radiology patient/study lookup
 ;
 ; returns RADATA array DFN, DATETIME, and PROCDESC
 ;
 N CPTCODE ;-- CPT code for the procedure
 N CPTNAME ;-- CPT name for the procedure
 N DFN
 N EXAMSTS ;-- Exam status (don't post images to CANCELLED exams)
 N PROCIEN ;-- radiology procedure ien in ^RAMIS(71)
 N RAIX ;----- cross reference subscript for case number lookup
 N RADPT1 ;--- first level subscript in ^RADPT
 N RADPT2 ;--- second level subscript in ^RADPT (after "DT")
 N RADPT3 ;--- third level subscript in ^RADPT (after "P")
 N SITE
 N I,LIST,VARIABLE,X,Z
 ;
 ; find the patient/study in ^RARPT using the Radiology Case Number
 ; 
 S X=$S(CASENUMB'["-"!($L($T(ACCFIND^RAAPI))=0):$$OLDCASE(CASENUMB,.LIST),1:$$ACCFIND^RAAPI(CASENUMB,.LIST))
 I X'=1 Q "-1~NO CASE #"  ; No Case
 ;
 S X=LIST(1) ; two conditions, no accession number & duplicate
 S RADPT1=$P(X,"^",1),RADPT2=$P(X,"^",2),RADPT3=$P(X,"^",3)
 I RADPT1=""!(RADPT2="")!(RADPT3="") Q "-1~BAD CASE #"
 ;
 I '$D(^RADPT(RADPT1,0)) Q "-1~NO CASE #"  ; no patient demographics file pointer
 ;
 ; get patient demographics file pointer
 S X=^RADPT(RADPT1,0),DFN=$P(X,"^")
 S SITE=$P($G(^RADPT(RADPT1,"DT",RADPT2,0)),"^",3)
 ;
 ; do not include cancelled exam
 S EXAMSTS=$P(^RADPT(RADPT1,"DT",RADPT2,"P",RADPT3,0),"^",3)
 I EXAMSTS="" Q "-1~BAD CASE #"
 S EXAMSTS=$$GET1^DIQ(72,EXAMSTS,.01)
 I EXAMSTS="" Q "-1~BAD CASE #"
 I EXAMSTS="CANCELLED" Q "-1~NO CASE #"
 ;
 Q "0~"_DFN_"~"_SITE
 ;
CONLKUP(CASENUMB) ; CPRS Consult/Procedure patient/study lookup
 ;
 N DFN
 N EXAMSTS ;-- Exam status (don't post images to CANCELLED exams)
 N GMRCIEN
 N SITE
 ;
 S GMRCIEN=$P(CASENUMB,"-",2)
 S DFN=$$GET1^DIQ(123,GMRCIEN,.02,"I")
 I DFN="" Q "-1~NO CASE #" ; no patient demographics file pointer
 S SITE=$$GET1^DIQ(123,GMRCIEN,.05,"I")
 I SITE="" Q "-1~NO CASE #"  ; incomplete consult study
 ;
 S EXAMSTS=$$GET1^DIQ(123,GMRCIEN,8) ; check for cancelled exam
 I EXAMSTS="CANCELLED" Q "-1~NO CASE #"
 ;
 Q "0~"_DFN_"~"_SITE
 ;
OLDCASE(CASENUMB,LIST)  ; Lookup case numbers using old method
 ;
 S RAIX=$S($D(^RADPT("C")):"C",CASENUMB["-":"ADC",1:"AE") ; for Radiology Patch RA*5*7
 S RADPT1=$O(^RADPT(RAIX,CASENUMB,"")) I 'RADPT1 Q 0
 S RADPT2=$O(^RADPT(RAIX,CASENUMB,RADPT1,"")) I 'RADPT2 Q 0
 S RADPT3=$O(^RADPT(RAIX,CASENUMB,RADPT1,RADPT2,"")) I 'RADPT3 Q 0
 S LIST(1)=RADPT1_"^"_RADPT2_"^"_RADPT3
 Q 1  ; Success
