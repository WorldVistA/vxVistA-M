OCXOZ0C ;SLC/RJS,CLA - Order Check Scan ;MAR 19,2013 at 14:13
 ;;3.0;ORDER ENTRY/RESULTS REPORTING;**32,221,243**;Dec 17,1997;Build 242
 ;;  ;;ORDER CHECK EXPERT version 1.01 released OCT 29,1998
 ;
 ; ***************************************************************
 ; ** Warning: This routine is automatically generated by the   **
 ; ** Rule Compiler (^OCXOCMP) and ANY changes to this routine  **
 ; ** will be lost the next time the rule compiler executes.    **
 ; ***************************************************************
 ;
 Q
 ;
CHK347 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK58+20^OCXOZ05.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK347 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(131) --> Data Field: PHARMACY LOCAL ID (FREE TEXT)
 ; OCXDF(136) --> Data Field: CLOZAPINE ANC W/IN 7 FLAG (BOOLEAN)
 ; OCXDF(137) --> Data Field: CLOZAPINE ANC W/IN 7 RESULT (NUMERIC)
 ; OCXDF(139) --> Data Field: CLOZAPINE WBC W/IN 7 FLAG (BOOLEAN)
 ; OCXDF(140) --> Data Field: CLOZAPINE WBC W/IN 7 RESULT (NUMERIC)
 ;
 ;      Local Extrinsic Functions
 ;
 S OCXDF(137)=$P($P($$CLOZLABS^ORKLR(OCXDF(37),7,OCXDF(131)),"^",3),";",2) I $L(OCXDF(137)) D CHK349
 S OCXDF(136)=$P($P($$CLOZLABS^ORKLR(OCXDF(37),7,OCXDF(131)),"^",3),";",1) I $L(OCXDF(136)),'(OCXDF(136)) D CHK371
 S OCXDF(139)=$P($P($$CLOZLABS^ORKLR(OCXDF(37),7,OCXDF(131)),"^",2),";",1) I $L(OCXDF(139)),'(OCXDF(139)) D CHK375
 S OCXDF(140)=$P($P($$CLOZLABS^ORKLR(OCXDF(37),7,OCXDF(131)),"^",2),";",2) I $L(OCXDF(140)) D CHK378
 Q
 ;
CHK349 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK347+15.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK349 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(131) --> Data Field: PHARMACY LOCAL ID (FREE TEXT)
 ; OCXDF(136) --> Data Field: CLOZAPINE ANC W/IN 7 FLAG (BOOLEAN)
 ; OCXDF(137) --> Data Field: CLOZAPINE ANC W/IN 7 RESULT (NUMERIC)
 ;
 ;      Local Extrinsic Functions
 ;
 I (OCXDF(137)<1.5) S OCXDF(136)=$P($P($$CLOZLABS^ORKLR(OCXDF(37),7,OCXDF(131)),"^",3),";",1) I $L(OCXDF(136)),(OCXDF(136)) D CHK353
 I (OCXDF(137)>1.499) D CHK355
 Q
 ;
CHK353 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK349+13.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK353 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(130) --> Data Field: CLOZAPINE LAB RESULTS (FREE TEXT)
 ; OCXDF(131) --> Data Field: PHARMACY LOCAL ID (FREE TEXT)
 ;
 ;      Local Extrinsic Functions
 ; FILE(DFN,114, ----> FILE DATA IN PATIENT ACTIVE DATA FILE  (Event/Element: CLOZAPINE ANC < 1.5)
 ;
 S OCXDF(130)=$P($$CLOZLABS^ORKLR(OCXDF(37),"",OCXDF(131)),"^",4),OCXOERR=$$FILE(DFN,114,"130") Q:OCXOERR 
 Q
 ;
CHK355 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK349+14.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK355 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(131) --> Data Field: PHARMACY LOCAL ID (FREE TEXT)
 ; OCXDF(136) --> Data Field: CLOZAPINE ANC W/IN 7 FLAG (BOOLEAN)
 ; OCXDF(137) --> Data Field: CLOZAPINE ANC W/IN 7 RESULT (NUMERIC)
 ;
 ;      Local Extrinsic Functions
 ;
 S OCXDF(136)=$P($P($$CLOZLABS^ORKLR(OCXDF(37),7,OCXDF(131)),"^",3),";",1) I $L(OCXDF(136)),(OCXDF(136)) D CHK358
 I (OCXDF(137)<"2.0") S OCXDF(136)=$P($P($$CLOZLABS^ORKLR(OCXDF(37),7,OCXDF(131)),"^",3),";",1) I $L(OCXDF(136)),(OCXDF(136)) D CHK506^OCXOZ0F
 Q
 ;
CHK358 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK355+13.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK358 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(130) --> Data Field: CLOZAPINE LAB RESULTS (FREE TEXT)
 ; OCXDF(131) --> Data Field: PHARMACY LOCAL ID (FREE TEXT)
 ;
 ;      Local Extrinsic Functions
 ; FILE(DFN,115, ----> FILE DATA IN PATIENT ACTIVE DATA FILE  (Event/Element: CLOZAPINE ANC >= 1.5)
 ;
 S OCXDF(130)=$P($$CLOZLABS^ORKLR(OCXDF(37),"",OCXDF(131)),"^",4),OCXOERR=$$FILE(DFN,115,"130") Q:OCXOERR 
 Q
 ;
CHK360 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK198+9^OCXOZ09.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK360 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(43) ---> Data Field: OI NATIONAL ID (FREE TEXT)
 ; OCXDF(74) ---> Data Field: VA DRUG CLASS (FREE TEXT)
 ; OCXDF(131) --> Data Field: PHARMACY LOCAL ID (FREE TEXT)
 ; OCXDF(132) --> Data Field: CLOZAPINE MED (BOOLEAN)
 ;
 ;      Local Extrinsic Functions
 ;
 S OCXDF(131)=$P($P($G(OCXPSD),"|",3),"^",4) I $L(OCXDF(131)) S OCXDF(37)=$G(DFN) I $L(OCXDF(37)) S OCXDF(132)=$P($$CLOZLABS^ORKLR(OCXDF(37),7,OCXDF(131)),"^",1) D CHK365
 S OCXDF(43)=$P($P($G(OCXPSD),"|",3),"^",1) I $L(OCXDF(43)) S OCXDF(74)=$P($$ENVAC^PSJORUT2(OCXDF(43)),"^",2) I $L(OCXDF(74)) D CHK498^OCXOZ0F
 Q
 ;
CHK365 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK360+14.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK365 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(130) --> Data Field: CLOZAPINE LAB RESULTS (FREE TEXT)
 ; OCXDF(131) --> Data Field: PHARMACY LOCAL ID (FREE TEXT)
 ; OCXDF(132) --> Data Field: CLOZAPINE MED (BOOLEAN)
 ;
 ;      Local Extrinsic Functions
 ; FILE(DFN,116, ----> FILE DATA IN PATIENT ACTIVE DATA FILE  (Event/Element: CLOZAPINE DRUG SELECTED)
 ;
 I $L(OCXDF(132)),(OCXDF(132)) S OCXDF(130)=$P($$CLOZLABS^ORKLR(OCXDF(37),"",OCXDF(131)),"^",4),OCXOERR=$$FILE(DFN,116,"130") Q:OCXOERR 
 Q
 ;
CHK371 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK347+16.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK371 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(130) --> Data Field: CLOZAPINE LAB RESULTS (FREE TEXT)
 ; OCXDF(131) --> Data Field: PHARMACY LOCAL ID (FREE TEXT)
 ;
 ;      Local Extrinsic Functions
 ; FILE(DFN,117, ----> FILE DATA IN PATIENT ACTIVE DATA FILE  (Event/Element: CLOZAPINE NO ANC W/IN 7 DAYS)
 ;
 S OCXDF(130)=$P($$CLOZLABS^ORKLR(OCXDF(37),"",OCXDF(131)),"^",4),OCXOERR=$$FILE(DFN,117,"130") Q:OCXOERR 
 Q
 ;
CHK375 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK347+17.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK375 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(130) --> Data Field: CLOZAPINE LAB RESULTS (FREE TEXT)
 ; OCXDF(131) --> Data Field: PHARMACY LOCAL ID (FREE TEXT)
 ;
 ;      Local Extrinsic Functions
 ; FILE(DFN,118, ----> FILE DATA IN PATIENT ACTIVE DATA FILE  (Event/Element: CLOZAPINE NO WBC W/IN 7 DAYS)
 ;
 S OCXDF(130)=$P($$CLOZLABS^ORKLR(OCXDF(37),"",OCXDF(131)),"^",4),OCXOERR=$$FILE(DFN,118,"130") Q:OCXOERR 
 Q
 ;
CHK378 ; Look through the current environment for valid Event/Elements for this patient.
 ;  Called from CHK347+18.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local CHK378 Variables
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(131) --> Data Field: PHARMACY LOCAL ID (FREE TEXT)
 ; OCXDF(139) --> Data Field: CLOZAPINE WBC W/IN 7 FLAG (BOOLEAN)
 ; OCXDF(140) --> Data Field: CLOZAPINE WBC W/IN 7 RESULT (NUMERIC)
 ;
 ;      Local Extrinsic Functions
 ;
 I (OCXDF(140)<"3.0") S OCXDF(139)=$P($P($$CLOZLABS^ORKLR(OCXDF(37),7,OCXDF(131)),"^",2),";",1) I $L(OCXDF(139)),(OCXDF(139)) D CHK382^OCXOZ0D
 I (OCXDF(140)>2.999),(OCXDF(140)<3.5) S OCXDF(139)=$P($P($$CLOZLABS^ORKLR(OCXDF(37),7,OCXDF(131)),"^",2),";",1) I $L(OCXDF(139)),(OCXDF(139)) D CHK388^OCXOZ0D
 I (OCXDF(140)>3.499) S OCXDF(139)=$P($P($$CLOZLABS^ORKLR(OCXDF(37),7,OCXDF(131)),"^",2),";",1) I $L(OCXDF(139)),(OCXDF(139)) D CHK393^OCXOZ0D
 Q
 ;
FILE(DFN,OCXELE,OCXDFL) ;     This Local Extrinsic Function logs a validated event/element.
 ;
 N OCXTIMN,OCXTIML,OCXTIMT1,OCXTIMT2,OCXDATA,OCXPC,OCXPC,OCXVAL,OCXSUB,OCXDFI
 S DFN=+$G(DFN),OCXELE=+$G(OCXELE)
 ;
 Q:'DFN 1 Q:'OCXELE 1 K OCXDATA
 ;
 S OCXDATA(DFN,OCXELE)=1
 F OCXPC=1:1:$L(OCXDFL,",") S OCXDFI=$P(OCXDFL,",",OCXPC) I OCXDFI D
 .S OCXVAL=$G(OCXDF(+OCXDFI)),OCXDATA(DFN,OCXELE,+OCXDFI)=OCXVAL
 ;
 M ^TMP("OCXCHK",$J,DFN)=OCXDATA(DFN)
 ;
 Q 0
 ;
