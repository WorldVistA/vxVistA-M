MAGVIM10 ;WOIFO/PMK/MLS/SG/DAC/JSL/MAT - Imaging RPCs for Importer ; 11 Oct 2012 3:13 PM
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
 Q
 ;
 ;***** RETURNS THE LIST OF RADIOLOGY PROCEDURES
 ; RPC: MAGV GET RADIOLOGY PROCEDURES
 ;
 ;      Modified from PROC^MAGDRPCA (RPC: MAG DICOM RADIOLOGY PROCEDURES)
 ;        for MAG*3.0*118.
 ;
 ; .ARRAY        Reference to a local variable where results
 ;               are returned to.
 ;
 ; STATIONUM     STATION NUMBER (#99) of an INSTITUTION file (#4) entry.
 ;
 ; [FILTER]      If 1, does not return procedure types of "B"road or "P"arent.
 ; 
 ; NOTE
 ; ====
 ; 
 ; The call to $$IEN^XUAF4() is Supported IA#1271
 ;
GETPROCS(ARRAY,STATIONUM,FILTER) ;
 N IMAGTYPE      ; IEN of the imaging type (file #79.2)
 N INACTDAT      ; Inactivation date of the procedure
 N OMLDAT        ; Outside imaging location data (file #2006.5759)
 N OMLIEN        ; IEN in OUTSIDE IMAGING LOCATION file (#2006.5759)
 N RADPROC       ; Radiology procedure data (file #71)
 N TODAY         ; today's date in Fileman format
 N PROCTYPE      ; Type of procedure
 ;
 N BUF,ERROR,IEN,Z
 K ARRAY
 ;
 ;--- Validate parameters
 S STATIONUM=$G(STATIONUM)
 I (STATIONUM'>0)!(STATIONUM'=+STATIONUM)  D  Q
 . S ARRAY(1)="-1,Invalid STATION NUMBER: '"_STATIONUM_"'."
 . Q
 ;
 ;--- Get IEN of INSTITUTION file (#4) from STATION NUMBER (Supported IA# 2171)
 N IENINST ; IEN of INSTITUTION file (#4).
 S IENINST=$$IEN^XUAF4(STATIONUM)
 ;
 I IENINST=""  D  Q
 . S ARRAY(1)="-2,Could not resolve Institution from STATION NUMBER '"_STATIONUM_"'."
 . Q
 ;
 S ERROR=$$DISPLAY^MAGDAIRG(0)
 I ERROR=-1 D  Q
 . S ARRAY(1)="-3,""No Credit"" entries must be added to the IMAGING LOCATIONS file (#79.1)"
 . S ARRAY(2)=""
 . S ARRAY(3)="Use the IMPORTER MENU option CHECK OUTSIDE IMAGING LOCATION FILE"
 . S ARRAY(4)="on the VistA system to correct the problem."
 . Q
 I ERROR=-2 D  Q
 . S ARRAY(1)="-4,Entries must be added to the OUTSIDE IMAGING LOCATIONS file (#2006.5759)"
 . S ARRAY(2)=""
 . S ARRAY(3)="Use the IMPORTER MENU option BUILD OUTSIDE IMAGING LOCATION FILE"
 . S ARRAY(4)="on the VistA system to correct the problem."
 . Q
 I ERROR'=0 D  Q
 . S ARRAY(1)="-5,Unexpected error #"_ERROR_" returned by $$DISPLAY^MAGDAIRG(0)"
 . Q
 ;
 S (ARRAY(1),IEN)=0,TODAY=$$DT^XLFDT()
 F  S IEN=$O(^RAMIS(71,IEN))  Q:'IEN  D  ; Private IA (#1174) 
 . S RADPROC=^RAMIS(71,IEN,0),IMAGTYPE=+$P(RADPROC,U,12)
 . ;--- Get outside imaging location associated
 . ;--- with the imaging type of the procedure
 . S OMLIEN=$O(^MAGD(2006.5759,"D",IENINST,IMAGTYPE,""))  Q:'OMLIEN
 . S OMLDAT=$G(^MAGD(2006.5759,OMLIEN,0))
 . Q:$P(OMLDAT,U,4)'=IENINST  ; Has to be in the same INSTITUTION.
 . ;--- Prepare the procedure descriptor
 . S BUF=$P(RADPROC,U)_U_IEN      ; Procedure Name and IEN
 . S PROCTYPE=$P(RADPROC,U,6)     ; Type of Procedure
 . I $G(FILTER)=1,(PROCTYPE="B")!(PROCTYPE="P") Q
 . S $P(BUF,U,3)=PROCTYPE         ; Type of Procedure
 . S $P(BUF,U,4)=$P(RADPROC,U,9)  ; CPT Code (file #81)
 . S $P(BUF,U,5)=IMAGTYPE         ; Type of Imaging (file #79.2)
 . S INACTDAT=$P($G(^RAMIS(71,IEN,"I")),U)
 . I INACTDAT,INACTDAT<TODAY Q    ; ignore inactive procedures
 . S $P(BUF,U,6)=INACTDAT         ; Inactivation Date
 . S $P(BUF,U,7)=$P(OMLDAT,U)     ; Imaging Location (file #79.1)
 . S Z=$P(OMLDAT,U,3)
 . S $P(BUF,U,8)=Z                ; Hospital Location (file #44) - IEN
 . S $P(BUF,U,9)=$$GET1^DIQ(44,Z,.01) ; Hospital Location (file #44) - NAME
 . ;--- Add the descriptor to the result array
 . S ARRAY(1)=ARRAY(1)+1,ARRAY(ARRAY(1)+1)=BUF
 . Q
 Q
 ;
 ; MAGVIM10
