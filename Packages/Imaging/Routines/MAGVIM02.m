MAGVIM02 ;WOIFO/MAT - Utilities for RPC calls for DICOM file processing ; 16 Mar 2012 3:19 PM
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
 ; +++++ Wrap calls to___    To retrieve____________
 ; 
 ;        OER^GMRCSLM1 ..... consult/procedure list (IA #2740).
 ;        ORDERS^MAGDRPCB .. Radiology Exam List.
 ;
 ;  RPC: MAGV GET PAT ORDERS
 ;
 ; INPUTS
 ; ======
 ;
 ;  MAGVRY ........... Name of output array, passed by reference.
 ;  PIDENT ........... EnterprisePatientID
 ;  PIDTYPE .......... EnterprisePatientIDType
 ;  PIDAUTHN ......... AssigningAuthority
 ;  PIDCR8OR ......... CreatingEntity
 ;  ORDRTYPE ......... OrderType
 ;  ORDTSTRT ......... DateStart
 ;  ORDTSTOP ......... DateEnd
 ;
 ; OUTPUTS
 ; =======
 ;
 ;  Array of radiology or consult orders as f(OrderType). Descriptions of each
 ;  appear before their respective tags, GETORCON & GETORAD.
 ;
 ;  NOTES
 ;  =====
 ;
GETORD(MAGVRY,PIDENT,PIDTYPE,PIDAUTHN,PIDCR8OR,ORDRTYPE,ORDTSTRT,ORDTSTOP) ;
 ;
 ;--- Initialize.
 K RETURN
 ;
 ;--- Set output array separators per MAG*3.0*34 convention.
 N SEPOUTP,SEPSTAT D ZRUSEPIN
 ;
 ;--- Validate inputs exist. External calls validate further.
 N MAGVERR S MAGVERR=0
 D
 . I '$D(ORDRTYPE)!(ORDRTYPE'?3U) S MAGVERR="Undefined or mis-formatted Order Type." Q
 . I "CON/RAD"'[ORDRTYPE S MAGVERR="Unsupported Order Type "_ORDRTYPE_"." Q
 . I '$D(ORDTSTRT) S MAGVERR="Undefined Order Start Date." Q
 . I '$D(ORDTSTOP) S MAGVERR="Undefined Order Stop Date." Q
 . I ORDTSTRT'?8N S MAGVERR="Unexpected Order Start Date format." Q
 . I ORDTSTOP'?8N S MAGVERR="Unexpected Order Stop Date format." Q
 . Q
 ;
 I MAGVERR'=0 S MAGVRY(0)="-1"_SEPSTAT_MAGVERR Q
 ;
 ;--- Convert incoming MMDDYYYY dates to FileMan format.
 D DT^DILF(,ORDTSTRT,.ORDTSTRT)
 D DT^DILF(,ORDTSTOP,.ORDTSTOP)
 ;
 ;--- Set defaults for incoming undefined.
 S:PIDTYPE="" PIDTYPE="D"
 S:PIDAUTHN="" PIDAUTHN="V"
 S:PIDCR8OR="" PIDCR8OR="Unknown." ;???_Lookup_in_^MAGV(2005.62,_Field_#.03
 ;
 ;--- Filter consults by ORDER STATUS file (#100.01) IEN's.
 N ORDRSTAT S ORDRSTAT="3,4,5,6,8,9,10,15,16"
 ;
 ;--- Branch to processor.
 D:ORDRTYPE["CON" GETORCON(PIDENT,"",ORDTSTRT,ORDTSTOP,ORDRSTAT,ORDRTYPE)
 D:ORDRTYPE["RAD" GETORRAD(PIDENT,ORDRTYPE)
 M MAGVRY=RETURN K RETURN
 Q
 ;
 ;+++++ Wrap call to OER^GMRCSLM1 (IA #2740).
 ;
 ;
GETORCON(MAGVPDFN,MAGVNULL,STARTDT,STOPDT,STATUS,OTYPE) ;
 ;
 S MAGVNULL=""
 ;
 ;--- IA #2740.
 D OER^GMRCSLM1(MAGVPDFN,MAGVNULL,STARTDT,STOPDT,STATUS,1)
 ;
 ;--- Re-format output w/o trailing "0" subscript.
 N MAGVGBL S MAGVGBL=$NA(^TMP("GMRCR",$J,"CS",1,0))
 ;
 ;--- Process if none found, or error, & QUIT.
 I $E($G(@MAGVGBL))="<" D  Q
 . ;
 . I $G(@MAGVGBL)["PATIENT DOES NOT HAVE ANY" D  Q
 . . S RETURN(0)="0"_SEPSTAT_"0"
 . S RETURN(0)="-1"_SEPSTAT_$G(@MAGVGBL)
 . Q
 ;
 S MAGVGBL=$NA(^TMP("GMRCR",$J,"CS",0))
 ;
 ;--- Process expected output.
 E  D
 . N LINETOT S LINETOT=$P($G(@MAGVGBL),U,4)
 . S RETURN(0)="0"_SEPSTAT_LINETOT
 . N CT F CT=1:1:LINETOT S MAGVGBL=$Q(@MAGVGBL) D
 . . N DATA S DATA=$G(@MAGVGBL)
 . . K MAGV
 . . ;
 . . ;--- Parse incoming data.
 . . S MAGV("GMRCDFN")=$P(DATA,U,1)
 . . S MAGV("REQSTDT")=$$FMTE^XLFDT($P(DATA,U,2),1)
 . . S MAGV("ORDRSTAT")=$P(DATA,U,3)
 . . S MAGV("SERVTO")=$P(DATA,U,4)
 . . S MAGV("NMPROCON")=$P(DATA,U,5)
 . . S MAGV("SERVFROM")=$P(DATA,U,6)
 . . S MAGV("CONTITLE")=$P(DATA,U,7)
 . . S MAGV("OERRDFN")=$P(DATA,U,8)
 . . ;
 . . ;--- record type for proper GUI icon
 . . ;--- "C"=reg cons, "P"=reg proc, "M"=clin proc, "I"=IF cons, "R"=IF proc
 . . S MAGV("CONTYPE")=$P(DATA,U,9)
 . . ;
 . . ;--- First line of Reason for Request (#123.01,.01).
 . . S MAGV("ORDREASN")=$$GET1^DIQ(123.01,"1,"_MAGV("GMRCDFN")_",",.01)
 . . ;
 . . S MAGV("CLINPROC")=$$GET1^DIQ(123,MAGV("GMRCDFN"),1.01)
 . . ;
 . . ;--- Re-format output.
 . . N LINEOUT S LINEOUT=$QS(MAGVGBL,4)
 . . S $P(RETURN(LINEOUT),U,1)=OTYPE
 . . S $P(RETURN(LINEOUT),U,2)=MAGVPDFN
 . . S $P(RETURN(LINEOUT),U,3)=MAGV("REQSTDT")
 . . S $P(RETURN(LINEOUT),U,4)=MAGV("ORDREASN")
 . . S $P(RETURN(LINEOUT),U,5)=MAGV("SERVTO")
 . . S $P(RETURN(LINEOUT),U,7)="GMRC-"_MAGV("GMRCDFN")
 . . S $P(RETURN(LINEOUT),U,8)=MAGV("ORDRSTAT")
 . . S $P(RETURN(LINEOUT),U,9)=MAGV("GMRCDFN")
 . . S $P(RETURN(LINEOUT),U,10)=MAGV("CONTITLE")
 . . S $P(RETURN(LINEOUT),U,11)=$G(MAGV("CLINPROC"))
 . . S $P(RETURN(LINEOUT),U,12)=$G(MAGV("EXAMIEN"))
 . . S $P(RETURN(LINEOUT),U,13)=$G(MAGV("RAOIEN"))
 . . S $P(RETURN(LINEOUT),U,14)=$G(MAGV("EXAMIEN2"))
 . . S $P(RETURN(LINEOUT),U,15)=$G(MAGV("ORDRPHYS"))
 . . S $P(RETURN(LINEOUT),U,16)=$G(MAGV("ORDRLOCIEN"))
 . . S $P(RETURN(LINEOUT),U,17)=$G(MAGV("PROCMOD"))
 . . ;
 . . S RETURN($QS(MAGVGBL,4))=$TR(RETURN(LINEOUT),U,SEPOUTP)
 . . Q
 . K MAGV
 . Q
 Q
 ;
 ;+++++ Wrap call to modified code from ORDERS^MAGDRPCB (RPC: MAG DICOM GET RAD ORDERS)
 ;
 ; NOTES
 ; =====
 ; Filters on REQUEST STATUS field (#5) of the RAD/NUC MED ORDERS file (#75.1)
 ; ,QUITting if status is DISCONTINUED, UNRELEASED or is null.
 ;
 ; Procedure Modifiers are multiple, concatenated as "Name1|1~Name2|2~...",
 ;   and get re-formatted as "NAME1~NAME2~..."
 ;
 ; Possible errors:
 ;
 ; . S ARRAY(1)="-1,Invalid or missing patient identifier: """_DFN_"""."
 ; . S ARRAY(1)="-2,DFN_" undefined in the RAD/NUC MED PATIENT file (#70).
 ;
GETORRAD(PATDFN,OTYPE) ;
 ;
 K RETURN
 D ORDERS^MAGVIM07(.RETURN,PATDFN)            ;MAGDRPCB(.RETURN,PATDFN)
 ;
 ;--- Process error output & QUIT.
 I $P(RETURN(1),",")<0 D  Q
 . S $P(MAGTMP(0),SEPSTAT,2)=$P(RETURN(1),",",2)
 . K RETURN S RETURN(0)="-1"_MAGTMP(0)
 . K MAGTMP
 . Q
 ;
 ;--- Re-subscript.
 N CTIN S CTIN=0
 F  S CTIN=$O(RETURN(CTIN)) Q:CTIN=""  S RETURN(CTIN-1)=RETURN(CTIN)
 S CTIN=$O(RETURN(""),-1) K RETURN(CTIN)
 ;
 N LINECUR S LINECUR=0
 N LINETOT S LINETOT=RETURN(0)
 N LINEOUT S LINEOUT=0
 ;
 F LINECUR=1:1:LINETOT D
 . ;
 . K MAGV
 . ;
 . ;--- Array "^" pieces & re-write line.
 . S MAGV("RAOIEN")=$P(RETURN(LINECUR),U,1)
 . S MAGV("PROCIEN")=$P(RETURN(LINECUR),U,2)
 . S MAGV("PROCMOD")=$P(RETURN(LINECUR),U,3)
 . ;--- Re-format Procedure Modifiers
 . I $G(MAGV("PROCMOD"))'="" N PCES S PCES="" D  S MAGV("PROCMOD")=PCES
 . . N CT F CT=1:1:$L(MAGV("PROCMOD"),"~") D
 . . . N PCE S PCE=$P($P(MAGV("PROCMOD"),"~",CT),"|",1),$P(PCES,"~",CT)=PCE
 . . . Q
 . . Q
 . . ;
 . ; S MAGV("ORDRSTAT")=$P(RETURN(LINECUR),U,4)
 . S MAGV("ORDERDT")=$$FMTE^XLFDT($P(RETURN(LINECUR),U,5),1)
 . S MAGV("ORDREASN")=$P(RETURN(LINECUR),U,6)
 . S MAGV("EXAMACN")=$P(RETURN(LINECUR),U,9)
 . N SS F SS="EXAMADC","EXAMCASE","EXAMIEN","EXAMIEN2","EXAMSTAT" S MAGV(SS)=""
 . D:MAGV("EXAMACN")'=""
 . . ;
 . . ;--- Resolve DAY-CASE, CASE from (Site-)Access'n Number [###-]MMDDYY-#####
 . . N ACNL S ACNL=$L(MAGV("EXAMACN"),"-")
 . . I ACNL>1 D
 . . . S MAGV("EXAMADC")=$P(MAGV("EXAMACN"),"-",ACNL-1,ACNL)
 . . E  D
 . . . S MAGV("EXAMADC")=MAGV("EXAMACN")
 . . S MAGV("EXAMCASE")=$P(MAGV("EXAMACN"),"-",ACNL)
 . . ;
 . . ;--- Resolve examination IEN from DAY-CASE/ACN.
 . . N ROOT S ROOT=$NA(^RADPT("ADC",MAGV("EXAMADC"),PATDFN)),ROOT=$Q(@ROOT)
 . . Q:$QS(ROOT,1)'="ADC"
 . . S MAGV("EXAMIEN")=$QS(ROOT,4)
 . . S MAGV("EXAMIEN2")=$QS(ROOT,5)
 . . ;
 . . N EXSTP
 . . S EXSTP=$P($G(^RADPT(PATDFN,"DT",MAGV("EXAMIEN"),"P",MAGV("EXAMIEN2"),0)),U,3)
 . . S MAGV("EXAMSTAT")=$P($G(^RA(72,EXSTP,0)),U,1)
 . . Q
 . ;--- IA #10103 (Supported).
 . S MAGV("EXAMDATE")=$$FMTE^XLFDT($P(RETURN(LINECUR),U,10),1)
 . ;--- IA #2056 (Supported).
 . S MAGV("PROCNAME")=$$GET1^DIQ(71,MAGV("PROCIEN"),.01) ; procedure name
 . S MAGV("ORDRLOCN")=$$GET1^DIQ(75.1,MAGV("RAOIEN"),22) D
 . . ;
 . . I MAGV("ORDRLOCN")="" S MAGV("ORDRLOCIEN")="" Q
 . . S MAGV("ORDRLOCIEN")=$$GET1^DIQ(75.1,MAGV("RAOIEN"),22,"I")
 . . Q
 . S LINEOUT=LINEOUT+1
 . ;
 . S MAGV("ORDRPHYS")=$$GET1^DIQ(75.1,MAGV("RAOIEN"),14,"I") ; Requesting Phys.
 . ;
 . ;--- Re-order for output.
 . S $P(RETURN(LINEOUT),SEPOUTP,1)=OTYPE
 . S $P(RETURN(LINEOUT),SEPOUTP,2)=PATDFN
 . S $P(RETURN(LINEOUT),SEPOUTP,3)=MAGV("ORDERDT")
 . S $P(RETURN(LINEOUT),SEPOUTP,4)=MAGV("ORDREASN")
 . S $P(RETURN(LINEOUT),SEPOUTP,5)=MAGV("ORDRLOCN")
 . S $P(RETURN(LINEOUT),SEPOUTP,6)=MAGV("EXAMDATE")
 . S $P(RETURN(LINEOUT),SEPOUTP,7)=MAGV("EXAMACN")
 . S $P(RETURN(LINEOUT),SEPOUTP,8)=MAGV("EXAMSTAT")
 . S $P(RETURN(LINEOUT),SEPOUTP,9)=MAGV("PROCIEN")
 . S $P(RETURN(LINEOUT),SEPOUTP,10)=MAGV("PROCNAME")
 . S $P(RETURN(LINEOUT),SEPOUTP,11)=MAGV("EXAMCASE")
 . S $P(RETURN(LINEOUT),SEPOUTP,12)=MAGV("EXAMIEN")
 . S $P(RETURN(LINEOUT),SEPOUTP,13)=MAGV("RAOIEN")
 . S $P(RETURN(LINEOUT),SEPOUTP,14)=MAGV("EXAMIEN2")
 . S $P(RETURN(LINEOUT),SEPOUTP,15)=MAGV("ORDRPHYS")
 . S $P(RETURN(LINEOUT),SEPOUTP,16)=MAGV("ORDRLOCIEN")
 . S $P(RETURN(LINEOUT),SEPOUTP,17)=MAGV("PROCMOD")
 . Q
 . ;
 S RETURN(0)="0"_SEPSTAT_LINEOUT
 Q
 ;
 ;+++ Routine Utility: Initialize Separators
ZRUSEPIN ;
 S SEPOUTP=$$OUTSEP^MAGVIM01
 S SEPSTAT=$$STATSEP^MAGVIM01
 Q
 ;
 ; MAGVIM02
