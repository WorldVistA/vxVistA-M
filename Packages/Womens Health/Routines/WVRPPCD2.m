WVRPPCD2 ;HCIOFO/FT,JR-REPORT: PROCEDURES STATISTICS; ;7/24/01  14:11
 ;;1.0;WOMEN'S HEALTH;**12**;Sep 30, 1998
 ;;  Original routine created by IHS/ANMC/MWR
 ;;* MICHAEL REMILLARD, DDS * ALASKA NATIVE MEDICAL CENTER *
 ;;  COLLATING CODE CALLED BY WVRPPCD.
 ;
 ; This routine uses the following IAs:
 ; <NONE>
 ;
SORT ;EP
 ;---> SORT AND STORE LOCAL ARRAY IN ^TMP("WV",$J,1,
 K ^TMP("WV",$J),WVRES,^TMP("WVX",$J),^TMP("WVNOHCF",$J)
 I $D(ZTQUEUED) S ZTREQ="@"
 ;---> WVBEGDT1=ONE SECOND BEFORE BEGIN DATE.
 ;---> WVENDDT1=THE LAST SECOND OF END DATE.
 N FE,FI,I,WVDFN,WVHCFN,WVHIEN,WVIEN,WVPCD,WVZSTOP,Y
 S WVBEGDT1=WVBEGDT-.0001,WVENDDT1=WVENDDT+.9999
 S WVDATE=WVBEGDT1,WVZSTOP=0
 F  S WVDATE=$O(^WV(790.1,"D",WVDATE)) Q:'WVDATE!(WVDATE>WVENDDT1)!($G(ZTSTOP)=1)  D
 .S WVIEN=0
 .F  S WVIEN=$O(^WV(790.1,"D",WVDATE,WVIEN)) Q:'WVIEN!($G(ZTSTOP)=1)  D
 ..S WVZSTOP=WVZSTOP+1
 ..;If background task, then every 100 records check if user wants to
 ..;stop the task.
 ..I $D(ZTQUEUED),WVZSTOP#100=0 D STOPCHK^WVUTL10(0) Q:$G(ZTSTOP)=1
 ..S Y=^WV(790.1,WVIEN,0)
 ..;---> QUIT IF THIS PROCEDURE HAS A RESULT OF "ERROR/DISREGARD".
 ..Q:$P(Y,U,5)=8
 ..S WVDFN=$P(Y,U,2),WVPCD=$P(Y,U,4),WVHCF=$P(Y,U,10) ;patient ien, procedure ien, facility ien
 ..I 'WVHCF S ^TMP("WVNOHCF",$J,WVIEN)="" Q  ;no facility
 ..S WVHCFN=$$FACNAME(WVHCF)
 ..I WVHCFN="" S ^TMP("WVNOHCF",$J,WVIEN)="" Q  ;no facility name
 ..S WVAGE=$$WVAGE(WVDFN,$P(Y,U,12),WVAGRG)
 ..;---> QUIT IF PATIENT'S AGE IS UNKNOWN OR OUTSIDE OF AGE RANGE.
 ..Q:'WVAGE
 ..;---> QUIT IF NOT SELECTING FOR ALL PROCEDURES AND IF THIS IS
 ..;---> NOT ONE OF THE SELECTED PROCEDURES.
 ..I '$D(WVARR("ALL")) Q:'$D(WVARR(WVPCD))
 ..;---> Quit if Facility is not one selected by user
 ..I '$D(WVSB("ALL")) Q:'$D(WVSB(WVHCF))
 ..;---> FOR WVRES: 0=NORMAL, 1=ABNORMAL, 2=NO RESULT.
 ..S WVRES=$$NORMAL^WVUTL4($P(Y,U,5))
 ..; Below 5 Lines added to gather Rad Credit for rept
 ..S WVJRC=$P($G(^WV(790.1,WVIEN,0)),U,35) I WVJRC'="" D
 ...I '$D(^TMP("WVX",$J,WVHCFN,WVHCF,WVPCD,WVAGE,WVDFN,WVRES,WVJRC)) D  Q
 ....S ^TMP("WVX",$J,WVHCFN,WVHCF,WVPCD,WVAGE,WVDFN,WVRES,WVJRC)=1
 ...S X=^TMP("WVX",$J,WVHCFN,WVHCF,WVPCD,WVAGE,WVDFN,WVRES,WVJRC)+1
 ...S ^TMP("WVX",$J,WVHCFN,WVHCF,WVPCD,WVAGE,WVDFN,WVRES,WVJRC)=X
 ..I '$D(^TMP("WV",$J,WVHCFN,WVHCF,WVPCD,WVAGE,WVDFN,WVRES)) D  Q
 ...S ^TMP("WV",$J,WVHCFN,WVHCF,WVPCD,WVAGE,WVDFN,WVRES)=1
 ..S X=^TMP("WV",$J,WVHCFN,WVHCF,WVPCD,WVAGE,WVDFN,WVRES)+1
 ..S ^TMP("WV",$J,WVHCFN,WVHCF,WVPCD,WVAGE,WVDFN,WVRES)=X
 ..Q
 .Q
 Q:$G(ZTSTOP)=1  ;user stopped the background task
 ;
TOTALS ;EP
 ;---> N=WVPCD, Q=WVAGE, M=WVDFN, P=WVRES (0,1,2), FI=WVHCF(internal)
 ;---> FE=WVHCF(external)
 N I,M,N,P,Q
 S FE=""
 F  S FE=$O(^TMP("WV",$J,FE)) Q:FE=""  S FI=0 F  S FI=$O(^TMP("WV",$J,FE,FI)) Q:'FI  S N=0 F  S N=$O(^TMP("WV",$J,FE,FI,N)) Q:N=""  D
 .S Q=0
 .F  S Q=$O(^TMP("WV",$J,FE,FI,N,Q)) Q:Q=""  D
 ..F I=0,1,2 S ^TMP("WVRES",$J,FE,FI,N,Q,I,"P")=0 S ^TMP("WVRES",$J,FE,FI,N,Q,I,"T")=0
 ..S M=0,(^TMP("WVRES",$J,FE,FI,N,Q,"P"),^TMP("WVRES",$J,FE,FI,N,Q,"T"),^TMP("WVRES",$J,FE,FI,N,Q,"VT","P"),^TMP("WVRES",$J,FE,FI,N,Q,"VT","T"))=0
 ..F  S M=$O(^TMP("WV",$J,FE,FI,N,Q,M)) Q:M=""  D
 ...S P=-1,^TMP("WVRES",$J,FE,FI,N,Q,"P")=^TMP("WVRES",$J,FE,FI,N,Q,"P")+1
 ...I $$GET1^DIQ(2,M,1901,"I")="Y" S ^TMP("WVRES",$J,FE,FI,N,Q,"VT","PA")=$G(^TMP("WVRES",$J,FE,FI,N,Q,"VT","PA"))+1
 ...F  S P=$O(^TMP("WV",$J,FE,FI,N,Q,M,P)) Q:P=""  D
 ....S ^TMP("WVRES",$J,FE,FI,N,Q,P,"P")=^TMP("WVRES",$J,FE,FI,N,Q,P,"P")+1
 ....S ^TMP("WVRES",$J,FE,FI,N,Q,P,"T")=^TMP("WVRES",$J,FE,FI,N,Q,P,"T")+^TMP("WV",$J,FE,FI,N,Q,M,P)
 ....I $$GET1^DIQ(2,M,1901,"I")="Y" D
 .....S ^TMP("WVRES",$J,FE,FI,N,Q,P,"VT","P")=$G(^TMP("WVRES",$J,FE,FI,N,Q,P,"VT","P"))+1
 .....S ^TMP("WVRES",$J,FE,FI,N,Q,P,"VT","T")=$G(^TMP("WVRES",$J,FE,FI,N,Q,P,"VT","T"))+^TMP("WV",$J,FE,FI,N,Q,M,P)
 ....F WVJRC=0,2 D
 .....I $G(^TMP("WVX",$J,FE,FI,N,Q,M,P,WVJRC))'="" S ^TMP("WVRES",$J,FE,FI,N,Q,"CM","PA",WVJRC)=$G(^TMP("WVRES",$J,FE,FI,N,Q,"CM","PA",WVJRC))+1 D
 ......S ^TMP("WVRES",$J,FE,FI,N,Q,P,"CM","P",WVJRC)=$G(^TMP("WVRES",$J,FE,FI,N,Q,P,"CM","P",WVJRC))+1
 ......S ^TMP("WVRES",$J,FE,FI,N,Q,P,"CM","T",WVJRC)=$G(^TMP("WVRES",$J,FE,FI,N,Q,P,"CM","T",WVJRC))+^TMP("WVX",$J,FE,FI,N,Q,M,P,WVJRC)
 ;---> NOW COMPUTE TOTAL #PROCEDURES FOR EACH PROCEDURE, EACH AGE GROUP.
 S FE=""
 F  S FE=$O(^TMP("WVRES",$J,FE)) Q:FE=""  S FI=0 F  S FI=$O(^TMP("WVRES",$J,FE,FI)) Q:'FI  S N=0 F  S N=$O(^TMP("WVRES",$J,FE,FI,N)) Q:'N  D
 .S Q=0 F  S Q=$O(^TMP("WVRES",$J,FE,FI,N,Q)) Q:'Q  D
 ..S M=-1 F  S M=$O(^TMP("WVRES",$J,FE,FI,N,Q,M)) Q:M=""!(M'?1N.N)  D
 ...S ^TMP("WVRES",$J,FE,FI,N,Q,"T")=^TMP("WVRES",$J,FE,FI,N,Q,"T")+^TMP("WVRES",$J,FE,FI,N,Q,M,"T")
 ...;S ^TMP("WVRES",$J,FE,FI,N,Q,"P")=^TMP("WVRES",$J,FE,FI,N,Q,"P")+^TMP("WVRES",$J,FE,FI,N,Q,M,"P")
 ...S ^TMP("WVRES",$J,FE,FI,N,Q,"VT","T")=$G(^TMP("WVRES",$J,FE,FI,N,Q,"VT","T"))+$G(^TMP("WVRES",$J,FE,FI,N,Q,M,"VT","T"))
 ...S ^TMP("WVRES",$J,FE,FI,N,Q,"VT","P")=$G(^TMP("WVRES",$J,FE,FI,N,Q,"VT","P"))+$G(^TMP("WVRES",$J,FE,FI,N,Q,M,"VT","P"))
 ...;*******************************************
 ...F WVJRC=0,2 D
 ....S ^TMP("WVRES",$J,FE,FI,N,Q,"CM","T",WVJRC)=$G(^TMP("WVRES",$J,FE,FI,N,Q,"CM","T",WVJRC))+$G(^TMP("WVRES",$J,FE,FI,N,Q,M,"CM","T",WVJRC))
 ....S ^TMP("WVRES",$J,FE,FI,N,Q,"CM","P",WVJRC)=$G(^TMP("WVRES",$J,FE,FI,N,Q,"CM","P",WVJRC))+$G(^TMP("WVRES",$J,FE,FI,N,Q,M,"CM","P",WVJRC))
 ...;*******************************************
 ;
 ;
 ;-> NOW COMPUTE TOTAL #PROCEDURES FOR EACH PROCEDURE.
 ;-> ^TMP("WVRES",$J,FE,FI,N,"P")=TOTAL PATIENTS WHO RECEIVED THIS PROCEDURE
 ;-> ^TMP("WVRES",$J,FE,FI,N,"T")=TOTAL TIMES THIS PROCEDURE WAS PERFORMED
 S FE=""
 F  S FE=$O(^TMP("WVRES",$J,FE)) Q:FE=""  S FI=0 F  S FI=$O(^TMP("WVRES",$J,FE,FI)) Q:'FI  S N=0 F  S N=$O(^TMP("WVRES",$J,FE,FI,N)) Q:'N  D
 .S Q=0,^TMP("WVRES",$J,FE,FI,N,"P")=0,^TMP("WVRES",$J,FE,FI,N,"T")=0,^TMP("WVRES",$J,FE,FI,N,"VT","P")=0
 .S ^TMP("WVRES",$J,FE,FI,N,"VT","T")=0,^TMP("WVRES",$J,FE,FI,N,"VT","PA")=0
 .F WVJRC=0,2 S ^TMP("WVRES",$J,FE,FI,N,"CM","T",WVJRC)=0,^TMP("WVRES",$J,FE,FI,N,"CM","PA",WVJRC)=0
 .F  S Q=$O(^TMP("WVRES",$J,FE,FI,N,Q)) Q:'Q  D
 ..S ^TMP("WVRES",$J,FE,FI,N,"P")=^TMP("WVRES",$J,FE,FI,N,"P")+^TMP("WVRES",$J,FE,FI,N,Q,"P")
 ..S ^TMP("WVRES",$J,FE,FI,N,"T")=^TMP("WVRES",$J,FE,FI,N,"T")+^TMP("WVRES",$J,FE,FI,N,Q,"T")
 ..S ^TMP("WVRES",$J,FE,FI,N,"VT","P")=^TMP("WVRES",$J,FE,FI,N,"VT","P")+$G(^TMP("WVRES",$J,FE,FI,N,Q,"VT","P"))
 ..S ^TMP("WVRES",$J,FE,FI,N,"VT","T")=^TMP("WVRES",$J,FE,FI,N,"VT","T")+$G(^TMP("WVRES",$J,FE,FI,N,Q,"VT","T"))
 ..S ^TMP("WVRES",$J,FE,FI,N,"VT","PA")=^TMP("WVRES",$J,FE,FI,N,"VT","PA")+$G(^TMP("WVRES",$J,FE,FI,N,Q,"VT","PA"))
 ..F WVJRC=0,2 D
 ...S ^TMP("WVRES",$J,FE,FI,N,"CM","P",WVJRC)=$G(^TMP("WVRES",$J,FE,FI,N,"CM","P",WVJRC))+$G(^TMP("WVRES",$J,FE,FI,N,Q,"CM","P",WVJRC))
 ...S ^TMP("WVRES",$J,FE,FI,N,"CM","T",WVJRC)=$G(^TMP("WVRES",$J,FE,FI,N,"CM","T",WVJRC))+$G(^TMP("WVRES",$J,FE,FI,N,Q,"CM","T",WVJRC))
 ...S ^TMP("WVRES",$J,FE,FI,N,"CM","PA",WVJRC)=$G(^TMP("WVRES",$J,FE,FI,N,"CM","PA",WVJRC))+$G(^TMP("WVRES",$J,FE,FI,N,Q,"CM","PA",WVJRC))
 ;
 D FLATFL^WVRPPCD3
 Q
 ;
WVAGE(DFN,DATE,X) ;EP
 ;---> SET AGE CATEGORY.
 ;---> REQUIRED VARIABLES: DATE=DATE PATIENT RECEIVED THIS PROCEDURE.
 ;--->                     DFN, X=WVAGRG (AGE RANGE).
 ;---> IF NOT DISPLAY BY AGE, SET ALL WVAGE=1
 Q:X=1 1
 N AGE,Y,Z
 S AGE=$P($$AGEAT^WVUTL1(DFN,DATE),"y/o")
 ;---> RETURN 0 IF PATIENT'S AGE IS UNKNOWN.
 Q:'+AGE 0
 ;
 F I=1:1:$L(X,",") S Y=$P($P(X,",",I),"-",2) Q:AGE'>Y
 S Z=$P($P(X,",",I),"-")
 ;---> RETURN 0 IF PATIENT IS OUTSIDE DATE RANGE.
 Q:(AGE<Z!(AGE>Y)) 0
 Q Y
FACNAME(IEN) ; Return Facility name
 ; Check if ien has been looked up already.
 I $G(WVHIEN(IEN))]"" Q $G(WVHIEN(IEN))
 N NAME
 S NAME=$$INSTTX^WVUTL6(IEN) ;get facility name
 I NAME="" Q ""
 S WVHIEN(IEN)=NAME ;update local array with name and ien
 Q NAME
 ;