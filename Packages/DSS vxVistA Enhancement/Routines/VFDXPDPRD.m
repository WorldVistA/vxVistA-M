VFDXPDPRD ; DSS/SMP - DELETE ROUTINES for a Batch ; 08/13/2014 08:47
 ;;3.1;DSS,INC VXVISTA SUPPORTED;;13 August 2013;Build 127
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;PRINTS A TEXT REPORT LISTING THE DELETE ROUTINES ASSOCIATED WITH A BATCH
 ;
 ;Declare the variables
 N I,J,X,Y,Z,BATCH,PID,DEL,RTN,SP,BUILDGROUP,DELRPT,R
 ;Ask the user for the VFD PATCH GROUP NAME and Goto START
 D INIT(6) I PID>0,$G(BATCH) D START
 ;Call the VFDXPD0 print routine passing it the DELRPT array
 D PRINT
 ;Q
 ;
START ; Find the Routines to be deleted for the batch and build report array called DELRPT
 ;      using R as the index DELRPT(R)
 Q:$G(PID)<1
 S R=2
 F Z=1:1:BATCH S I=+BATCH(Z) D
 .S J=0 F  S J=$O(^VFDV(21692,I,4,J)) Q:'J  S Y=^(J,0),RTN(Y)=""
 .Q
 W ! F I=1:1:1 W !,$TR($T(1+I),";"," ")
 S X=0 F  S X=$O(RTN(X)) S DELRPT(R)=X S R=R+1 Q:X=""
 Q
 ;
PRINT ; Prints the Routines to Delete
 M DELRPT=Y
 S BUILDGROUP=$P(^VFDV(21692.1,PID,0),"^",1)
 S DELRPT(.1)="Routines To Delete for Batch "_BUILDGROUP
 I DELRPT(2)="" S DELRPT(.1)="Batch "_BUILDGROUP_" has no Routines to Delete"  
 D TRANS(.DELRPT,U,$C(9)),RPT^VFDXPD0(.DELRPT,,"Batch "_BUILDGROUP_" has no Routines to Delete")
 Q
 ;
 ;RPT^VFDXPD0(.DELRPT,.OUT,TXT) ; Call the report writer routine with parameters
 ; choose between browser, terminal, or hfs
 ; .RPT - req - RPT(n)=text
 ; .OUT - opt - 1:browser; 2:terminal; 3:hfs
 ;              both an input and output param
 ;  TXT - opt - text to display if there is no data in RPT()
 ;
BATCH(BATCH,PID,TYPE) ;
 ;Extrinsic function - return total# kids builds or -1^message 
 ;   PID - req - ien to file 21692.1
 ;  TYPE - opt - Boolean, default to 0
 ;.BATCH - Return BATCH = total number of builds in processing group
 ;                        If problems BATCH = -1^message
 ;                BATCH(n)=val for n=1,2,3,4,...
 ;                BATCH("B",buildname)=n
 ; I 'TYPE val = <21692_ien>^build name^seq#^description
 ; I TYPE val = <21692_ien>^build name^pkg^ver^patch#^seq#
 ; NOTE: pkg,ver,patch#, or seq# may be equal to space " "
 ;       BATCH(1,2,3,...,n) will already be sorted by pkg,ver,seq,patch
 N I,J,N,W,X,Y,Z,DESC,TMP
 I $G(PID)<1 Q $$ERR(5,1)
 S TYPE=$G(TYPE),SORT=$G(SORT)
 S I=0 F  S I=$O(^VFDV(21692.1,PID,1,I)) Q:'I  D
 .W "I = ",I
 .S Z=$G(^VFDV(21692,I,0)) Q:Z=""  S DESC=$G(^("DESC"))
 .K Y F J=1,5,6,7,9 S Y(J)=$P(Z,U,J) I Y(J)="" S Y(J)=" "
 .S W=$NA(TMP(Y(5),Y(6),Y(7),Y(9),I))
 .S @W=Y(1)_U_$S($P(DESC,U)'="":$P(DESC,U),1:$P(DESC,U,2))
 .Q
 S N=0,Z="TMP" F  S Z=$Q(@Z) Q:Z=""  S X=@Z D
 .N IEN,NM,PATCH,PKG,SEQ,VER,VFD
 .S PKG=$QS(Z,1),VER=$QS(Z,2),SEQ=$QS(Z,3),PATCH=$QS(Z,4),IEN=$QS(Z,5)
 .S NM=$P(X,U)
 .I " "[PKG!(" "[VER)!(" "[PATCH) D
 ..D PARSE^VFDVXPD0(NM,.VFD)
 ..I " "[PKG,VFD(1)'="" S PKG=VFD(1)
 ..I " "[VER,VFD(2) S VER=VFD(2)
 ..I " "[PATCH,VFD(3) S PATCH=VFD(3)
 ..Q
 .I 'TYPE S W=IEN_U_NM_U_SEQ_U_$P(X,U,2)
 .E  S W=IEN_U_NM_U_PKG_U_VER_U_PATCH_U_SEQ
 .S N=N+1,BATCH(N)=W
 .S BATCH("B",NM)=N
 .Q
 I N S BATCH=N
 E  S BATCH=$$ERR(6)
 Q BATCH
 ;
TRANS(A,BEFORE,AFTER) ;
 N I S I=""
 F  S I=$O(A(I)) Q:I=""  S A(I)=$TR(A(I),BEFORE,AFTER)
 Q
 ;
INIT(OPT) ;
 ; set the variables PID,PATH if appropriate
 N I,J,X,Y,Z
 D KILL
 I "178AH"[OPT S PATH=""
 I 58'[OPT S X=("124A"[OPT),PID=$$PID^VFDVXPD0(X,"I '$P(^(0),U,3)") Q:PID<1
 I "178AH"[OPT S PATH=$$PATH^VFDVXPD0 S:PATH=-1 PATH="" Q:PATH=""
 I "3679P"[OPT S Y=("9P"[OPT) D
 .S X=$$BATCH^VFDVXPDB(.BATCH,PID,Y)
 ;.S BATCHNAME = X
 .I X<1 W !!,$P(X,U,2) K BATCH
 .Q
 Q
 ;
KILL K ^TMP($J),^TMP("VFDV",$J),^TMP("VFDVRPT",$J),^UTILITY($J) Q
 ;
ERR(A,WR) ;
 N T S A=$G(A)
 I A=1 S T="No path or directory received"
 I A=2 S T="No files found"
 I A=3 S T="Filename has a DAT file plus other files"
 I A=4 S T="Filename has multiple files with the same extension: "
 I A=5 S T="No processing group received"
 I A=6 S T="No entries found for the processing group"
 I '$G(WR) Q "-1^"_T
 W !?3,T
 Q
