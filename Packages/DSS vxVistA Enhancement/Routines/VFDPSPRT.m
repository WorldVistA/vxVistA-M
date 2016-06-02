VFDPSPRT ;DSS/SGM,JG,SMP - PHARMACY MAR LABEL PRINT ; 07/16/2015 15:30
 ;;1.0;DSS,INC VXVISTA OPEN SOURCE;;11 Jun 2013;Build 29
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ; This is a rewrite of inpatient pharmacy labels for specialty
 ; printers for label printing.  As of 11/15/2011 this routine is
 ; specific to Zebra ZPL II programming language.  The original
 ; routines were designed to print to a 1x3.5" label on a dot matrix
 ; printer.  Those print routine also expected the label stock to have
 ; 2 columns of labels.  The output of the labels to these label
 ; printers has been completely reformatted so as to expect only one
 ; column of labels.
 ; 1. Convert all explicit WRITES to a local array then print the local
 ;    array. Format array instead of tabbing to columns.
 ; 2. From the original print routines, the information on the 1st and
 ;    4th lines on the label in the second column on the first row of
 ;    labels was retained.
 ; 3. New label format
 ;    a. The first line of the label now mimics the 1st line on the 1st
 ;       label from the second column.
 ;    b. Special instructions start printing on the 4th line of the 1st
 ;       label
 ;       1) May be multi-lined
 ;       2) There may be 2 or more labels per pharmacy order
 ;          a) IVs tend to be more wordy
 ;    c. If special instructions consists of only a single line, then
 ;       then the activity line will print on the first label
 ;    d. Else, the activity line will print on the second or subsequent
 ;       label
 ;    e. The activity line is the one which contains the status such as
 ;       'Discontinued'
 ; 4. Instead of doing direct WRITEs ad hoc as data is built, this
 ;    program will create a formatted text array. Then a FOR loop will
 ;    print out that array.  The format for each line looks like:
 ;    <43-character string>|<xx-character string>
 ;    a. The routine will build the 43-char strings first + "|"
 ;    b. Then it will append the <8-chars> (this is the admin times)
 ;
 ;Modules cloned from the following routines
 ;---------------------------------------------------------------------
 ;PSGLOI ;BIR/CML3-ORDER INFO (AND PRINT) FOR LABELS ;30 Apr 98 / 4:41 PM
 ;;5.0; INPATIENT MEDICATIONS ;**7,44,58,110,111,138**;16 DEC 97
 ;
 ;PSGLPI ;BIR/CML3-PATIENT INFO FOR LABELS ;15 DEC 95 / 10:26 AM
 ;;5.0; INPATIENT MEDICATIONS ;;16 DEC 97
 ;
 ;PSIVUDL ;BIR/PR,MLM-IV ORDER INFORMATION FOR UNIT DOSE LABEL ;25 Nov 98 / 9:12 AM
 ;;5.0; INPATIENT MEDICATIONS ;**21,58,110**;16 DEC 97
 ;
EN(TAG,LABEL,TEST) ; check for label printer device
 ;   TAG - req - line tag to go to: TEST, PSGLOI, PSGLPI ,PSIVUDL
 ; LABEL - opt - print a test label where LABEL indicates the line tag
 ;               to get the sample text: T1, T2, T3
 ;               If LABEL'="" then TAG = "TEST"
 ;  TEST - opt - default to 0.  Flag indicating testing print to a
 ;               non-specialty label printer like a terminal
 ;               0: print output expected to go to a real label printer
 ;               1: print the text without Zebra format characters
 ;               2: print the text including Zebra format characters
 ;
 ; Align label option 'PSJU AL' (ENAL^PSGLBA) uses PSGLOI test label
 ;
 N X,Y,PARAM,ROU
 S TAG=$G(TAG),LABEL=$G(LABEL),TEST=+$G(TEST)
 ; Check parameter for alternate label routine
 S PARAM=$$GET1^VFDCXPR(,"ALL~VFD PS LABEL PRINT~PSJ",1)
 I +PARAM'=-1 S ROU=PARAM,TAG=TAG_U_ROU
 I TAG=""!($T(@TAG)="") Q 0
 I 'TEST,IOST'?1"P-".E1"VFDLBL" Q 0
 I $P(TAG,U)="TEST" S X=LABEL D  I LABEL="" Q 0
 .I LABEL?1"T"1N,123[$E(LABEL,2) Q
 .S LABEL=$S(X="PSGLOI":"T1",X="PSGLPI":"T2",X="PSIVUDL":"T3",1:"")
 .I LABEL'="",$T(@$S($D(ROU):LABEL_U_ROU,1:LABEL))="" S LABEL=""
 .Q
 D @TAG
 Q 1
 ;=====================================================================
TEST ; test printing label
 ; called from ENLP^PSGLBA
 N I,L,X,Y,Z,VFD
 F I=1:1:7 S VFD(I)=$P($T(@LABEL+I),";",3)
 D WR
 Q
 ;=====================================================================
PSGEUD ; [Only called from PSGEUD] Print labels for extra doses dispensed
 ; ZEXCEPT: VFDPSGDRG ; Drug IEN from PRT^PSGEUD
 ; ZEXCEPT: PSGORD    ; order number
 ; ZEXCEPT: PSGP      ; DFN
 ; ZEXCEPT: VFDPSGEUD ; # dispensed
 ;
 N DFN S DFN=PSGP
 ; Get ward and room-bed plus patient IDs
 N VAIN,VA
 D PID^VADPT,INP^VADPT
 S PSJPWDN=$P(VAIN(4),U,2)
 S PSJPRB=VAIN(5)
 N VFD ; printing array
 S VFD(1)=$$GET1^DIQ(2,DFN,.01)_" (#"_$$PID()_")"
 S VFD(2)=PSJPWDN_" (R/B: "_$S(PSJPRB]"":PSJPRB,1:"n/a")_")" ; ward name/room-bed
 S VFD(3)=""                     ; line 3 is empty
 N DN S DN=$$GET1^DIQ(50,VFDPSGDRG,.01)  ; drug name
 N DU S DU=$$GET1^DIQ(50,VFDPSGDRG,14.5) ; drug unit
 S VFD(4)=DN
 N MR,DOS,SCH,ADM,SCHTYP,SI ; med route, dose, schedule, admin times, sched type (C/P), special instructions
 N N S N=$NA(^PS(55,PSGP,5,PSGORD))
 Q:'$D(@N)
 S MR=$P(@N@(0),U,3),MR=$$EXTERNAL^DILFD(55.06,3,,MR)
 S SCHTYP=$P(@N@(0),U,7),SCHTYP=$$EXTERNAL^DILFD(55.06,7,,SCHTYP)
 S DOS=$P(@N@(.2),U,2)
 S SCH=$P(@N@(2),U,1)
 S ADM=$P(@N@(2),U,5)
 S SI=$P($G(@N@(6)),U)
 I SI]"" S SI=$$ENSET^PSGSICHK(SI) ; Special instructions may not be present; if they are, format them
 I $L(SI)>48 S SI="SEE PROVIDER COMMENTS"
 I SI="",$O(@N@(12,0)) S SI="SEE PROVIDER COMMENTS" ; We may have a multi-line special instructions; can't print all
 S VFD(5)="Give: "_DOS_" "_MR_" "_SCH_" ("_SCHTYP_")"
 S VFD(6)=$G(SI)
 S VFD(7)="Dispense extra quantity of: "_VFDPSGEUD_" "_DU
 D WR
 Q
 ;=====================================================================
PSGLOI ; code copied from ENP^PSGLOI
 ; Notes:
 ;  F = global reference to pharm data file
 ;  L = subscript value of next PSGLAT(l)
 ;  N = subscript value of last VFD(n) set
 ;  Program designed for 8 lines per label
 ;
 N C,I,J,L,N,X,Y,Z,DRUG,LEN,NCP,SP,VDATA,VFD,XX
 S (L,N,DRUG)=0,$P(SP," ",51)=""
 D LN1(PSGLPN) ;                         patient name - pat id
 D LN2(PSGLDOB,PSGLAGE,PSGLWDN,PSGLRB) ; dob (age) - ward - rm-bed
 D ACT(PSGLDT,PSGLR,"U") ;               activity d/t - activity
 ; drug name - schedule type
 S I=0 F  S I=$O(DRUGNAME(I)) Q:'I  D
 .S Y=DRUGNAME(I) S:I=1 $E(Y,43)=PSGLST D SET(Y)
 .Q
 I PSGLSI'="" D LOISI
 S X=$$LOILL
 D BUILD(X,.PSGLAT),WR
 Q
 ;
 ; dosage - med route - schedule
 ;W:L=5 ?52,$S(PSGLWGN]"":$E(PSGLWGN,1,21),1:"NOT FOUND"),?79,$J($S(PSGLWDN]"":$E(PSGLWDN,1,21),1:"NOT FOUND"),21)
 ;W !,?1 S L=L+1
 ;=====================================================================
PSGLPI ; code copied from ENHEDER^PSGLPI
 N I,X,Y,LEN,NF,SP,VFD
 S NF="NOT FOUND",$P(SP," ",51)=""
 ;
 ; patient name
 S VFD(1)=$$CJ^XLFSTR(" *** "_PSGLPN_" ***",50),VFD(2)=""
 ;
 ; patient id dob (age) sex
 S X=$$PID,$E(X,15)=PSGLDOB_" ("_PSGLAGE_")  "
 S VFD(3)=X_$S(PSGLSEX'="":PSGLSEX,1:"____"),VFD(4)=""
 ;
 ; ward/rm-bd
 S X=$S(PSGLWGN]"":PSGLWGN,1:NF),Y=$S(PSGLRB]"":PSGLRB,1:NF)
 S VFD(5)=$E(X_SP,1,48-$L(Y))_"  "_Y
 ;
 ; diagnosis
 S VFD(6)="DX: "_PSGLDX
 S VFD(7)=$TR($P($$HTE^XLFDT(%H,"5Z"),":",1,2),"@"," ")
 D WR
 Q
 ;=====================================================================
PSIVUDL ; code copied from ENP^PSIVUDL
 N A,I,L,N,X,Y,Z,DRUG,LEN,MSG,PSGLRN,PSGLRNDT,SP,VDATA,VFD,XX
 S N=0,$P(SP," ",51)=""
 S PSGLRN=$S(ON["P":$G(^PS(53.1,+ON,4)),1:$G(^PS(55,DFN,"IV",+ON,4)))
 I $G(DFN),$G(ON) N PSGLREN S PSGLREN=+$$LASTREN^PSJLMPRI(DFN,ON)
 S PSGLRNDT=$P(PSGLRN,"^",2),PSGLRN=+PSGLRN
 I PSGLRNDT,$G(PSGLREN),$G(PSGLREN)>PSGLRNDT S PSGLRN=0
 I ON["P",P(2)="",+PSGLRN S X="P E N D I N G"
 E  S X=$S(P(2)]"":$E(P(2),1,5)_$E(P(2),9,14),1:"           ")_" |"_P(3)
 ;
 D LN1(VADM(1)) ;                         1st line of label
 S Y=$$ENDTC^PSGMI(+VADM(3))
 D LN2(Y,VADM(4),$P(PSJLWD,U,2),PSJLRB) ; 2nd line of label
 D ACT(PSJLDT,PSJLR,"I") ;                3rd label - activity
 ;
 ; build temp drug array
 ; starting with 4th label print out messages and warnings
 S MSG=$$IVDRUG I MSG]"" D
 .S X=MSG_$P(P("OPI"),U)
 .S:$L(X)>43 X=$E(X,1,39)_"..."
 .S $P(P("OPI"),U)=X
 .Q
 S X=$P(P("MR"),U,2)_" "_P(9)_" "_P(8)
 I P("OPI")'=""!(P(4)="C") D
 .D SET(X) S X=" " I P(4)="C" D SET(" *CAUTION-CHEMOTHERAPY*")
 .Q:P("OPI")=""  S Z=$P(P("OPI"),U),LEN=$L(Z) F Y=1:1:LEN D
 ..S A=$P(Z," ",Y) I ($L(X)+$L(A))>43 D SET(X) S X=" "
 ..S X=X_A_" "
 ..Q
 .Q
 I $L(X)>1 D SET(X)
 ;
 ; now add the drugs from the temp array
 S I=0 F  S I=$O(DRUG(I)) Q:'I  D SET(DRUG(I))
 ;
 ; build actual label print with | admin times
 S X="",$E(X,25)="RPH:"_PSJRPH,$E(X,34)=" RN:"_PSGLRN
 D BUILD(X,.PSJLAT),WR
 ;G DONE^PSIVUDL ; See ENP+2^PSIVUDL
 Q
 ;
 ;=========================================================
 ;   activity for either PSGLOI or PSIVUDL
ACT(DATE,ST,PS) ;
 ; ST = PSGLR if from PSGLOI; PSJLR if from PSIVUDL
 ; PS = U:unit dose; I:IV - where data came from
 N X,Y,Z
 I PS="I",'DATE Q
 I PS="I",ST="R" S ST="NR"
 I PS="I",ST="RI" S ST="RE"
 I PS="U" S X=" " I DATE S X=X_DATE_" - "
 E  S X=" "_$$ENDTC^PSGMI(DATE)_" - "
 S X=X_$S(ST="NR":"RENEWAL ",ST["N":"NEW ",1:"")_"ORDER "
 S Z="" I ST="AD" S Z="AUTO-DC'ED"
 I ST="ARI" S Z="AUTO-REINSTATED"
 I ST="DE" S Z="DC'ED (EDIT)"
 I ST="E" S Z="EDITED"
 I ST["D" S Z="DISCONTINUED"
 I ST="H0" S Z="OFF OF HOLD"
 I ST="H0" S Z="ON HOLD"
 I ST="NE" S Z="(EDIT)"
 I ST="RI" S Z="REINSTATED"
 I Z'="" S X=X_Z
 I PS="U",PSGLFFD]"",PSGLFFD'>DATE,ST'["D" S X=X_" (EXPIRED)"
 D SET(X)
 Q
 ;=========================================================
 ;   build label print array with admin times appended
BUILD(LAST,LAT,MAX) ;
 ; LAST - last line of label with initials
 ;  MAX - maximum lines per label, default to 8
 ;EXPECTS  VFDATA() and .LAT from
 ;    PSGLAT() for PSGLOI   PSJLAT() from PSIVUDL
 ;RETURNS VFD(n) = data strings fully formatted
 ;
 N I,J,L,X,Y,Z,NEXT
 K VFD
 S NEXT=$E(" See next label for continuation"_SP,1,43)_"| "
 S (J,L)=0,LAST=$G(LAST),MAX=$G(MAX) S:'MAX MAX=8
 ; J = incrementor for VFD(n)   L = incrementor for LAT(n)
 F I=1:1 Q:'$D(VDATA(I))  S X=VDATA(I) D
 .S J=J+1,L=L+1 S VFD(J)=X_$G(LAT(L))
 .I '(I#(MAX-1)),$D(VDATA(I+1)) S J=J+1,VFD(J)=NEXT
 .Q
 ; if any more admin times add them to blank lines
 ; if last label print line not at MAX-1 line, then fill in
 ; initials may be last line AND one more admin time to print
 ; J and L are already incremented for next array check or set
 S J=J+1,L=L+1,Z=0,X=$E(SP,1,43)_"| "
 F  D  Q:Z
 .; check for last line on last label
 .I '(J#MAX),'$D(LAT(L+1)) S VFD(J)=LAST_$G(LAT(L)),Z=1 Q
 .S VFD(J)=X_$G(LAT(L))
 .S J=J+1,L=L+1
 .Q
 Q
 ;=========================================================
 ;   build a temp array of IV Drugs
IVDRUG() ; build temp drug array
 ; returns DRUG(i) and extrinsic function string
 N I,J,X,Y,Z,MSG,NAME
 S (I,X)=0,MSG="" F  S X=$O(DRG("AD",X)) Q:'X  D
 .K NAME D NAME^PSIVUTL(DRG("AD",X),47,.NAME,1)
 .S Y=0 F  S Y=$O(NAME(Y)) Q:'Y  D
 ..S I=I+1,DRUG(I)=" "_NAME(Y)
 ..S Z=$P($G(^PS(52.6,+DRG("AD",+X),0)),U,9) I Z'="" S MSG=MSG_Z_" "
 ..Q
 .Q
 ;
 I $G(DRG("SOL",0)) S (J,X)=0 F  S X=$O(DRG("SOL",X)) Q:'X  D
 .K NAME D NAME^PSIVUTL(DRG("SOL",X),47,.NAME,1)
 .S Y=0 F  S Y=$O(NAME(Y)) Q:'Y  D
 ..S I=I+1,J=J+1,Z="    " I J=1 S Z=" in "
 ..S DRUG(I)=Z_NAME(Y),Z=$P(^PS(52.7,+DRG("SOL",X),0),U,4)
 ..I Z'="" S I=I+1,DRUG(I)="       "_Z
 ..Q
 .Q
 Q MSG
 ;=========================================================
 ;   first line of label; patient name - patient id
LN1(NAME) ;
 ; <PATIENT,NAME>             <PATIENT ID>|
 N X,Y,LEN
 S Y=$$PID,LEN=1+$L(NAME)+$L(Y)
 D SET(NAME_$E(SP,1,43-LEN)_Y)
 Q
 ;=========================================================
 ;   2nd line of label; dob (age) - ward - room/bed
LN2(DOB,AGE,WARD,BED) ;
 ; <DOB (AGE)>      <WARD>      <ROOM-BED>|
 N X,Y,LEN
 S WARD=$G(WARD) S:WARD="" WARD="(No Ward)"
 S BED=$G(BED) S:BED="" BED="*NF*"
 S AGE=$G(AGE),DOB=$G(DOB) S:AGE DOB=DOB_" ("_AGE_")"
 S LEN=43-4-$L(DOB)-$L(BED) ; max len for ward
 S Y=DOB_"  "_$$CJ^XLFSTR(WARD,LEN)_"  "_BED
 D SET(Y)
 Q
 ;=========================================================
 ;   initials - last line on last label
LOILL() ;
 N Z
 S Z=$E("WS",1,PSGLWS*2)
 S:PSGLSM $E(Z,5)=$E("HSM",PSGLSM,3)
 S $E(Z,9)=$E("NF",1,PSGLNF*2)
 S $E(Z,25)="RPH:"_PSGLRPH
 S $E(Z,34)=" RN:"_PSGLRN
 Q $E(Z_SP,1,43)_"| "
 ;=========================================================
 ;   Special instructions
LOISI ;
 N I,X,Y,NCP
 S NCP=$L(PSGLSI," "),X="" F I=1:1:NCP D
 .S X=X_$P(PSGLSI," ",I)_" ",Y=$P(PSGLSI," ",I+1)
 .I ($L(X)+$L(Y))>42 D SET(X) S X=""
 .Q
 I $L(X) D SET(X)
 Q
 ;=========================================================
PID() ; return primary ID
 I $G(VA("MRN"))'="" Q VA("MRN")
 I $G(VA("PID"))'="" Q VA("PID")
 I $G(PSGLSSN)'="" Q PSGLSSN
 I $G(VADM(2))'="" Q $P(VADM(2),U,2)
 Q ""
 ;=========================================================
SET(T) ; Set text into next VDATA(n)
 S N=N+1,VDATA(N)=$E(T_SP,1,43)_"| "
 Q
 ;=========================================================
CASSETTE ; [Public] Print Cassette Label
 Q:'$G(DFN)
 N VAIN,VADM,VAERR D DEM^VADPT,INP^VADPT
 N MRN S MRN=$G(VA("MRN"),VA("PID"))
 N NAME S NAME=VADM(1)
 N LASTNAME S LASTNAME=$P(NAME,",")
 S LASTNAME=$E(LASTNAME,1,19) ; we can only fit 19
 N RESTNAME S RESTNAME=$P(NAME,",",2,99)
 N DOB S DOB=$P(VADM(3),U,2)
 N WARDNAME S WARDNAME=$P(VAIN(3),U,2)
 N RB S RB=VAIN(5)
 I RB="" S RB="<NONE>"
 N A
 N %
 S A(1)="^XA^LL203.3^PW711.2"
 S A(2)="^LH10,0"
 S A(3)="^FO,50^A0B,20"
 S A(4)="^FDMRN: %MRN%^FS",%("%MRN%")=MRN
 S A(5)="^FO30,30^ANV,45^FB615,1,0,C,0"
 S A(6)="^FD%LASTNAME%^FS",%("%LASTNAME%")=LASTNAME
 S A(7)="^FO30,70^ANV,30^FB615,1,0,C,0"
 S A(8)="^FD%RESTNAME%^FS",%("%RESTNAME%")=RESTNAME
 S A(9)="^FO30,100^FB615,1,0,C,0^ANV,10"
 S A(10)="^FDDOB: %DOB%^FS",%("%DOB%")=DOB
 S A(11)="^FO30,120^ANP,30^FB600,1,0,C,0"
 S A(12)="^FDRoom/Bed: %RB%^FS",%("%RB%")=RB
 S A(13)="^FO30,150^AN0,14^FB600,1,0,C,0"
 S A(14)="^FD%WARDNAME%^FS",%("%WARDNAME%")=WARDNAME
 S A(15)="^FO620,120^BXN,5,200"
 S A(16)="^FD%MRN%^FS",%("%MRN%")=MRN
 S A(17)="^XZ"
 N I F I=0:0 S I=$O(A(I)) Q:'I  D
 . N W S W=$$REPLACE^XLFSTR(A(I),.%)
 . W W,!
 Q
 ;=========================================================
WR ; write out VFD()
 ; HSIZ = width of 1 character+inter-character space in dots
 ; VSIZ = height of 1 character+arbitrary inter-line space in dots
 ; LPLB = number of lines per physical label
 ; LCOL = starting character position of line
 ;
 ; If TEST valued, output to the device as if it's a Zebra.
 ; Don't include the Zebra control codes in the output if equal=1
 N A,B,I,J,L,X,Y,Z,FORM,INC,INIT,LL,LP
 ;I VFDTEST=9 G WR3
 I TEST=1 M FORM=VFD G WR1
 S FRO="^XA" ;      open label format for printing
 S FRC="^XZ" ;      close label format for printing
 S LL=8 ;           label length
 S LP="^XZ^PH^XA" ; close label format, eject label, open next label
 S INC=22 ; [22]
 S INIT="^LL203.3^PW711.2^CFD,18,10^LH15,25"
 S FORM(1)="^XA"
 S FORM(2)=INIT
 S X=0,Y=-INC,J=2,(I,L)=0
 F  S I=$O(VFD(I)) Q:'I  D
 .S L=L+1 ; '(L#LL) indicates last line on label
 .S Y=L-1#LL*INC
 .S J=J+1,FORM(J)="^FO"_X_","_Y_"^FD"_VFD(I)_"^FS"
 .I $D(VFD(I+1)),'(L#LL) S J=J+1,FORM(J)=LP
 .Q
 S J=J+1,FORM(J)="^XZ^PH"
 ;
WR1 ;
 N TST S TST=$G(TEST)
 S I=0 F  S I=$O(FORM(I)) Q:'I  W FORM(I) I TST,TST<9 W ! W:'(I#8) !
 Q
 ;
WR3 ; print zebra labels expecting a form
 N I,J,L,X,Y,Z,EJLBL,FORM,FRC,FRO,INC,LL,LP,NAME,TEXT
 S NAME="VFDMAR8.ZPL" ; pharmacy MAR label, 8 lines/label
 S FRO="^XA" ;                        open label
 S FRC="^XZ" ;                        close label
 S FRMFL="^FN"_INC_"^FD"_TEXT_"^FS" ; write data field# inc
 S FRMO="^XFR:"_NAME ;                invoke form
 S LL=8 ;                             label length
 S EJLBL="^XZ^PH^XA" ;                eject label
 S FORM(1)=FRO
 S FORM(2)=FRMO
 S (J,Y)=2,(I,INC)=0 F  S I=$O(VFD(I)) Q:'I  D
 .S TEXT=VFD(I)
 .S L=L+1 ;                 '(L#LL) indicates last line on label
 .S INC=L#LL S:'INC INC=LL ; field number 1-8
 .S J=J+1 S @("FORM(J)=FRMFL")
 .I $D(VFD(I+1)),INC=LL S J=J+1,FORM(J)=EJLBL
 .Q
 S J=J+1,FORM(J)=EJLBL
 D WR1
 Q
 ;
WRF ; Zebra form format for MAR labels
 N F,I,J,X,Y,Z,FORM,INC,INIT,NAME
 S NAME="VFDMAR8.ZPL" ; pharmacy MAR label, 8 lines/label
 S INC=22 ;
 S INIT="^LL203.3^PW711.2^CFD,18,10^LH15,25"
 S FORM(1)="^XA"
 S FORM(2)="^DFR:"_NAME_"^FS"
 S FORM(3)=INIT
 S I=3,X=0,Y=-INC F F=1:1:8 D
 .S I=I+1,Y=Y+INC
 .S FORM(I)="^FO"_X_","_Y_"^FN"_F_"^FS"
 .Q
 S FORM(I+1)="^XZ"
 D WR1
 Q
 ;=========================================================
 ;   TEST LABELS
T1 ; PSGLOI
 ;; <PATIENT,NAME>               <PATIENT ID>| xx   ]
 ;; <DOB (AGE)>       <WARD>       <ROOM-BED>| xx   ]
 ;; <ACTIVITY DATE/TIME> <ACTIVITY>          |      ]
 ;; <DRUG NAME>               <SCHEDULE TYPE>|      ]
 ;; <DOSAGE ORDERED>  <MED ROUTE>  <SCHEDULE>|      ]
 ;; <SPECIAL INSTRUCTIONS>                   |      ]
 ;; <WS/HSM/NF>          <RPH:_____ RN:_____>|      ]
 ;
T2 ; PSGLPI
 ;;             <**** PATIENT NAME ****>            ]
 ;;                                                 ]
 ;; <PATIENT ID> <DOB (AGE)>  <SEX>                 ]
 ;; <WARD NAME>                           <ROOM-BED>]
 ;; <DX: DIAGNOSIS>                                 ]
 ;; <TODAY'S DATE-TIME>                             ]
 ;;                                                 ]
 ;
T3 ; PSIVUDL
 ;; <PATIENT,NAME>               <PATIENT ID>| xx   ]
 ;; <DOB (AGE)>       <WARD>       <ROOM-BED>| xx   ]
 ;; <ACTIVITY DATE/TIME> <ACTIVITY>          |      ]
 ;; <DRUG NAME>               <SCHEDULE TYPE>|      ]
 ;; <DOSAGE ORDERED>  <MED ROUTE>  <SCHEDULE>|      ]
 ;; <SPECIAL INSTRUCTIONS>                   |      ]
 ;; <WS/HSM/NF>          <RPH:_____ RN:_____>|      ]
