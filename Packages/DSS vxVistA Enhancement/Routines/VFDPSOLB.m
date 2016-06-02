VFDPSOLB ;DSS/SGM - SUPPORT FOR MODS FOR PSO LABELS ; 4/3/14 2:33pm
 ;;1.0;DSSDOR - DOCUMENT STORAGE SYS;;17 Mar 2014;Build 2
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ;This routine is invoked from various PSO label print routines.
 ;The mods in those routines check for $G(VFDPSOLB) to determine as to
 ;whether it should come to this routine or continue processing like a
 ;VA VistA routine (OSEHRA Compliance).
 ;
OUT I 1
 Q
 ;
COPAY(SRC) ; remove the display of any copay messages (COPAYVAR)
 N X,Y S SRC=$G(SRC)
 I SRC="LBLN" D  ; called from L12^PSOLBLN2
 . W !,$P(PS,U,2),?54,"Days Supply: "_$G(DAYS),?102
 . W "Tech__________RPh_________",!,$P(PS,U,7)_", "
 . W STATE_" "_$G(PSOHZIP)
 . Q
 I SRC="LLL3" D PRINT("")
 I SRC="" D  ; called from START^PSOLBLN1
 . W !?54,"Days Supply: "_$G(DAYS),?102,"Mfg "_$G(MFG)_" Lot# "_$G(LOT)
 . Q
 G OUT
 ;
DEA ; called from START^PSOLBLN1
 ; change ?54,"*DEA or VA# to ?54,"*DEA#
 W !,?54,"*Print Name:"_ULN_"*",!
 W $S($G(PS55)=2:"***DO NOT MAIL***",1:"***CRITICAL MEDICAL SHIPMENT***")
 W ?54,"*DEA #_________________Date_____________*"
 W ?102,"Routing: "_$S("W"[$E(MW):MW,1:MW_" MAIL")
 W !,?54,"*Refills: 0 1 2 3 4 5 6 7 8 9 10 11",?99,"*"
 W ?102,"Days Supply: "_$G(DAYS)_" Cap: "_$S(PSCAP:"**NON-SFTY**",1:"SAFETY")
 ; remove ,?54,"***** To be filled in VA Pharmacies only *****"
 W !,?102,"Isd: "_ISD_" Exp: "_EXPDT,!,PNM,?54,$G(VAPA(1)),?102,"Last Fill: "_$G(PSOLASTF)
 ; remove Pat. Stat
 W !,$S($D(PSMP(1)):PSMP(1),1:VAPA(1)),?54,$G(ADDR(2)),?102,"Clinic: "_PSCLN
 G OUT
 ;
MAIL(VFDMAIL,SITE,PTECH,FORM) ;
 ; .VFDMAIL - return mailing address
 ;  SITE - req - pointer to file 59
 ; PTECH - opt - Boolean flag to include TECH on address line
 ;   return address formats
 ;  FORM - opt - addess format
 ;  FORM - 1 [default]                           | FORM = 2
 ;---------------------------------------------- | --------------------
 ;<name>                                         | <name>
 ;<street address>    PH: <phone>                | <street address>
 ;<city, state zip>    <pharmacist's initial(s)> | <city, state zip>
 ;                                               | PH: <phone>   pharm inits
 N I,J,X,Y,Z,PF,PH,VFDT K VFDMAIL
 S FORM=$G(FORM) S:'FORM FORM=1
 F I=1:1:4 S VFDMAIL(I)=""
 Q:'$G(SITE)
 K Z F I=0,21600 S Z(I)=$G(^PS(59,+SITE,I))
 ; get site name
 S X=$P(Z(0),U),Y=$P(Z(21600),U,2) S:Y'="" X=Y S VFDMAIL(1)=X
 S Y="",J=$P(Z(0),U,3) S:$L(J) Y=J_"-" S J=$P(Z(0),U,4) S:$L(J) Y=Y_J
 S PH="" I Y'="" S PH="PH: "_Y
 S VFDMAIL(2)=$P(Z(0),U,2) S:FORM=1 VFDMAIL(2)=VFDMAIL(2)_"     "_PH
 S:FORM=2 VFDMAIL(4)=PH
 ; set city, st zip
 S X=$P(Z(0),U,7),Y=$P(Z(0),U,8) S:Y Y=$G(^DIC(5,+Y,0))
 I $L(Y) S J=$P(Y,U,2) S:J="" J=$P(Y,U) S Y=J
 S:$L(X) X=X_", " S X=X_Y ; city, st
 S Y=$P(Z(0),U,5)
 I $L(Y) D ZIPOUT^PSOUTLA S:$L(X) X=X_" " S X=X_Y ; zip
 S VFDMAIL(3)=X,VFDT=""
 I $G(PTECH),$G(TECH)'="" D
 . S (I,J,X)=""
 . I TECH["/" S I=$P($P(TECH,"/"),"(",2),J=$P($P(TECH,"/",2),")")
 .;DSS/AMC - Start mod for Rph initials from partial and refill
 .I $D(RXF) S J=I
 .I $D(RXP),$G(RXPI) S I=$P(^PSRX(RX,"P",RXPI,0),U,7),J=""
 .;DSS/AMC - End of mod
 . I I="" S I=$P($P(TECH,"(",2),")")
 . S Y=$P($G(^VA(200,+I,0)),U,2)
 . I +J S Y=Y_"/"_$P($G(^VA(200,+J,0)),U,2)
 . I $L(Y) S VFDT="("_Y_")"
 . Q
 S I=$S(FORM=1:3,FORM=2:4,1:3)
 I $L(VFDT) S VFDMAIL(I)=VFDMAIL(I)_"    "_VFDT
 G OUT
 ;
PSTAT(SRC) ; remove printing of patient status
 ; patient status (#53) are all veteran specific
 ; usually will remove "Pat. Stat "_PATST prior to clinic
 N X,Y S SRC=$G(SRC)
 I SRC="LBLN" D  ; called from L12^PSOLBLN
 . W !,$S($D(PSMP(1)):PSMP(1),1:$G(VAPA(1)))
 . W ?54,"[ ] Permanent",?102,"Clinic: ",PSCLN
 . Q
 I SRC="LBLN1" D  ; called from NORENW^PSOLBLN1
 . W !,$S($D(PSMP(2)):PSMP(2),$D(PSMP(1)):"",1:$G(ADDR(2))),?54
 . W "*Indicate address change on back of this form"
 . W ?102," Clinic: ",PSCLN
 . Q
 I SRC="LLL2" D  ; called from L12^PSOLLL2
 . S PTEXT="Clinic: "_PSCLN D STRT^PSOLLU1("SIG2",PTEXT,.L)
 . S T=PTEXT D PRINT(T,1)
 . Q
 I SRC="LLL8"!(SRC="LLL9") D  ; called from START^PSOLLL9
 . S X="Clinic: "_PSCLN D PRINT(X)
 . Q
 G OUT
 ;
SVC(SRC) ; replace (119) with "" or Pharmacy
 ; SRC - req - last chars of calling routine
 N X,Y S SRC=$G(SRC)
 I SRC="LBL1" D  ; called from START^PSOLBL1
 . ; exactly same, except VA (119) removed prior to ?10
 . W $C(13) S $X=0 W ?10,$$FMTE^XLFDT(DT,"2Z")
 . W:('SIDE)&(PRTFL) ?40,"PLEASE REFER ONLY TO '",$S(REF:"1. REFILL REQUEST",1:"2. RENEWAL ORDER"),"'"
 . W:+$G(RXP) ?100,"(PARTIAL)" W:$D(REPRINT) ?110,"(REPRINT)"
 . Q
 I SRC="LBLN2" D  ; called from START^PSOLBLN2
 . S ^TMP($J,"PSOMAIL",$S(PRCOPAY:1,1:3))="Pharmacy Service" ; removed (119)
 . S ^($S(PRCOPAY:2,1:4))=$G(VAADDR1)
 . S ^($S(PRCOPAY:3,1:5))=$G(VASTREET)
 . S ^($S(PRCOPAY:4,1:6))=$P(PS,U,7)_", "_$G(STATE)_" "_$G(PSOHZIP)
 . Q
 I SRC="LLL1" D  ; called from WARN^PSOLLL1
 . S T=$E("Attn: Pharmacy"_BLNKLIN,1,40)_$$FMTE^XLFDT(DT) D PRINT(T,1)
 . Q
 I SRC="LLL7" D  ; called from MAIL^PSOLLL7
 . S TEXT="Attn: Pharmacy" D PRINT(TEXT,1)
 . Q
 G OUT
 ;
VAMC(SRC) ; replace VAMC chars with 1-4 char abbrev from 59,21600.01
 ; SRC - req - last chars of calling routine
 N I,J,X,Y S SRC=$G(SRC)
 ; build y = name city, state zip
 S X=$P($G(^PS(59,+$G(PSOSITE),21600)),U),X=$E(X_" ",1,5)
 S Y=X_$P(PS,U,7)_", "_STATE_" "_$G(PSOHZIP)
 I SRC="LBL2" D  ; called from REP^PSOLBL2
 . W Y,?102,"(REPRINT)" W:$G(RXP) "(PARTIAL)"
 . W !,$P(PS2,U,2)_" "_$P(PS,U,3)_"-"_$P(PS,U,4)_" "_TECH
 . Q
 I SRC="LBLN" D  ; called from L1^PSOLBLN
 . W " "_Y,?54,Y,?102
 . W:$D(REPRINT) $S($G(PSOBLALL):"(GROUP REPRINT)",1:"(REPRINT)")
 . W:$G(RXP) "(PARTIAL)"
 . Q
 I SRC="LLL1"!(SRC="LLLH") D
 . ; called from L1^PSOLLL1 or HDR^PSOLLLH
 . I SIGF!$G(FILLCONT) D PRINT(" ",1),PRINT(" ",1) Q
 . N X,VFD S X=(SRC="LLL1")
 . D MAIL(.VFD,PSOSITE,X)
 . F X="PSOFONT","PSOX" S @("VFD(0,X)="_X)
 . S PSOFONT=-999,PSOX=0
 . S VFD(-1)=$C(27)_"(10U"_$C(27)_"(s1p7v0s3b16602T"
 . S VFD(-2)=$C(27)_"(10U"_$C(27)_"(s1p7v0s0b16602T"
 . S VFD(1)=VFD(-1)_VFD(1)_VFD(-2)
 . F I=2,3,4 S VFD(I)=VFD(-2)_VFD(I)
 . F I=1:1:4 D PRINT(VFD(I))
 . F X="PSOFONT","PSOX" S @(X_"=VFD(0,X)")
 . Q
 G OUT
 ;
 ;
 ; Following entry points by DSS/SMH to support printing NDC
 ; and manufacturer from the Outpatient Routines.
 ; Mods made only to PSOLLL1 for laser labels.
 ;
NDC(RX) ; $$ ; Get NDC for a prescription for last fill in File 52
 ; Input:  Rx: Rx IEN in file 52.
 ; Output: NDC in 5-4-2 format (dashes included)
 ;
 QUIT $$GETNDC^PSONDCUT(RX)
 ;
MANUF(RX) ; $$ ; Get Manufacturer for a prescription in File 52
 ; Input:  Rx: Rx IEN in file 52.
 ; Output: Manufacturer Name
 ;
 N NDC S NDC=$$NDC(RX) ; 5-4-2 NDC
 Q $$MANUF1(NDC) ; Pass control to inner call
 ;
MANUF1(NDC) ; [Private] - Inner Manuf call by formatted NDC
 ; Input: NDC in 5-4-2 (HIPPA) format
 ; Output: Manufacturer or ____ if not found.
 ; Unit Testable
 N MNDC S MNDC=$P(NDC,"-",1,2)      ; Get 5-4
 I NDC="" Q "_____________"         ; nothing.
 ;
 ; Read back
 N RESULT
 I $D(^VFD(21675.95,0)) D  ; If FDB Manuf file here...
 . N MIEN S MIEN=$$FIND1^DIC(21675.95,,"QX",MNDC,"B") ; Find in B Ix
 . I MIEN S RESULT=$$GET1^DIQ(21675.95,MIEN,.02) I 1 ; Return short nm
 . E  D  ; no match on 5-4. Try just 5.
 . . S MNDC=$P(MNDC,"-")
 . . N %1,IEN
 . . S %1=$O(^VFD(21675.95,"B",MNDC))
 . . I %1'[MNDC QUIT  ; Fell off the deep end
 . . S IEN=$O(^(%1,""))
 . . S RESULT=$$GET1^DIQ(21675.95,IEN,.02)
 E  D                    ; Otherwise use VA Manuf file
 . S MNDC=$P(MNDC,"-"),MNDC="0"_MNDC               ; Use 6 digit manu
 . N MIEN S MIEN=$$FIND1^DIC(55.95,,"QX",MNDC,"C") ; Find in C index
 . I MIEN S RESULT=$$GET1^DIQ(55.95,MIEN,.01)      ; Manu name field.
 I '$D(RESULT) Q "_____________"         ; nothing.
 Q RESULT
 ;
PTADDR(PSOYORIG) ; DSS/SMH - Print Patient Address on Label
 ;
 ; If SIG overflow or continued fill(?) don't print it.
 ; Already printed on earlier label
 I SIGF!$G(FILLCONT) QUIT
 ;
 ; Move cursor to the top
 S PSOY=PSOYORIG
 ;
 ; Get Pharmacy Address info in VFD
 N VFD
 D MAIL(.VFD,PSOSITE,1)
 ;
 ; Compute the lengths of the lines in Pharm Addr into VFDL
 N VFDL,L
 N I F I=1:1:4 D
 . D STRT^PSOLLU1("SIG",VFD(I),.L)   ; Get Length
 . S VFDL(I)=L(6)+L(8)/2             ; Get size for font 7
 . S VFDL("B",VFDL(I))=""            ; Index lengths
 ;
 ; Find longest
 N MAX S MAX=$O(VFDL("B",""),-1)     ;
 ;
 I MAX<2.1 S MAX=2.1                 ; Justify if small address
 ;
 ; Move Cursor to beyond the longest + simdgen
 S PSOX=MAX*300+5
 ;
 N REM S REM=3.375-MAX                 ; Remaining # of inches
 ;
 S PSOFONT="F6B"
 D PRINTTC(PNM,REM,PSOFONT)            ; Print Patient Name
 S PSOFONT="F6"
 ;
 ; ADD CHECK FOR BAD ADDRESS INDICATOR OR FOREIGN ADDRESS
 N PSOBADR,PSOTEMP,PSOFORGN,I
 S PSOBADR=0,PSOTEMP=0
 S PSOFORGN=$P($G(VAPA(25)),"^",2) I PSOFORGN'="",PSOFORGN'["UNITED STATES" S PSOFORGN=1
 I 'PSOFORGN S PSOBADR=$$BADADR^DGUTL3(DFN)
 I 'PSOFORGN,PSOBADR S PSOTEMP=$$CHKTEMP^PSOBAI(DFN)
 ;
 ; Print Patient Address lines (Addr 1-3)
 F I=1:1:3 I $G(VAPA(I))]"" D
 . S T="" I I=1,'PSOFORGN,PSOBADR,'$G(PSOTEMP) S T="** BAD ADDRESS INDICATED **"
 . I I=1,T="",PSOFORGN S T="*** FOREIGN ADDRESS ***"
 . I T="" I 'PSOFORGN I 'PSOBADR!$G(PSOTEMP) S T=$G(VAPA(I))
 . D PRINTTC(T,REM,PSOFONT)
 ;
 ; Print City State Zip
 S A=+$G(VAPA(5)) I A S A=$S($D(^DIC(5,A,0)):$P(^(0),"^",2),1:"UNKNOWN")
 S T="" I 'PSOFORGN I 'PSOBADR!$G(PSOTEMP) S T=$G(VAPA(4))_", "_A_"  "_$S($G(VAPA(11)):$P(VAPA(11),"^",2),1:$G(VAPA(6)))
 D PRINTTC(T,REM,PSOFONT)
 QUIT
 ;
 ;
TRANWARN(RX) ; [Protected] - Print Federal Controlled Subst Warning
 ; Input: RX - Prescription IEN in 52
 ; DSS/SMH
 N DEA S DEA=$$GET1^DIQ(52,RX,"6:3") ; DRUG:DEA
 S DEA=+DEA ; Remove the suffixes
 I 234'[DEA QUIT  ; Not Schedules 2, 3 or 4
 ;
 ; Text from http://www.deadiversion.usdoj.gov/pubs/manuals/pharm2/pharm_content.htm Section X
 N TEXT S TEXT="CAUTION: Federal law prohibits the transfer of this drug to any person other than the patient for whom it was prescribed."
 N F S F=$C(27)_"(10U"_$C(27)_"(s1p4v0s0b16602T" ; Font
 S PSOY=PSOY-25  ; Move up a little
 D PRINT(F_TEXT) ; and print
 QUIT
 ;
OMH() ; [Only permitted to be called PSOLLL1] Are we on OMH? Certain elements need to bolded/unbolded per NY Law
 N SITE S SITE=$P($$SITE^VASITE(),U,3)
 Q $E(SITE,1,3)=111 ; OMH Station Numbers
 ;
 ;
 ; DSS/AMC - Start mods for partial ndc and mfg
GETRXPI(VFDRX,RXP1) ; [Protected] Get IEN for Partial
 N PARTDT,RXP2
 S PARTDT=$P(RXP1,U),RXP2=$O(^PSRX(VFDRX,"P","B",PARTDT,0))
 Q +RXP2
 ; DSS/AMC - End mod
 ;
 ; DSS/AMC - Start of mod ; Get Partial NDC Number
GETNDC(RX,PART) ; [Protected] Returns the Partial's NDC #
 ; Input:  (r) RX - Rx IEN (#52)
 ;         (o) PART - Partial #
 ; Output:     NDC - Rx NDC #
 N NDC S NDC=$$GET1^DIQ(52.2,PART_","_RX,1)
 Q $$NDCFMT^PSSNDCUT(NDC)
 ; DSS/AMC - End of mods
 ;
 ;----------------------- PRIVATE SUBROUTINES -----------------------
PRINT(T,F) ; Send Text to Printer
 I $G(F)=1 I $G(PSOIO(PSOFONT))]"" X PSOIO(PSOFONT)
 I $G(PSOIO("ST"))]"" X PSOIO("ST")
 W T,!
 I $G(PSOIO("ET"))]"" X PSOIO("ET")
 Q
PRINTTC(T,LEN,PSOFONT) ; [Private to routine]; Print and possibly truncate
 ; T = Text
 ; LEN = Length in inches in which to print it
 ; PSOFONT = Font size. F6,F6B,F8,F9,F10,F11,F12 etc
 N SIZEFONT S SIZEFONT=$P(PSOFONT,"B") ; Remove bold
 S SIZEFONT=$E(SIZEFONT,2,99)   ; Remove the F
 N L D STRT^PSOLLU1("ML",T,.L)  ; Get text lengths
 N DONE S DONE=0
 F  D  Q:DONE  Q:T=""
 . I L(SIZEFONT)<LEN D PRINT(T,1) S DONE=1
 . E  K L S T=$E(T,1,$L(T)-1) D STRT^PSOLLU1("ML",T,.L)
 QUIT
 ;
TEST I $L($T(^XTMUNIT)) D EN^XTMUNIT($T(+0),1) QUIT
T1 ; @Test $$MANUF1 for all drugs
 ;N POP D OPEN^%ZISH("FILE1",$$DEFDIR^%ZISH(),"NDC-MANUF-ANALYSIS.TXT","W")
 ;I POP QUIT
 ;U IO
 N VFDI F VFDI=0:0 S VFDI=$O(^PSDRUG(VFDI)) Q:'VFDI  D
 . I ^(VFDI,0)
 . I +$G(^("I")) QUIT  ; Inactive date present
 . N VAC S VAC=$P(^(0),U,2)
 . I VAC["XA" QUIT
 . N NDC S NDC=$P($G(^(2)),U,4)
 . I 'NDC QUIT
 . S NDC=$$NDCFMT^PSSNDCUT(NDC) ; Hippa NDC format
 . N MNF S MNF=$$MANUF1(NDC)
 . W $P(^PSDRUG(VFDI,0),U),?40,NDC,?60,MNF,!
 . D CHKTF^XTMUNIT(MNF'["____")
 ;D CLOSE^%ZISH()
 QUIT
