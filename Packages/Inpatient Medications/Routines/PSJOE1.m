PSJOE1 ;BIR/CML3-UD OE FOR COMBINED OE ; 12/1/10 1:05pm
 ;;5.0; INPATIENT MEDICATIONS ;**2,7,25,30,47,56,64,179**;16 DEC 97;Build 45
 ;
 ; Reference to ^DICN is supported by DBIA# 10009
 ; Reference to ^VALM is supported by DBIA# 10118
 ;
 S PC=0 G AD
 ;
EN ;
 S PC=0
 ;
AD ; Ask Drug
 N PSJNORD,PSGORQF,PSGSDX,PSGFDX,PSGNEFDO S PSJNORD=1 I $D(VALM("TM")) S IOTM=VALM("TM"),IOBM=IOSL W IOSC,@IOSTBM,IORC
 D ^PSGOE7
 I PSGORQF>0 S PSJORQF=1 G DONE
 S PC=1,PSJORQF=0 I X?1"S."1.E D ^PSGOES G AD
 D ^PSGOE4:'$P(PSJSYSP0,"^",12),^PSGOE3:$P(PSJSYSP0,"^",12)
 G:$G(PSGOROE1)=1 AD
 K PSGEFN,PSGOEEF,PSGOEE,PSGOEOS S PSGEFN="1:13" F X=1:1:13 S PSGEFN(X)=""
 S PSGPDN=$$OINAME^PSJLMUTL(PSGPDRG),PSGPD=PSGPDRG,PSGOINST="",PSGSDN=$$ENDD^PSGMI(PSGNESD)_U_$$ENDTC^PSGMI(PSGNESD),PSGFDN=$$ENDD^PSGMI(PSGNEFD)_U_$$ENDTC^PSGMI(PSGNEFD)
 S PSGAT=PSGS0Y,PSGLIN=$$ENDD^PSGMI(PSGDT)_U_$$ENDTC^PSGMI(PSGDT),PSGLI=PSGDT,PSGEBN=$$ENNPN^PSGMI(DUZ),PSGSTAT=$S(PSGOEAV:"ACTIVE",1:"NON-VERIFIED")
 D CHK^PSGOEV("^^"_PSGMR_"^^^^"_PSGST,PSGPDRG_U_PSGDO,PSGSCH_U_PSGNESD_"^^"_PSGNEFD)
 S PSGSD=PSGNESD,PSGFD=PSGNEFD
 K PSJACEPT S VALMBCK="Q" D:$D(Y) EN^VALM("PSJU LM ACCEPT")
 S PSJNOO=-1 I $G(PSJACEPT)=1 S PSJNOO=$$ENNOO^PSJUTL5("N")
 I $G(PSJNOO)<0 K PSJACEPT W !,"No order created." G AD
 K PSGOEE D ^PSGOETO S PSJORD=PSGORD I PSGOEAV D  G AD
 .I '$D(PSGOEE),+PSJSYSU=3 D EN^PSGPEN(PSGORD)
 S PSGOEEF=0 D GETUD^PSJLMGUD(PSGP,PSGORD),ENSFE^PSGOEE0(PSGP,PSGORD),^PSGOE1,EN^VALM("PSJ LM UD ACTION")
 G AD
 Q
 ;
EDIT(PROMPT) ;
 ; Edit fields in a UD order.
 ; PROMPT=0 - Select fields to edit by number.
 ; PROMPT=1 - Prompt to select fields for editing.
 ;
 ;* D @$S('PROMPT:"ENEFA2^PSGON",1:"ENEFA^PSGON") Q:'Y  S PSGOEEG=3 D EDIT^PSGOEE ;$S(PSGOEEWF[53.1:3,1:5) D:Y EDIT^PSGOEE
 D @$S('PROMPT:"ENEFA2^PSGON",1:"ENEFA^PSGON") Q:'Y  S:$G(PSJNEWOE) PSGOEEWF="^PS(53.1," S PSGOEEG=$S('$D(PSGOEEWF):3,PSGOEEWF[53.1:3,1:5) D EDIT^PSGOEE
 I $G(PSJNEWOE) S PSGOEENO=0,DR="",VALMBCK="R"
 I '$G(PSJNEWOE) D ENNOU^PSGOEE0 I 'PSGOEENO,DR="" S VALMBCK="R" Q
 I 'PSGOEENO,$D(PSGOES) D ENNOU^PSGOEE0  ; only update on order sets
 ;*179 No longer call CKDT^PSGOEE from here.
 ;I 'PSGOEENO,$G(PSGPDNX)=1 D CKDT^PSGOEE
 K VALMSG I PSGOEENO D
 .S VALMSG="This change will cause a new order to be created." D GTSTATUS^PSGOEE,CHKDD^PSGOEE
 .S PSGEBN=$$ENNPN^PSGMI(DUZ),PSGLIN=$$ENDD^PSGMI(PSGDT)_U_$$ENDTC^PSGMI(PSGDT),PSGLI=PSGDT
 D CHK^PSGOEV("^^"_PSGMR_"^^^^"_PSGST,PSGPDRG_U_PSGDO,PSGSCH_U_PSGSD_"^^"_PSGFD)
 D INIT^PSJLMUDE(PSGP,$G(PSGORD))
 Q
DONE ;
 K %,DA,DIC,DIE,DR,DRG,DRGN,DRGO,ND,OC,ORIFN,ORIT,ORPK,ORSTOP,ORSTRT,ORSTS,ORTX,PC,PSGDO,PSGMR,PSGMRN,PSGNEDFD,PSGNEFD,PSGNESD,PSGOES,PSGOROE1,PSGORD,PSGS0XT,PSGS0Y,PSGSCH,PSGSI,PSGX,Y,Z Q
 ;
GDO ;
 W !!,"Drug is not found in Formulary List." F  S %=1 W !,"Would you like to try to search the list again" D YN^DICN Q:%  D TAM
 Q:%<2
FTD ;
 R !!,"Enter FREE TEXT DRUG: ",PSGDRGN:DTIME E  W $C(7) S PSGDRGN="^" Q
 Q:"^"[PSGDRGN  S X=$S(PSGDRGN'?.ANP:"Control character(s)",PSGDRGN["^":"Up-arrow ('^') in text",$L(PSGDRGN)>39:"Reponse longer than 39 characters",1:"") I X]"" W $C(7),"  ??",!?2,"(",X," not allowed.)" G FTD
 Q:PSGDRGN'?1."?"
 W !!?2,"ENTER DRUG ORDERED (1-39 CHARACTERS).",!?2,"Since the drug cannot be found in the DRUG file, enter the drug name here",!,"exactly as ordered.  Press the RETURN key (or enter an '^') to skip over this",!,"drug, or to again search the"
 W " DRUG file for this one." G FTD
 ;
TAM ; Try Again Message
 W !!,"  Enter a 'Y' to try again to find the drug ordered from the Formulary.  (The",!,"order cannot become active until a Formulary drug has been entered.)  Enter 'N'",!,"to enter the drug ordered as free text for later reference."
 W "  Enter '^' to exit.",! Q
