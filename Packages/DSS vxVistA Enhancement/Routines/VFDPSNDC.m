VFDPSNDC ;DSS/SMH - NDC Utilities for vxVISTA ; 10/26/14 10:03pm
 ;;2013.1;DSS - OPEN SOURCE MODIFICATIONS;;9/29/2014;Build 18
 ;
 ;
 ; --- PEPs for capture for NDC (IP and OP) ---
 ;
LOOPNDC(DRG,NDC,DISPLAY) ; [Public Procedure] Keep asking keep asking keep asking keep
 ; Input: DRG Drug IEN
 ;        .NDC Default NDC if already known
 ;        DISPLAY: Should I show the drug name?
 ; Output: .NDC: NDC in 5-4-2 format
 ;         .NDC("SCAN"): User Input that resolves to an NDC
 ;         or
 ;         .NDC = ^, cancel operation.
 ;
 N RETURN ; Really just an "intermediate" return
 N ERR ; We do not return the error to the caller (use below call if you want it)
 F  D  Q:$E($G(ERR))=U  Q:$G(RETURN)
 . K ERR
 . W !
 . I $G(DISPLAY) WRITE "For "_$P(^PSDRUG(DRG,0),U)
 . S RETURN=$$ASKNDC(DRG,$G(NDC),.ERR)
 . I $D(ERR)#2,ERR'=U W !,ERR
 . I $D(ERR)#2,ERR=U W "   ...cancelled"
 I $G(RETURN) S NDC=$P(RETURN,U),NDC("SCAN")=$P(RETURN,U,2)
 E  S NDC=U
 I NDC W "   "_NDC
 QUIT
 ;
ASKNDC(DRG,DEFAULT,ERR) ; [$$ Public] Ask for a simple NDC and then call LOOKNDC
 ; Returns NDC^USER INPUT; empty if there is an error
 ; DRG is required (DRUG IEN)
 ; DEFAULT is optional default NDC. Used to present DIR("B").
 ; .ERR (optional)
 ; .ERR is returned if there there is an error. Check that if NDC=""
 ; .ERR can be a message or ^ or ^^ from ^DIR.
 ; Caller should decide how to handle ^ or ^^.
 N NDC S NDC=""
 N DIR,X,Y,DIRUT,DUOUT,DIROUT,DA
 N DIR S DIR(0)="F",DIR("A")="Scan barcode or Type NDC"
 S DIR("?")="^D NDCHLP^VFDPSNDC"
 I +$G(DRG) S DIR("??")="^D NDCHLP2^VFDPSNDC"
 I +$G(DEFAULT) S DIR("B")=DEFAULT
 D ^DIR
 S:$D(DIRUT) ERR=U
 S:$D(DIROUT) ERR="^^"
 I $E($G(ERR))=U QUIT ""
 I Y?1.A S ERR="Entries containing letters are not accepted." QUIT ""
 I X["-" S X=$TR(X,"-")
 S NDC=$$LOOKNDC(X,$G(DRG),.ERR)
 IF $G(ERR)="" Q NDC_U_X
 ELSE  QUIT ""
 ;
LOOKNDC(SCAN,EXPECTEDDRUG,ERR) ; [$$ Public] Simple NDC lookup; also daisy chained to a $$ASKNDC
 ; RETURN: NDC. Caller knows the scan code
 ; Scan -> Barcode
 ; (Optional) Expected Drug -> Does the NDC match the expected drug?
 ; (Optional) .ERR -> Error
 S SCAN=$TR(SCAN,"-")
 N NDC S NDC="" ; return
 N DRG S DRG=$O(^PSDRUG("C",SCAN,""))  ; Any match
 I DRG D SYNOBT I 1
 E  S ERR="No match found."
 QUIT NDC
 ;
SYNOBT ; [Internal] Process synonym scan lookup
 ; ZEXCEPT: DRG,ERR,NDC,SCAN,EXPECTEDDRUG
 I $G(EXPECTEDDRUG),DRG'=EXPECTEDDRUG S ERR="Scan code matches "_$P(^PSDRUG(DRG,0),U)_" but the current drug is "_$P(^PSDRUG(EXPECTEDDRUG,0),U)_"." QUIT
 N SYNIEN S SYNIEN=$O(^PSDRUG("C",SCAN,DRG,""))
 N SYN0 S SYN0=$G(^PSDRUG(DRG,1,SYNIEN,0))
 I SYN0="" S ERR="C Index corruption." QUIT
 N LINKEDNDC S LINKEDNDC=$P(SYN0,U,2)   ; piece 2 (Associated NDC)
 I LINKEDNDC D  ; Stack $T
 . S NDC=$$NDCFMT^PSSNDCUT(LINKEDNDC)  ; Try that.
 . I NDC="" S ERR="Found match for scan, but attached NDC is incorrect."
 E  S ERR="Found match for scan, but attached NDC is absent." QUIT
 QUIT  ; This is there as a place holder just in case
 ;
 ; -------------------------------------------------------------
 ;
 ; --- HELP EPS ---
 ;
NDCHLP ; [Internal] Help for simple NDC selection
 W !,"Enter an NDC (11 digits without dashes) or a barcode."
 QUIT
 ;
NDCHLP2 ; [Internal] Help Text for ?? for the NDC Code Selection
 ; ZEXCEPT: DRG,X
 W !?3,"Drug file data for synonyms for troubleshooting:",!!
 ;
 W ?3,"The user input must be in the Synonym column.",!!
 W ?3,"For an NDC to be valid, the synonym below must have a valid NDC ",!
 W ?3,"attached to it that must meet NCPDP transmission format of 5-4-2. ",!
 W !
 W ?3,"Synonym",?30,"Associated NDC",?55,"Valid NDC?",!
 N % S $P(%,"-",64)="-" W %,! K %
 N SYN,Z S SYN=0 F  S SYN=$O(^PSDRUG(DRG,1,SYN)) Q:'SYN  S Z=^(SYN,0) D
 . N SYNVAL S SYNVAL=$P(Z,U,1)
 . N SYNNDC S SYNNDC=$P(Z,U,2)
 . N VALNDC S VALNDC=$$NDCFMT^PSSNDCUT(SYNNDC)
 . W ?3,SYNVAL
 . W ?30,$S(SYNNDC="":"<Empty>",1:SYNNDC)
 . W ?55,$S(VALNDC:"Yes",1:"No")
 . W !
 Q
 ;
 ; -------------------------------------------------
 ;
EDITRX ; [Public] Supervisor edit of NDCs; OPTION: VFD PSO SUP NDC EDIT
 ;
 ; I feel I am a student in a class writing this. Boring and using well defined constructs.
 ;
 N X,Y,DTOUT,DUOUT,DINUM,DLAYGO,DIC  ; ^DIC inputs and outputs
 S DIC(0)="AEMQV",DIC=52
 S DIC("S")="I $P($G(^PSRX(+Y,""STA"")),U)'=13"  ; Keep deleted prescriptions out
 S DIC("W")="D DICW^VFDPSNDC"  ; Custom identifiers
 D ^DIC
 ;
 Q:(Y<1)
 ;
 N RXIEN S RXIEN=+Y
 ;
 N DRG S DRG=$P(^PSRX(RXIEN,0),U,6)
 I 'DRG W "Invalid Rx",! QUIT
 ;
 N ORIGNDC S ORIGNDC=$$GET1^DIQ(52,RXIEN,"NDC")
 ;
 N HASRF,HASPR ; Has refills, has partials
 S HASRF=''$O(^PSRX(RXIEN,1,0))
 S HASPR=''$O(^PSRX(RXIEN,"P",0))
 ;
 N DIR,DIROUT,DIRUT,DTOUT,DUOUT,DA,X,Y
 S DIR(0)="S^M:Original Rx NDC "_ORIGNDC
 I HASRF S DIR(0)=DIR(0)_";R:Refill NDC"
 I HASPR S DIR(0)=DIR(0)_";P:Partial NDC"
 S DIR("A")="What would you like to edit? "
 D ^DIR
 ;
 Q:$D(DIRUT)
 ;
 I Y="M" D  QUIT
 . N Y ; my precious!
 . D LOOPNDC(DRG,.ORIGNDC)
 . I ORIGNDC=U QUIT
 . N FDA
 . S FDA(52,RXIEN_",",27)=ORIGNDC
 . S FDA(52,RXIEN_",",21600.02)=ORIGNDC("SCAN")
 . N ERR D FILE^DIE(,$NA(FDA),$NA(ERR))
 . ; ZEXCEPT: DIERR
 . I $D(DIERR) W "An error has occured. Contact your sys admin." D APPERROR^%ZTER("FILEMAN FILING") Q
 . W ?55,"...filed successfully."
 ;
 ;
 ;
 I Y="R" D  QUIT
 . N X,Y,DTOUT,DUOUT,DINUM,DLAYGO,DIC  ; ^DIC inputs and outputs
 . S DIC(0)="AEMQ",DIC="^PSRX(RXIEN,1,"
 . S DIC("W")="D EN^DDIOL($P(^(1),U,3),,""?50"")"  ; Custom identifiers
 . D ^DIC
 . Q:(Y<1)
 . N RFIEN S RFIEN=+Y
 . N RFNDC S RFNDC=$$GET1^DIQ(52.1,RFIEN_","_RXIEN_",","NDC")
 . D LOOPNDC(DRG,.RFNDC)
 . Q:(RFNDC=U)
 . N FDA
 . S FDA(52.1,RFIEN_","_RXIEN_",",11)=RFNDC
 . S FDA(52.1,RFIEN_","_RXIEN_",",21600)=RFNDC("SCAN")
 . N ERR D FILE^DIE(,$NA(FDA),$NA(ERR))
 . ; ZEXCEPT: DIERR
 . I $D(DIERR) W "An error has occured. Contact your sys admin." D APPERROR^%ZTER("FILEMAN FILING") Q
 . W ?65,"...filed successfully."
 ;
 I Y="P" D  QUIT
 . N X,Y,DTOUT,DUOUT,DINUM,DLAYGO,DIC  ; ^DIC inputs and outputs
 . S DIC(0)="AEMQ",DIC="^PSRX(RXIEN,""P"","
 . S DIC("W")="D EN^DDIOL($P(^(0),U,12),,""?50"")"  ; Custom identifiers
 . D ^DIC
 . Q:(Y<1)
 . N PRIEN S PRIEN=+Y
 . N PRNDC S PRNDC=$$GET1^DIQ(52.1,PRIEN_","_RXIEN_",","NDC")
 . D LOOPNDC(DRG,.PRNDC)
 . Q:(PRNDC=U)
 . N FDA
 . S FDA(52.2,PRIEN_","_RXIEN_",",1)=PRNDC
 . S FDA(52.2,PRIEN_","_RXIEN_",",21600.01)=PRNDC("SCAN")
 . N ERR D FILE^DIE(,$NA(FDA),$NA(ERR))
 . ; ZEXCEPT: DIERR
 . I $D(DIERR) W "An error has occured. Contact your sys admin." D APPERROR^%ZTER("FILEMAN FILING") Q
 . W ?65,"...filed successfully."
 QUIT
 ;
DICW ; [Do Not Touch] DIC("W") for the above call, as it's complicated.
 ; ZEXCEPT: Y from fileman
 ; D DICW^VFDPSNDC is executed by Fileman in DIC("W"). So don't be extrinsic.
 ; I want this: Rx #; Date; Patient Name; Drug Name; NDC
 ;                                  Refill 1 Date NDC
 ;                                  Refill 2 Date NDC
 ;
 ; ^DD(52,0,"ID)
 ; ^DD(52,0,"ID",6)="W:$D(^(0)) ""   ""_$S($D(^PSDRUG(+$P(^(0),U,6),0))#2:$P(^(0),U,1),1:"""")_$E(^PSRX(+Y,0),0)_$S($P($G(^PSRX(+Y,""STA"")),U)=13:""  ***MARKED FOR DELETION***"",1:"""")"
 ; ^DD(52,0,"ID",108)="W:$D(^(""D"")) ""   "",$P(^(""D""),U,3)"
 ;
 ; Sorry... this is shit ugly
 I '$D(^PSRX(+Y,0)) QUIT  ; no data
 N RXNUM S RXNUM=$P(^PSRX(+Y,0),U)
 N DN S DN=$S($D(^PSDRUG(+$P(^PSRX(+Y,0),U,6),0))#2:$P(^(0),U,1),1:"") ; DRG NAME
 N PN S PN=$S($D(^DPT(+$P(^PSRX(+Y,0),U,2),0))#2:$P(^(0),U,1),1:"")     ; PT NAME
 N ISS S ISS=$$FMTE^XLFDT($P(^PSRX(+Y,0),U,13))                        ; ISSUE DATE
 N NDC S NDC="" S:$D(^PSRX(+Y,2)) NDC=$P(^PSRX(+Y,2),U,7)               ; NDC
 N REF  ; refill array (ref(i)=date^ndc)
 N I F I=0:0 S I=$O(^PSRX(+Y,1,I)) Q:'I  D  ; for each refill
 . Q:'$D(^PSRX(+Y,1,I,0))  ; No data
 . S REF(I)=$$FMTE^XLFDT($P(^PSRX(+Y,1,I,0),U)) ; date
 . S REF(I)=REF(I)_U_$P($G(^PSRX(+Y,1,I,1)),U,3) ; ndc
 ;
 N PAR ; Partials (par(i)=date^ndc)
 N I F I=0:0 S I=$O(^PSRX(+Y,"P",I)) Q:'I  D  ; for each partial
 . Q:'$D(^PSRX(+Y,"P",I,0))
 . S PAR(I)=$$FMTE^XLFDT($P(^PSRX(+Y,"P",I,0),U)) ; date
 . S PAR(I)=PAR(I)_U_$P($G(^PSRX(+Y,"P",I,0)),U,12) ; ndc
 ;
 ; Now write this crap
 ; http://www.hardhats.org/fileman/pm/cl_ddiol.htm
 N WSTR
 S WSTR(1)=ISS,WSTR(1,"F")="?16"
 S WSTR(2)=$E(PN,1,50),WSTR(2,"F")="?45"
 S WSTR(3)=$E(DN,1,40),WSTR(3,"F")="!?3"
 S WSTR(4)="Original NDC: "_NDC,WSTR(4,"F")="?45"
 ;
 I $D(REF) D
 . N N S N=$O(WSTR(" "),-1)+1
 . S WSTR(N)="Refills: ",WSTR(N,"F")="!?6"
 . N I F I=0:0 S I=$O(REF(I)) Q:'I  D
 . . S N=$O(WSTR(" "),-1)+1
 . . S WSTR(N)=I_". Date: "_$P(REF(I),U),WSTR(N,"F")="!?7"
 . . S WSTR(N+1)="NDC: "_$P(REF(I),U,2),WSTR(N+1,"F")="?40"
 ;
 I $D(PAR) D
 . N N S N=$O(WSTR(" "),-1)+1
 . S WSTR(N)="Partials: ",WSTR(N,"F")="!?6"
 . N I F I=0:0 S I=$O(PAR(I)) Q:'I  D
 . . S N=$O(WSTR(" "),-1)+1
 . . S WSTR(N)=I_". Date: "_$P(PAR(I),U),WSTR(N,"F")="!?7"
 . . S WSTR(N+1)="NDC: "_$P(PAR(I),U,2),WSTR(N+1,"F")="?40"
 ;
 N N S N=$O(WSTR(" "),-1)+1
 S WSTR(N)="",WSTR(N,"F")="!"
 D EN^DDIOL(.WSTR)
 ;
 ; Restore naked reference
 I ^PSRX(+Y,0)
 QUIT
 ; -------------------------------------------------
 ;
 ; --- PARAMETER SETTINGS ---
 ;
 ; Capture on for various sections?
 ;
OPON() ; [Public] Is Outpatient NDC capture on?
 ; Return 0 or 1
 ; Call like this: I $$OPON^VFDPSNDC D ...
 I '$D(^%ZOSF("ZVX")) QUIT 0  ; No
 Q +$$GET^XPAR("ALL","VFD PSO NDC CAPTURE")  ; Yes or No.
 ;
IPON() ; [Public] Is Inpatient NDC capture on?
 ; Return 0, 1^F, 1^A
 ; Call like this: I $$IPON^VFDPSNDC,$P($$IPON^VFDPSNDC,U,2)="F" or '="F" etc.
 I '$D(^%ZOSF("ZVX")) QUIT 0  ; No
 N % S %=$$GET^XPAR("ALL","VFD PSJ NDC CAPTURE")
 I %="D" Q 0      ; Disabled
 I %="F" Q "1^F"  ; Enabled for Fill on Request
 I %="A" Q "1^A"  ; Enabled for All prescriptions
 QUIT 0  ; Parmeter is not set-up
 ;
111() ; [Public] Is this Site 111 (OMH)?
 N SITE S SITE=$P($$SITE^VASITE(),U,3)
 Q ($E(SITE,1,3)=111)
 ;
 ; -------------------------------------------------
 ;
 ;
ASSERT(X) ; Assert
 I 'X S $EC=",UASSERT,"
 QUIT
 ;
