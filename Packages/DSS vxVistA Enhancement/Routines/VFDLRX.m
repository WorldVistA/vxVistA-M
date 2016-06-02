VFDLRX ;DSS/FHS - DSS LR UTILITY; 4/16/14 4:06pm ;
 ;;2013.1;VENDOR - DOCUMENT STORAGE SYS;**16**;05 May 2014;Build 2
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;DSS/FHS - Dec 10, 2014
 ;General Utility LR Function calls
 Q
UIDLRDFN(LRUID) ;Return LRDFN when given UID
 ; LRUID=UID
 ; Returns LRDFN_U_DFN_U_LRMRN_U_LRCNUM
 ; Note: LRMRN and LRCNUM null for non-patients
 ; Note: LRCNUM error =*DFN*
 ; Return 0 if LRUID lookup fails
 ;
 N DFN,IEN,LRAA,LRAD,LRAN,LRCNUM,LRDFN,LRDPF,LRINST,LRMRN,LOOK,LRQ,LRX,X,Y
 I LRUID="" Q 0_U_"No LRUID PASSED"
 S LOOK=1
 S LRX=$$LRAA(LRUID,1)
 Q LRX
 ;
LRAA(LRUID,LOOK) ;Define for LRUID - variables LRSS,LRAA,LRAD,LRAN,LRDFN,LRIDT
 ; IN - UID of accession
 ; LOOK=1 OUT = LRDFN_U_DFN_U_LRMRN_U_LRCNUM
 ; LOOK=0 Also include ^LRO(68,LRAA,1,LRAD,1,LRAN) variables.
 ; OUT=0 If UID not found
 ; If LRUID defind OUT = LRDFN_U_DFN_U_LRMRN_U_LRCNUM
 ;  If LOOK=0 these variables defined
 ;  LRAA,LRAD,LRAN,LRSS,LRINST,LRDFN,DFN,LRIDT,LRDPF,LRMRN,LRODT,LRSN,LRDIV
 ;  LRXVAL=
 ;  GETS^DIQ(68.02,LRAN_","_LRAD_","_LRAA_",","*","I","LRXVAL","LRERR")
 ;  GETS^DIQ(69.01,LRSN_","_LRODT_",","*","I","LRXVAL","LRERR")
 K LRX I '$G(LOOK) D
 . K LRXVAL
 . S (LRAA,LRAD,LRAN,LRSS,LRINST,LRDFN,DFN,LRIDT,LRDPF,LRMRN,LRODT,LRSN)=""
 . S (LRIDIV,LRXVAL)=""
 S LRX=0
 S LRQ=$Q(^LRO(68,"C",LRUID))
 I $QS(LRQ,3)'=LRUID,$G(LOOK) Q LRX_U_"Not valid UID"
 I $QS(LRQ,3)'=LRUID S LRX="0^Not valid UID" Q LRX
 S LRAA=$QS(LRQ,4),LRAD=$QS(LRQ,5),LRAN=$QS(LRQ,6)
 S LRDFN=+$G(^LRO(68,LRAA,1,LRAD,1,LRAN,0))
 S LRINST=+$G(^LRO(68,LRAA,1,LRAD,1,LRAN,.4))
 I LRDFN<1 S LRX="0^Corrupt or missing accesion" I $G(LOOK) Q LRX
 I LRDFN<1 Q 0
 S LRDPF=$P(^LR(LRDFN,0),U,2),DFN=$P(^(0),U,3)
 S LRMRN="" S:LRDPF=2 LRMRN=$$ID^VFDDFN(DFN,,"MRN",,1)
 S LRCNUM="" S:LRDPF=2 LRCNUM=$$ID^VFDDFN(DFN,LRINST,"C_NUM",,1)
 S LRX=LRDFN_U_DFN_U_LRMRN_U_LRCNUM
 I $G(LOOK) Q LRX
DATA68 ; Pull LRO(68 Data
 S LRSS=$P(^LRO(68,LRAA,0),U,2),LRIDIV=$P(^(0),U,19)
 S:LRIDIV="" LRIDIV="CP"
 S LRODT=$P(^LRO(68,LRAA,1,LRAD,1,LRAN,0),U,4),LRSN=$P(^(0),U,5)
 S LRORD=$P(^LRO(68,LRAA,1,LRAD,1,LRAN,.1),U)
 S LRACC=$P(^LRO(68,LRAA,1,LRAD,1,LRAN,.2),U)
 S LRDIV=$P(^LRO(68,LRAA,1,LRAD,1,LRAN,.4),U)
 S LRCDT=$P(^LRO(68,LRAA,1,LRAD,1,LRAN,3),U),LRIDT=$P(^(3),U,5)
 K LRERR
 D GETS^DIQ(68.02,LRAN_","_LRAD_","_LRAA_",","*","I","LRXVAL","LRERR")
 D GETS^DIQ(69.01,LRSN_","_LRODT_",","*","I","LRXVAL","LRERR")
 Q LRX
 ;
PROC(LRLL,LRISQN) ; Call PROC^VFDLRAV(LRLL,LRISON)
 ; Return LREND=0 if no error
 ; Return LREND=1^test if failed
 D PURGPROC
 S LREND=0
 D PROC^VFDLRAV(LRLL,LRISQN)
 I $G(LREND) Q LREND
 S VFD6249=$P(LRNODE,U,13)
 S LREND=$S($P(^LAHM(62.49,VFD6249,0),U,3)="X":0,1:1_U_"Processing error")
 I LREND D PURGPROC Q LREND
 S LRDFN=+^LRO(68,LRAA,1,LRAD,1,LRAN,0)
 S LRDPF=$P(^LR(LRDFN,0),U,2),DFN=$P(^(0),U,3)
 D
 . N LREND
 . D PT^LRX
 D DATA68
 I $D(LRERR) S LREND="1^Error setting LRXVAL data" D PURGPROC
 Q LREND
 ;
PURGPROC ;Clean up PROC call variables
 D KVAR^VADPT
 K AGE,DFN,DOB,DOD,LRAA,LRAD,LRAN,LRDPF,LRDFN,LRNODE,LRNT
 K LRPRAC,LRRB,LRSS,LRTREA,LRTST,LRTSTS,LRUID,LRWRD,LRXVAL,PNM
 K SEX,SSN,VA,VAERR,VFD6249,VFDFID,LRERR
 K L,L1,LRACC,LRCDT,LRDIV,LRIDIV,LRIDT,LRODT,LRORD,LRSN
 Q
 ;
MFILESEL(VAL) ;Select which file to add/edit entry
 ;1:LAB CODE MAPPING(#62.47)
 ;2:ETIOLOGY FIELD(#61.2)
 N DIR,VAL,Y,X
 S DIR(0)="SO^1:LAB CODE MAPPING(#62.47);2:ETIOLOGY FIELD(#61.2)"
 S DIR("L")="   "
 S DIR("L",1)="            Select file to add or edit entry"
 S DIR("L",2)="  "
 S DIR("L",3)="           1     LAB CODE MAPPING(#62.47)"
 S DIR("L",4)="   "
 S DIR("L",5)="           2     ETIOLOGY FIELD(#61.2)"
 D ^DIR
 W !
 I Y="" Q 0
 I Y=1 S VAL=62.47 Q VAL
 I Y=2 S VAL=61.2 Q VAL
 Q 0
 Q
MFILEDIT ;Add or entry for LEDI standard terms
 ; Used to allow user to edit 62.47 or 61.2 without having programmer mode access
 ; Variables LRFPRIV and LRFMERTS are set to allow edits or additions to be made
 N LRFPRIV,LRFMERTS
 N LRFILE,VAL,DIC,DIE,D0,D1
 N DIR,DXS,XMV,X,Y
 S LRFILE=$$MFILESEL(.VAL) ; Select file to use
 I LRFILE=0 W !!,?10,"Nothing Selected" Q
 I LRFILE=62.47 S LRFPRIV=1
 I LRFILE=61.2 S LRFMERTS=1
 S DIE=LRFILE D EN^DIB
 Q
X21660 ;Input screen for reference normal low - normal high input.
 ; Called from: $P(^DD(21660,DA,5),U,5)
 ; Called from: $P(^DD(21660,DA,6),U,5)
 ; Prevent entering values that conflict with other entries.
 ; Input DA of the entry being edited
 ; X=value to store
 ; Output X if passes screen
 Q:$D(DIU(0))
 I $G(DIUTIL)="VERIFY FIELDS" Q
 N AGETYP,IEN,VAL,LROK,LRTST,LRSPEC,NODE0,VAL,RGEL,RGEH,SEXX
 S NODE0=^VFDLR(21660,DA,0),LROK=0
 I $P(NODE0,U,12),$G(LRDUZ(2)),$P(NODE0,U,12)'=$G(LRDUZ(2)) Q
 S LRTST=$P(NODE0,U,2)
 S LRSPEC=$P(NODE0,U,3),SEX=$P(NODE0,U,4)
 S AGETYP=$P(NODE0,U,5) ; LRDUZ(2)=+$P(NODE0,U,12)
 S SEXX=SEX F SEX="A",SEXX D  Q:$G(LROK)
 . S IEN=0 F  S IEN=$O(^VFDLR(21660,"AC",LRTST,LRSPEC,SEX,AGETYP,IEN)) Q:IEN<1!($G(LROK))  I IEN'=$G(DA) D
 . . S NODE=^VFDLR(21660,IEN,0) S RGEL=$P(NODE,U,6),RGEH=$P(NODE,U,7)
 . . I $G(LRDUZ(2)),+$P(NODE,U,12)'=LRDUZ(2) Q
 . . I $S(RGEL=""!(RGEH=""):1,1:0) Q 
 . . S LROK=$S((X=RGEL!(X=RGEH)):1,(X>RGEL&(X<RGEH)):1,1:0)
 . . I $G(LROK) D EN^DDIOL("Deleted - value of ("_X_") ConflictS with : ["_IEN_"] "_$P(NODE,U),,"!") K X
 Q
RANGE(TST,SPEC) ; Pass in test and specimen for lookup
 ; Called from V25^LRVER5
 ; Set normal values based 21660 data
 ; If in testing mode LRVVRFY=1 --- caled from TEST^VFDLRNR
 ; LRVVIEN RETURN to testing option.
 N AGEX,LRANS,LRANX,LROK,AGEFLG,NODE,RANGE,RG1,RG2,IEN,AGEFLG,SEXX,LRCHK
 N SAVESEX,CHK
RANGEX ;
 I $G(LRCDT),$G(DOB) S AGE(2)=$$AGE2^LRX(DOB,LRCDT)
 S AGEFLG=$S(AGE(2)["yr":"Y",AGE(2)["dy":"D",AGE(2)["mo":"M",1:0) S:AGEFLG="" AGEFLG="Y"
 S (LRANS,LRNGX,LRANX,LROK,LRCHK,LRVVIEN)=0,SEXX=SEX
 I SEX="A" F SEX="A","M","F" D CHK Q:$G(LRVVIEN)
 I SEX'="A" F SEX=SEX,"A" D CHK Q:$G(LRVVIEN)
 S SEX=SEXX
 Q
 ;
CHK ;Check for match in file in the Multi-normal range file - prevent duplicats
 S IEN=0 F  S IEN=+$O(^VFDLR(21660,"AD",+$G(LRDUZ(2),DUZ(2)),TST,SPEC,SEX,AGEFLG,IEN)) Q:IEN<1!($G(LRVVIEN))  W:$G(LRDBUG) !,"INST "_IEN,! D CHK1
 I '$G(LRVVIEN) S IEN=0 F  S IEN=+$O(^VFDLR(21660,"AC",TST,SPEC,SEX,AGEFLG,IEN)) Q:IEN<1!($G(LRVVIEN))  W:$G(LRDBUG) !,"NO Inst "_IEN,! D CHK1
 Q
 ;
CHK1 ; Looking for age range for multi-normal range function
 S NODE=^VFDLR(21660,IEN,0)
 S SAVESEX(IEN)=NODE
 S CHK(IEN)=$G(LRCHK)_U_$G(LROK)_U_$G(LRVVIEN)
 I '$G(LRVVRFY),'$P(NODE,U,13) Q  ;Activate entery
 I $P(NODE,U,12),$P(NODE,U,12)'=$G(LRDUZ(2),DUZ(2)) Q
 S LRCHK=$$AGE(SEX,.AGE,NODE)
 I $G(LRVVIEN) D
 . I $G(LRVVRFY) S LRNGX=LRNG
 . S $P(LRNG,U,2)=$P(LRANX,U,3)
 . S $P(LRNG,U,3)=$P(LRANX,U,4)
 . I $P(LRANX,U,5) S $P(LRNG,U,4)=$P(LRANX,U,5)
 . I $P(LRANX,U,6) S $P(LRNG,U,5)=$P(LRANX,U,6)
 Q
AGE(SEX,AGE,NODE) ; Find Age/Sex range for IEN
 ;NODE=^VFDLR(21660,IEN,0)
 K AGEX
 S LRCHK=0
 I SEX'="A",SEX'=$P(NODE,U,4) Q LRCHK
 S LROK=0,LRANX=0,RANGE=$P(NODE,U,5)
 S AGEFLG=""
 S AGEFLG=$S(AGE(2)["yr":"Y",AGE(2)["dy":"D",AGE(2)["mo":"M",1:0) S:AGEFLG="" AGEFLG="Y"
 S RG1=$P(NODE,U,6),RG2=$P(NODE,U,7),AGEX=+AGE(2)
 I (AGEX=RG1!(AGEX>RG1)&(AGEX=RG2!(AGEX<RG2))) S LRANX=AGEX_U_RANGE_U_$P(NODE,U,8,13) D
 . S LRCHK=1,LRVVIEN=IEN
 Q LRCHK
 ;
ULRIDT(LRDFN,LRSS,LRXCDT) ; Find unique ^LR(LRDFN,LRSS,LRIDT,0) node
 ; LRCDT is change to match LRIDT value if required
 ; Out = LRIDT
 I '$D(^LR(+$G(LRDFN),0)) Q "0^Patient not defined"
 N LRXIDT
 S LRIDT=9999999-LRXCDT
 I $D(^LR(LRDFN,LRSS,LRIDT,0)) S LRIDT=0 D
 . F  D  Q:LRIDT 
 . . S LRXIDT=9999999-LRCDT
 . . L +^LR(LRDFN,LRSS,LRXIDT,0):5
 . . I '$T S LRCDT=$$FMADD^XLFDT(LRCDT,0,0,0,1) Q
 . . I '$D(^LR(LRDFN,LRSS,LRXIDT,0)) S LRIDT=LRXIDT Q
 . . L -^LR(LRDFN,LRSS,LRXIDT,0)
 . . S LRCDT=$$FMADD^XLFDT(LRCDT,0,0,0,1)
 Q LRIDT
 ;
DTLNG(VAL) ; Convert 20110531140551-0500 to Tue May 31 14:05:51 GMT-05:00 2011
 ;
 ; RETURN THE DAY OF THE WEEK $$DOW^XLFDT(x[,y])
 ; - X = FM DATE
 ;
 Q:VAL'?14N1"-"4N "-1^Format not recognized."
 N FM,DY,EFM,TM,OS
 S FM=$E(VAL,1,4)-1700_$E(VAL,5,8)
 S DY=$E($$DOW^XLFDT(FM),1,3)
 D DT^DILF("E",FM,.EFM) Q:EFM=-1 "-1^Unable to convert."
 S TM=$E(VAL,9,14),TM=$E(TM,1,2)_":"_$E(TM,3,4)_":"_$E(TM,5,6)
 S OS=$P(VAL,"-",2),OS=$E(OS,1,2)_":"_$E(OS,3,4),OS="GMT-"_OS
 Q DY_" "_$$TITLE^XLFSTR($P(EFM(0),","))_" "_TM_" "_OS_" "_$E(VAL,1,4)
 ;
UP9531   ; Change ^LAB(95.31,X,0) to uppercase to be compatible with ^DD(62.9001 screens
 ; Change ^DD(95.31,.01,0) Component to all uppercase text
 ; D BMES("Changing field DD(95.31,.01,0) to UpperCase Text")
 ; added 9/14/2014 for CORE T8
 D
 . N IEN,IENX,FDA,ERR,VAL,VALX,X
 . S IEN=0 F  S IEN=$O(^LAB(95.31,IEN)) Q:IEN<1  D
 . . S VAL=$G(^LAB(95.31,IEN,0)) Q:VAL=""  D
 . . . K FDA S IENX=IEN_",",VALX=$$UP^XLFSTR(VAL)
 . . . S FDA(2,95.31,IENX,.01)=VALX
 . . . D FILE^DIE("K","FDA(2)","ERR")
 . . . I $D(ERR) W !,IEN,"  ",VALX
 Q
