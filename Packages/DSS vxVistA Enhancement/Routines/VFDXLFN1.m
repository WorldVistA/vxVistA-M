VFDXLFN1 ;DSS/SGM - NAME STANDARDIZATON (CONT) ;03/26/2014 14:10
 ;;2013.0;DSS,INC VXVISTA OPEN SOURCE;;07 Feb 2014;Build 2
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ; ***** This routine is only invoked via the VFDXLF routine *****
 ;
 ; ICR#  Use  Description
 ;-----  ---  ---------------------------------------------------------
 ; 2054  Sup  FDA^DILF
 ; 3065  Sup  ^XLFNAME: $$BLDNAME,$$CLEANC,$$FMNAME,$$NAMEFMT,STDNAME
 ;10104  Sup  ^XLFSTR: $$UP
 ;
 Q
 ;---------------------------------------------------------------------
 ;          CONVERT A PERSON'S NAME INTO VISTA STANDARD FORMAT
 ;---------------------------------------------------------------------
 ; 1. This line label is called only as a DO w/params
 ; 2. This module will call multiple ^XLFNAME APIs
 ; 3. It returns all possible data from the various XLFNAME APIs
 ; 4. ^XLFNAME standardizes string names and named components
 ;    a. HL format is a delimited string of named components
 ;    b. Convert string name into named components or vice versa
 ; 5. ACCEPTIBLE KEY_TERMS (or subscripts) for input or output:
 ;    For input of the string form of the name, use the NM ket term
 ;    FAMILY - family (last) name
 ;     GIVEN - given (first) name
 ;    MIDDLE - middle name(s)
 ;    SUFFIX - suffix(es)
 ;    PREFIX - prefix
 ;    DEGREE - degree
 ;        HL - string of name components in HL7 standard way
 ;         <last>dlm<first>dlm<middle>dlm<suffix>dlm<prefix>dlm<degree>
 ;             If DELIM not passed in then default to '^'
 ;       F20 - <source_DD#>;<source_field #>;<source_iens> [input only]
 ;      FILE - ;-piece one of F20
 ;     FIELD - ;-piece two of F20
 ;      IENS - ;-piece three of F20
 ;        NM - Entire name in string form
 ;       NMS - VistA standard name with prefix and degree
 ;      NMSL - VistA std name in mixed case with prefix and degree
 ;       NMG - Given name
 ;      NMGL - Given name in mixed case with prefix and degree
 ;      NMES - Standardized electronic signature name
 ;
 ; 6. INPUT PARAMETERS
 ;    ----------------
 ;    DELIM, MAX - opt
 ;      [.]VFDIN - req - see KEY_TERMS above, 3 different input types
 ;         a. VFDIN = name_input where program will determine key_term
 ;                   +VFDIN => key_term F20
 ;              VFDIN[DELIM => key_term HL
 ;                     Else => key_term NM
 ;         b. VFDIN(j) = key_term^value where j=1,2,3,... [from RPC]
 ;         c. VFDIN(key_term) = value [from API]
 ;       If value is the string form of the name then it must be one of
 ;       three types.  Value's characters can be upper, lower, or mixed
 ;                1) family,given middle suffix(es)
 ;                2) family, given middle suffix(es)
 ;                3) given middle family suffix(es)
 ;       NOTE: VistA Standardized Name Format
 ;                   family,given middle suffix(es) all uppercase
 ;             Given Name format
 ;                   given middle family suffix(es) all uppercase
 ;
 ; 7. RETURN VALUE(S)
 ;    ---------------
 ;      $$NAMECNVT returns the value of VFDNAME
 ;        VFDNAME = -1[^message] -OR- VistA_Standardized_Name
 ;        VFDNAME(key_term) = value(s)
 ;      D NAMECNVT returns
 ;        VFDNAME(j) = key_term ^ value   for j=1,2,3,4,5,...
 ;        If problems, VFDNAME(1) = -1[^msg]
 ;
NAMECNVT(VFDEF) ; see NAMECNVT^VFDXLF for input/output parameters
 ; VFDEF - opt - from VFDXLF to indicate whether module was called as
 ;               an extrinsic function or as a DO w/params
 ;               Default to 0:Do w/params  [ 1:extrinsic function]
 ; 
 N I,J,X,Y,Z,VOK,VQA,VRET
 K VFDNAME
 D  ; initialize local variables
 . S DELIM=$G(DELIM),MAX=$G(MAX),VFDEF=+$G(VFDEF)
 . S:DELIM=","!(DELIM=" ") DELIM="" S:DELIM="" DELIM=U
 . S MAX=$S('MAX:50,MAX>245:245,1:MAX)
 . Q
 ; QA input
 ; standardize all name components
 ; create as many standardized input array elements as possible
 S X=$$QAINPUT(.VRET,.VFDIN) I +X=-1 S VQA=X G NM0
 ; vfdin="" and .vfdin() exists
 S X=$$STDNAME(.VRET) I 'X S VQA=-1 G NM0
 S VQA="",X="",J=0
 ; if called as an API, then return alpha subscripts
 I $S(VFDEF:1,1:'$O(VFDIN(0))) M VFDNAME=VRET
 E  D
 . S Z=$$COMP
 . S J=1,VFDNAME(J)="NM^"_$G(VRET("NM"))
 . F I=1:1:16 I I'=12 S Y=$P(Z,U,I),J=J+1,VFDNAME(J)=Y_U_$G(VRET(Y))
 . Q
 ;
NM0 ;
 S Z=$S(VFDEF:1,1:'$O(VFDIN(0)))
 I +VQA=-1 S:Z VFDNAME=VQA S:'Z VFDNAME(1)=VQA
 Q
 ;
 ;---------------------------------------------------------------------
 ;                  VALIDATE FILE,FIELD,IENS VALID
 ;---------------------------------------------------------------------
 ; This validates that ^DD(FILE,FIELD) is a pointer field to file 20
 ; This also validates that IENS is a valid record
 ; Extrinsic Function returns 1 if valid, else return -1^msg
 ;
VALID() ;
 N I,J,X,Y,Z,ATT,DIERR,VFDA,VFDER
 I $S(VFILE'>0:1,VFILE'=+VFILE:1,VFLD'>0:1,1:VFLD'=+VFLD) Q $$ZERR(1)
 I $G(VIENS)'>0 Q $$ZERR(1)
 S ATT("POINTER")="" D FIELD^VFDCFM(.VFDA,VFILE,VFLD,,.ATT)
 ; first validate file and field
 I $G(VFDA("POINTER"))'="VA(20," D  Q X
 . I $D(VFDA("POINTER")) S X=$$ZERR(2)
 . E  S X=VFDA(1)
 . Q
 ; now validate iens
 K VFDA D FDA^DILF(VFILE,VIENS,VFLD,"R","","VFDA","VFDER")
 I '$D(DIERR) Q 1
 Q "-1^"_$$MSG^VFDCFM("E",,,,"VERR")
 ;
 ;=====================================================================
 ;               P R I V A T E    S U B R O U T I N E S
 ;=====================================================================
 ;
COMP() Q $P($T(COMP+1),";",3)_$P($T(COMP+2),";",3)
 ;;FAMILY^GIVEN^MIDDLE^SUFFIX^PREFIX^DEGREE^HL^F20^FILE^FIELD^IENS^NM^
 ;;NMS^NMSL^NMG^NMGL
 ;;
 ;;   1  ^  2  ^   3  ^   4  ^  5   ^   6  ^7 ^ 8 ^  9 ^ 10  ^ 11 ^12
 ;;13 ^ 14 ^15 ^ 16
 ;
 ;---------------------------------------------------------------------
UP(T) N I,X,Y,Z S:T?.E1L.E T=$$UP^XLFSTR(T) Q T
 ;
 ;---------------------------------------------------------------------
QAINPUT(OUT,IN) ;
 ; QA user/application call input
 ; Goal, return only one input value for standardization
 ; Since multiple input values allowed, determine which name component
 ;   to use and return only one type of component so that other modules
 ;   in this routine do not have to determine which of the varied input
 ;   values allowed to be used for standarding name components.
 ;
 ; Since this allows for multiple input types of names, then of the 4
 ;   XLFNAME name types allowed for standardization input, select the
 ;   input type in this precedence:
 ;   1. F20 input
 ;   2. HL input
 ;   3. Name string input
 ;   4. Use entire input array
 ;
 ; Some programming logic notes
 ;   If input contains delimiter then assume HL input
 ;
 ; RETURN PARAMETER: .OUT
 ;   OUT = "" or -1^error_msg
 ;   OUT(key_term[s]) = value(s)
 ;
 N I,J,X,Y,Z,ERR,MAX,RET,TMP
 S OUT="",ERR=$$ZERR(1)
 ;
 ; Move .IN() into a single TMP array all uppercased
 ; If $L(IN) then ignore all subscripted values
 S TMP=$G(IN) I $L(TMP) S TMP=$$UP(IN)
 I '$L(TMP) S X="" F  S X=$O(IN(X)) Q:X=""  D
 . S Y=$$UP(X),Z=$$UP(IN(X)) S:$L(Z) TMP(Y)=Z
 . Q
 ;
 ; Either TMP=value -OR- TMP(j)=value
 I $L(TMP) S X=TMP D  G QA0
 . I +X D QAF20 Q
 . I X[DELIM S OUT("HL")=X
 . E  S OUT("NM")=X
 . Q
 ;
 ; if alpha subscripts
 S Z=$P($$COMP,U,1,6)_U
 ;
 I '$O(TMP(0)) D  G QA0
 . S X=$G(TMP("F20")) I +X D QAF20 Q
 . S X=$G(TMP("HL")) I $L(X) D QAHL Q
 . S X=$G(TMP("NM")) I $L(X) D QANM Q
 . F I=1:1:6 S Y=$P(Z,U,I) S:$D(TMP(Y))#2 OUT(Y)=TMP(Y)
 . Q
 ;
 ; only numeric subscripts left
 S I=0 F  S I=$O(TMP(I)) Q:'I  D
 . S Y=$P(TMP(I),U),X=$P(TMP(I),U,2,99)
 . I Y="F20" K TMP D QAF20 Q
 . I Y="HL" K TMP D QAHL Q
 . I Y="NM" K TMP,OUT S OUT="",OUT(Y)=X Q
 . I Z[(Y_U) S OUT(Y)=X
 . Q
 ;
QA0 ;
 I OUT="",$D(OUT)=1 S OUT=ERR
 Q OUT
 ;
QAF20 ; get file 20 data
 N F,I,Z K OUT S OUT="",F="FILE^FIELD^IENS"
 F I=1,2,3 S Z(I)=$P(X,";",I)
 I Z(1),Z(2),Z(3) F I=1:1:3 S OUT($P(F,U,I))=Z(I)
 Q
 ;
QAHL K OUT S OUT="" S:X[DELIM OUT("HL")=X Q
 ;
QANM K OUT S OUT="",OUT("NM")=X Q
 ;
 ;---------------------------------------------------------------------
 ;
STDNAME(VIN) ;
 ; Run components through VistA name standardization
 ; .VIN - req - both an input and return parameter
 ;              VIN(key_term)=value
 ;              input array expects array output from $$QAINPUT
 ;
 ; EXTRINSIC FUNCTION returns: 1:no problems encountered, 0:problems
 ;
 ; Logic:
 ; 1. Initialize local array .VFDNM
 ; 2. Once VFDFN = standardized, can construct most return values
 ; 3. Or we may have VFDNM(j) = standardized name components
 ; 4. STDNAME was called after QA module so should only have one
 ;    input value to standardize.  QA determined which component
 ;    to standardize on.
 ;
 N I,J,X,Y,Z,VAUDIT,VFDNM,VFDX,VFDZ,VTMP
 ; Initialize VFDNM
 S VFDNM=""
 F X="FAMILY","GIVEN","MIDDLE","SUFFIX","PREFIX","DEGREE" S VFDNM(X)=""
 ;
 ; let's run through all the component types and standardize
 S X="" F  S X=$O(VFDNM(X)) Q:X=""  S Y=$G(VIN(X)) I $L(Y) D
 . S Z="" S:X="FAMILY" Z="F" S VFDNM(X)=$$CLEAN(Y,Z)
 . Q
 ;
 I $G(VIN("FILE")) D  G STDC
 . F X="FILE","FIELD","IENS" S VFDNM(X)=VIN(X)
 . S VFDNM=$$BLDNAME^XLFNAME(.VFDNM,MAX)
 . Q
 ;
 I VFDNM("FAMILY")'="" S VFDNM=$$BLDNAME^XLFNAME(.VFDNM,MAX) G STDC
 ;
 ; is input HL7
 S X=$G(VIN("HL")) I X'="" D  G STDC
 . S VFDX="CS" S:MAX VFDX=VFDX_"L"_MAX
 . S VFDNM=X,VFDNM=$$FMNAME^XLFNAME(.VFDNM,VFDX,DELIM)
 . Q
 ;
 ; only string name format left
 S X=$G(VIN("NM")) I X="" G STDC
 I X?.E1", ".E S X=$P(X,",")_","_$P(X,", ",2,9)
 S VFDNM=X K VAUDIT S VFDX="CP" S:VFDNM["," VFDX="FCP"
 D STDNAME^XLFNAME(.VFDNM,VFDX,.VAUDIT)
 ; if $D(VAUDIT) should this be considered an error condition?
 ; since passed in P, if VAUDIT("STRIP") only node then not error
 ;
STDC ;
 ; have standardized name, now let's get all missing componets
 ; if VFDNM="" then above failed
 K VIN I VFDNM="" Q 0
 ;
 ; get name components from standard name
 K VAUDIT S X="CP" S:VFDNM["," X="FCP"
 D STDNAME^XLFNAME(.VFDNM,X,.VAUDIT)
 ;
 ; initialize all return array nodes
 S Z=$$COMP F I=1:1:16 S X=$P(Z,U,I),VFDNM(X)=$G(VFDNM(X))
 ;
 ;
 ; HL subscript
 S X="" F I=1:1:6 S Y=$P(Z,U,I),$P(X,DELIM,I)=$G(VFDNM(Y))
 S VFDNM("HL")=X
 ;
 ; all the name "NM" subscripts to be set
 S VFDNM("NM")=VFDNM
 S VFDNM("NMS")=VFDNM
 S VFDNM("NMSL")=$$NAMEFMT^XLFNAME(.VFDNM,"F","CDcMP")
 S VFDNM("NMG")=$$NAMEFMT^XLFNAME(.VFDNM,"G")
 S VFDNM("NMGL")=$$NAMEFMT^XLFNAME(.VFDNM,"G","DcM")
 S VFDNM("NMES")=$$NAMEFMT^XLFNAME(.VFDNM,"G","DcMP")
 K VIN M VIN=VFDNM
 Q VFDNM'=""
 ;
CLEAN(X,F) N I,Y,Z S X=$G(X) S:$L(X) X=$$CLEANC^XLFNAME(X,$G(F)) Q X
 ;
 ;---------------------------------------------------------------------
ZERR(L) ;
 ;;No valid input parameter(s) received
 ;;DD file and field is not a pointer to file 20
 ;
 Q "-1^"_$P($T(ZERR+L),";",3)
 ;
 ;Example of ^TMP("JNYMDUZ1",$J) global
 ; ,2)="1^SHAIKH, MUHAMMAD G^SHAIKH, MUHAMMAD G^U^00743^^^"
 ; ,2,1)=1
 ; ,2,2)="SHAIKH, MUHAMMAD G"
 ; ,2,3)="SHAIKH, MUHAMMAD G"
 ; ,2,4)="U"
 ; ,2,5)="00743"
 ; ,2,6)=""
 ; ,2,7)=""
 ; ,2,8)=""
 ; ,2,"STD")="SHAIKH,MUHAMMAD G"
 ; ,2,"STD","DEGREE")=""
 ; ,2,"STD","F20")=""
 ; ,2,"STD","FAMILY")="SHAIKH"
 ; ,2,"STD","FIELD")=""
 ; ,2,"STD","FILE")=""
 ; ,2,"STD","GIVEN")="MUHAMMAD"
 ; ,2,"STD","HL")="SHAIKH^MUHAMMAD^G^^^"
 ; ,2,"STD","IENS")=""
 ; ,2,"STD","MIDDLE")="G"
 ; ,2,"STD","NM")="SHAIKH,MUHAMMAD G"
 ; ,2,"STD","NMES")="Muhammad G Shaikh"
 ; ,2,"STD","NMG")="MUHAMMAD G SHAIKH"
 ; ,2,"STD","NMGL")="Muhammad G Shaikh"
 ; ,2,"STD","NMS")="SHAIKH,MUHAMMAD G"
 ; ,2,"STD","NMSL")="Shaikh,Muhammad G"
 ; ,2,"STD","PREFIX")=""
 ; ,2,"STD","SUFFIX")=""
