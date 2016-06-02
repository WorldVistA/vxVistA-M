VFDXUS2B ;DSS/SGM - A/V SUPPORT MODS IN XUS* ; 1/26/2016 1335
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**7,55**;26 Jan 2016;Build 2
 ;Copyright 1995-2016,Document Storage Systems Inc. All Rights Reserved
 ;
 ; ICR#  Sup  Description
 ;-----  ---  ---------------------------------------------------------
 ; 2263   Y   GET^XPAR
 ; 3065   Y   $$HLNAME^XLFNAME
 ; 4057  CSX  AVHLPTXT^XUS2
 ;       no   ^XUS2: AHELP, CLR
 ; 4058  CSX  Global read of ^VA(200,"A")
 ;       no   Global read of ^VA(200,"AOLD")
 ; 4758  CSX  $$EN^XUSHSH
 ;10060   Y   All fields in file 200 supported for Read w/Fileman
 ;10069   Y   ^XMB
 ;10096   Y   All nodes ^%ZOSF() are useable
 ;10097   Y   $$LGR^%ZOSV
 ;10104   Y   UP^XLFSTR
 ;Notes: no = no VA ICR# of any type
 ;      CSX = controlled subscription type, but we are not a subscriber
 ;
 ;  VXVISTA CONTROLLED SUBSCRIPTION APIS
 ; FOR KERNEL SECURITY MODIFICATIONS ONLY
 ;
 ;External routine calls into this routine:
 ; Line Tag  | Routine | Patch | Line tag in VFDXUS2B invoked
 ;-----------|---------|-------|-----------------------------
 ;CHECKAV+14 | XUS     |  584  | $$MIXCASE(.X1)
 ;CHECKAV+20 |         |       | VXTX("EXTA")
 ;AASK1+5    | XUS2    |  574  | $$AASK1
 ;GET+2      |         |       | $$VCASE
 ;VCHK+1     |         |       | $$VCHK
 ;BRCVC+1    |         |       | $$BRCVC
 ;VXNET+6    |         |       | $$PARAM(1)
 ;VXTEXT     |         |       | TEXT(,TAG,WR,BELL)
 ;VALIDAV+23 | XUSRB   |  523  | $$PARAM(1)
 ;VER+5      | XUVERIFY|  265  | $$SYS
 ;
 ;---------------------------------------------------------------------
 ;Access Code Pattern Match/Help Text Display Replaced in $$AASK1^XUS2
 ;---------------------------------------------------------------------
AASK1(MIN) ;
 ;Extrinsic Function Returns:
 ;   1:no issues; 0:unacceptable input; -1:vxVistA config as FOIA
 ; MIN - opt - minimum number of chars, default to 3
 ;
 ;Original code at AASK1^XUS2 was
 ;I X[$C(34)!(X[";")!(X["^")!(X[":")!(X'?.UNP)!($L(X)>20)!($L(X)<6)!(X="MAIL-BOX") D CLR W $C(7),$$AVHLPTXT(1) D AHELP Q
 ;I 'XUAUTO,((X?6.20A)!(X?6.20N)) D CLR W $C(7),$$AVHLPTXT(1),! Q
 N I,Y,Z,ERR,PARAM,TAG,VXH
 S PARAM=$$PARAM(3) I "f"[PARAM Q -1 ; let unmodified VA code process
 ;
 ;I PARAM="o" Q -9 ;  activate if we have an 'other' method
 ;
 ;  vxVistA rules apply (a.k.a. CCHIT), 3-20 chars, same punctuation
 ;  chars excluded as FOIA, allow lower case, may be all alpha, and
 ;  must have at least 1L and 1U
 Q $$VXACODE($G(MIN))
 ;
 ;---------------------------------------------------------------------
 ;       Called From BRCVC^XUS2 To Change Verify Code From Broker
 ;---------------------------------------------------------------------
BRCVC() ;
 Q:$S('$$VXNET(""):0,1:$$T1("TVCH",6))
 ;
 ;---------------------------------------------------------------------
 ;              ^XUS and ^XUS2 To Uppercase User A/V Input
 ;---------------------------------------------------------------------
MIXCASE(VFX,ACN,VCN) ;
 ;  VFX - req - is the user's input which may or may not have a ';'
 ;  ACN - opt - Boolean, if 1 do not uppercase Access code
 ;  VCN - opt - Boolean, if 1 do not uppercase Verify code
 ;  called from CHECKAV^XUS where X1 passed by reference (.X1) in .VFX
 ;  if VFX'[";" then assume code is Access code only
 ;  Verify code only should be sent in as ";"_VFX
 ;  Future improvement, provide more discreet handling of access vs
 ;    verify code and all the differing combinations of the two.
 ;
 I $G(VFX)="" Q ""
 I $G(ACN),$G(VCN) Q VFX
 I '$$PARAM(2) Q $$UC(VFX)  ;RAC 2/3/16 - chgd from (X) to (VFX) to prevent UNDEF error
 I VFX'[";" Q $$UC(VFX)
 I VFX[";" S $P(VFX,";")=$$UC($P(VFX,";"))
 Q VFX
 ;
VCASE(VFX) ; called from GET^XUS2
 ; VFX - req - inputed A or V code, unencrypted without a ';'
 ;  if here via VERED^XUS2, then VFDVC=1
 I $G(VFX)="" Q ""
 I $G(VFDVC),VFX'[";" S VFX=";"_VFX
 S VFX=$$MIXCASE(VFX) I $G(VFDVC) S VFX=$P(VFX,";",2)
 Q VFX
 ;
 ;---------------------------------------------------------------------
 ;     Called When Kernel Security vxVistA Parameter Value Needed
 ; RAC 2/3/16 - deprecate the VFD CASE-SENSITIVE VERIFY CODE take the
 ; value of VFD VERIFY CODE PATTERN if FOIA set verify to Upper case
 ; and if anything else verify code should be Upper and Lower case
 ;---------------------------------------------------------------------
PARAM(PARAM,ENT,INST) ;
 N P1,RET
 I $G(PARAM)="" Q ""
 I PARAM?1.N
 S P1=PARAM S:P1=2 PARAM=3
 S PARAM=$$P(PARAM) I PARAM="" Q ""
 S ENT=$G(ENT) S:ENT="" ENT="ALL"
 I $G(INST)="" S RET=$$GET^XPAR(ENT,PARAM)
 E  S RET=$$GET^XPAR(ENT,PARAM,INST)
 I P1=2,RET["f" S RET=0 ; For FOIA set case to upper
 I P1=2,RET'["f" S RET=1 ; For anything else set to U/L case
 Q RET
 ;
P(N) ;
 ;;VFD AUTH ENABLE
 ;;VFD CASE-SENSITIVE VERIFY CODE
 ;;VFD VERIFY CODE PATTERN
 ;
 Q $S($G(N)<1:"",1:$P($T(P+N),";",3))
 ;
 ;---------------------------------------------------------------------
 ;                    Return vxVistA System Type
 ;---------------------------------------------------------------------
SYS() ;
 ; 9/20/2013 - not properly coded to handle Other types
 ; X(3) = f:FOIA   c:vxVistA   o:Other
 ; Return one of three characters: f or c or o
 ;   if "*" appended to return, then verify code case sensitive
 ;   if "#" appended to reutrn, then network authentication enabled
 N I,X,Y,Z
 F I=1,2,3 S X(I)=$$PARAM(I)
 S Y=X(3) S:X(2) Y=Y_"*" S:X(1) Y=Y_"#"
 Q Y
 ;
 ;---------------------------------------------------------------------
 ;                Retrieve And/Or Write Text Messages
 ;---------------------------------------------------------------------
TEXT(VFDRET,TAG,WR,BELL,TXT) ;
 ; .TAG - opt - line tag name in this routine
 ;                 TAG = line tag name and/or
 ;              TAG(#) = line tag name
 ;   WR - opt - Boolean, default to 0
 ;              If 0 then return text in VFDRET()
 ;              If +WR then Write text to current device
 ;              If WR>1 then do not do an initial W !
 ;              If $D(DDS) S WR=2 ; screenman mode
 ; BELL - opt - Boolean, default to 0, if +BELL W $C(7) first
 ; .TXT - opt - additional text to be display -OR-
 ;              text to be displayed in lieu of text for TAG
 ; Return: .VFDRET
 ;   Return array passed by reference to retrieve the text msgs
 ;   VFDRET(#) = text     VFDRET = # lines of text or ""
 ;
 N A,B,C,T,VXH
 S WR=+$G(WR),BELL=+$G(BELL)
 I WR S WR=$S($D(DDS):2,WR>1:2,1:1)
 I $D(TAG) D
 . I $G(TAG)'="" D TH(TAG) Q
 . S A=0 F  S A=$O(TAG(A)) Q:'A  D TH(TAG(A))
 . Q
 I $D(TXT) D
 . S C=$O(VXH(" "),-1)
 . S A=0 F  S A=$O(TXT(A)) Q:'A  S C=C+1,VXH(C)=TXT(A)
 . Q
 S VXH=$O(VXH("A"),-1)
 I WR W:BELL $C(7) W:WR=1 !
 I WR,$D(VXH) S A=0 F  S A=$O(VXH(A)) Q:'A  W VXH(A),!
 M VFDRET=VXH
 Q:$Q VXH Q
 ;
 ;---------------------------------------------------------------------
 ;                         Uppercase String
 ;---------------------------------------------------------------------
UC(X) S:X?.E1L.E X=$$UP^XLFSTR(X) Q X
 ;
 ;---------------------------------------------------------------------
 ;       Called From VCHK^XUS2 To Validate Verify Code Pattern
 ;---------------------------------------------------------------------
VCHK(S,EC) ;
 ; extrinsic function call expecting 0 or #^message
 ;VCHK(S,EC) ;Call with String and Encrypted versions
 ; I $$VXCK N VXCK S VXCK=$$VCHK^VFDXUS2B(S,EC) Q:VXCK'=-1 VXCK ;DSS/vxvista
 ; ;Updated per VHA directive 6210 Strong Passwords
 ; N PUNC,NA,XUPAT S PUNC="~`!@#$%&*()_-+=|\{}[]'<>,.?/"
 ; S NA("FILE")=200,NA("FIELD")=.01,NA("IENS")=DA_",",NA=$$HLNAME^XLFNAME(.NA),XUPAT=XUSVCMIN_".20"
 ; I ($L(S)<XUSVCMIN)!($L(S)>20)!(S'?.UNP)!(S[";")!(S["^")!(S[":") Q "1^"_$$AVHLPTXT
 ; I (S?@(XUPAT_"A"))!(S?@(XUPAT_"N"))!(S?@(XUPAT_"P"))!(S?@(XUPAT_"AN"))!(S?@(XUPAT_"AP"))!(S?@(XUPAT_"NP")) Q "2^VERIFY CODE must be a mix of alpha and numerics and punctuation."
 ; I $D(^VA(200,DA,.1)),EC=$P(^(.1),U,2) Q "3^This code is the same as the current one."
 ; I $D(^VA(200,DA,"VOLD",EC)) Q "4^This has been used previously as the VERIFY CODE."
 ; I EC=$P(^VA(200,DA,0),U,3) Q "5^VERIFY CODE must be different than the ACCESS CODE."
 ; I S[$P(NA,"^")!(S[$P(NA,"^",2)) Q "6^Name cannot be part of code."
 ; Q 0
 N MIN,PARAM S PARAM=$$PARAM(3),MIN=8
 I "f"[PARAM Q -1 ; let the original FOIA process VC checking
 I PARAM="o" Q -1 ; RAC 2/4/16 removed comment and follow FOIA or D SOMETHING Q value'=-1
 ;
 N PUNC,NA,XUPAT S PUNC="~`!@#$%&*()_-+=|\{}[]'<>,.?/"
 S NA("FILE")=200,NA("FIELD")=.01,NA("IENS")=DA_","
 S NA=$$HLNAME^XLFNAME(.NA),XUPAT=MIN_".20"
 I $L(S)<8!($L(S)>20)!(S'?.ANP)!(S[";")!(S["^")!(S[":") Q "2^"_$$T1("TACH",1)
 ;cannot be all 1 type of character
 I (S?1.U)!(S?1.L)!(S?1.N)!(S?1.P) Q $$T1("TVCH",1)
 I (S'?.E1L.E)!(S'?.E1U.E) Q $$T1("TVCH",1)
 I $D(^VA(200,DA,.1)),EC=$P(^(.1),U,2) Q $$T1("TVCH",2)
 I $D(^VA(200,DA,"VOLD",EC)) Q $$T1("TVCH",3)
 I EC=$P(^VA(200,DA,0),U,3) Q $$T1("TVCH",4)
 I S[$P(NA,"^")!(S[$P(NA,"^",2)) Q $$T1("TVCH",5)
 Q 0
 ;
 ;---------------------------------------------------------------------
 ;          Called From XUS* or ZT* To Run Supported API Call
 ;---------------------------------------------------------------------
VXTX(CODE) ;
 ; CODE - req - shortcut name for Supported API to run
 S CODE=$G(CODE)
 I "^LST^LSU^"[(U_CODE_U) Q:$Q "" Q  ; deprecated
 Q:$Q $$XTX(CODE)
 D XTX(CODE)
 Q
 ;
 ;---------------------------------------------------------------------
 ;                PRIVATE MODULES FOR THIS ROUTINE ONLY
 ;---------------------------------------------------------------------
SET S TEST=$T,LGR=$$LGR^%ZOSV Q
 ;
OUT ; return value
 ; reset naked reference and $TEST to value prior to API call
 I $G(LGR)'="" S LGR=$G(@LGR) ; reset naked reference
 I $G(TEST) ; reset $TEST value
 Q:$Q VX Q
 ;
T1(TAG,OFF) ;
 N W
 S W=$P($T(@TAG+OFF),";",3,99)
 I TAG="TACH",OFF=1 S W=$$THFIX(1,W)
 Q W
 ;
TH(TAG) ;
 ; expect [not required] VXH(), does not initialize, but adds to it
 N A,B,C,I,L,T
 I TAG="TNETA",+$G(DA),+$G(^VA(200,DA,21603)) S TAG="TNETAN"
 S L=+$O(VXH(" "),-1)
 F I=1:1 S T=$P($T(@TAG+I),";",3,99) Q:T=""  S L=L+1,VXH(L)=T
 I TAG="TACH" S VXH(1)=$$THFIX(1,VXH(1))
 Q
 ;
THFIX(TYP,W) ; place help specific modifications here
 I TYP=1 Q $P(W,"|")_$G(MIN,3)_$P(W,"|",2)
 Q
 ;
TACH ; edit access code help
 ;;Enter |-20 characters any combination of alphanumeric-punctuation
 ;;  You cannot include any of these characters: '^', ';', ':'
 ;;  You cannot have all numeric or all punctuation characters
 ;;
TNETA ; network authentication enabled - access code
 ;;vxVistA configured for network authentication
 ;;You must enter user's network USERNAME as their ACCESS CODE.
 ;;
TNETAN ; net auth enabled, access code, user config to not net auth
 ;;vxVistA configured for network authentication
 ;;This user is configured to use VistA authentication instead of using
 ;;network authentication.
 ;;
TNETV ; network authentication enabled - verify code
 ;;Your system is configured for network authentication.  As such there
 ;;is no need to edit your Verify Code as your network password will be
 ;;used for authentication and privileging for vxVistA.
 ;;
TVCH ; edit verify code help
 ;;2^Minimally, at least 1 uppercase and 1 lowercase character required
 ;;3^This code is the same as the current one.
 ;;4^This has been used previously as the VERIFY CODE.
 ;;5^VERIFY CODE must be different than the ACCESS CODE.
 ;;6^Name cannot be part of code
 ;;1^Network authentication active, no editing of verify code needed
 ;;
TERR(N) ;
 ;;-1^This option not to be used to change one's own ACCESS CODE.
 ;;-1^This ACCESS CODE value is already in use
 ;;-1^This is user's current ACCESS CODE
 ;;-1^This has been used previously as an ACCESS CODE.
 ;;
 Q $P($T(TERR+N),";",3)
 ;
VXACODE(MIN) ; cchit rules for access code
 ; expects X = unencrypted access code
 ; cloned and modified AASK1^XUS2
 ; RETURNS: 1:AC passes tests; 0:AC failed test
 N ERR,VXH
 S ERR=0,MIN=$G(MIN) S:'MIN MIN=3
 I X[$C(34)!(X[";")!(X["^")!(X[":")!(X'?6.20UNP) S ERR=1
 I X="MAIL-BOX" S ERR=1
 I 'ERR,'XUAUTO,X?1.N!(X?1.P) S ERR=1
 I 'ERR Q 1
 D CLR^XUS2
 ; maximum of 4 lines of help text
 D TH("TACH") K:$$VXNET&$D(DDS) VXH(3) D TH("TNETA")
 D TEXT(,,2,1,.VXH)
 Q 0
 ;
VXAFOIA(IGN,VXUS) ; FOIA rules for access code
 ;   DA - req - expected to be defined and equal to a NEW PERSON DUZ
 ;  IGN - opt - Boolean, default to 0
 ;              1: allow value even if used before (AOLD xref)
 ; VXUS - opt - Boolean, default to 0, 1:call from normal Kernel edit
 ;              put here just in case for future use.
 ;BUSINESS RULES
 ;1. Normal VistA processes will run through the VA code not this code
 ;2. Allows for previously used access code
 ;3. For VFDXTCAC, does not allow a person to change own code
 ;4. Routines that invoked this code:
 ;   a. VFDXTCAC
 ;       1) Return 1^<unencrypted access code>^<encrypted access code>
 ;                 0 if invalid access code  or  -1^message
 ;   b. XUS2 - called via AASK1 from above
 ;       1) Not invoked as of cloned/modified date (see begin routine)
 ;       2) Certain X* variables expected to defined here by ^XUS2
 ;       3) Return 0:invalid input or 1:valid input
 ;
 ;NOTE:
 ; +VXUS should be 0.  But code put here if ever this routine is
 ; invoked and this module invoked when called from ^XUS2.
 ;
 N USERAC S USERAC=0,IGN=$G(IGN),VXUS=$G(VXUS)
 I '$G(DA)!($G(DUZ)<1) G FOUT
 I $G(^VA(200,DA,0))="" G FOUT
 S USERAC=1
 ; VA logic from AASK1^XUS2 via AASK1^VFDXUS2B
 I VXUS D  G FOUT
 . I X[$C(34)!(X[";")!(X["^")!(X[":")!(X'?.UNP) S USERAC=0
 . I ($L(X)>20)!($L(X)<6)!(X="MAIL-BOX") S USERAC=0
 . I 'XUAUTO,((X?6.20A)!(X?6.20N)) S USERAC=-1
 . Q:USERAC>0
 . D CLR^XUS2
 . I 'USERAC W $C(7),$$AVHLPTXT^XUS2(1) D AHELP^XUS2
 . I USERAC=-1 W $C(7),$$AVHLPTXT^XUS2(1),!
 . S USERAC=0
 . Q
 ;
 ; should not have been called from ^XUS2 at this point
 ;
 N ACD,XMB,XUU,XUH,XMB,XUEX
 I DA=DUZ S USERAC=$$TERR(1) G FOUT
 I X?.E1L.E S X=$$UP^XLFSTR(X)
 I X[$C(34)!(X[";")!(X["^")!(X[":")!(X'?6.20UNP) S USERAC=0
 I (X?6.20A)!(X?6.20N)!(X="MAIL-BOX") S USERAC=0
 I 'USERAC G FOUT
 S XUU=X,X=$$EN^XUSHSH(X),XUH=X
 S ACD=+$O(^VA(200,"A",XUH,0))
 I ACD S USERAC=$S(Z'=DA:$$TERR(2),1:$$TERR(3)) G FOUT
 ; no one has this access code
 S ACD=+$O(^VA(200,"AOLD",XUH,0))
 I 'ACD!IGN G FOUT
 S USERAC=$$TERR(4)
FOUT ;
 I 'VXUS,USERAC=1 S USERAC="1^"_XUU_U_XUH
 Q USERAC
 ;
VXCK(T) ; check if vxVistA system
 ; T - opt - additional check for existing of tag^routine
 N LGR,TEST,VX D SET
 S VX=$G(^%ZOSF("ZVX"))["VX" I VX,$G(T)'="" D
 . I T=1 S T="X^VFDXTX"
 . S VX=($T(@T)'="")
 . Q
 G OUT
 ;
VXNET(VI) ; check to see in external authentication enabled
 ; Return 1:vxVistA external authentication enabled
 ;        0:not enabled
 ; VI - opt - IEN to file 200 [ check field 21603.01 ]
 ;            If '$D(VI) then default VI=$G(DA)
 ;            Only check field 21603.01 if VI>0
 N LGR,TEST,VX D SET
 I '$D(VI) S VI=+$G(DA)
 S VX=$$VXCK S:VX VX=$$PARAM(1)
 I VX,VI,+$G(^VA(200,VI,21603)) S VX=0
 G OUT
 ;
XTX(CODE) ;
 ;;code;Supported API name;Boolean flag
 ;;  Boolean: 0:Do w/params, 1:Strictly ext. funct, 2:Either DO or EF
 ;;EXTA;AUTHENTICATE USER EXTERNALLY
 ;;
 ;;---;The items below should be deprecated as of 9/25/2013
 ;;ACE;ACCESS CODE EDIT
 ;;LST;SHARE LICENSE SLOTS (TASKMAN)
 ;;LSU;SHARE LICENSE SLOTS (USER)
 ;;VCE;VERIFY CODE EDIT
 ;;   ;CHANGE VERIFY CODE
 ;;   ;BROKER CHANGE VERIFY CODE
 ;;   ;AV HELP TEXT
 ;;
 N I,J,T,X,Y,Z
 F I=3:1 S Z=$P($T(XTX+I),";",3,5) Q:Z=""  D
 . S Y=$P(Z,";"),Z(Y)=$P(Z,";",2),Z(Y,0)=$P(Z,";",3)
 . Q
 S T=$S($G(CODE)="":"",1:$G(Z(CODE)))
 I T="" Q:$Q "" Q
 Q:$Q $$X^VFDXTX(T)
 D X^VFDXTX(T)
 Q
 ;
LICSHARE(T) ; attempt to reassign Cache license name
 ; T = LST or T = LSU
 ; Cache 2103 (perhaps in earlier versions) changed the way the Cache
 ; object script to reassign a process to a new license process name
 ; or ID.  Previously that license name was whatever you passed in.
 ; With the change, the Cache object script will ALWAYS append the
 ; IP address to the name passed in.  This hampered what vxVistA was
 ; trying to do which was to assign a Cache license name to a process
 ; based upon the VistA DUZ value.
 ; In the modifications prior to vxVistA 2013.0 many calls were routed
 ; through VFD^XUS.  With vxVistA 2013.0 the VFD^XUS module is moved to
 ; this routine.
 ; For description as to where to modify VistA for license sharing, see
 ; Mods file entries for: XUS,
