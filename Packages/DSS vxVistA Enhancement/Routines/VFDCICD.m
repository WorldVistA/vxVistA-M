VFDCICD ;DSS/SMP - ICD9/ICD10 UTILITIES ;09/16/2015 16:50
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**2**;28 Jan 2013;Build 3
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;  this contains RPC calls for retrieving ICD9 information
 ;  It is code set versioning compliant in that it looks for
 ;  the ICDCODE routine.
 ;
 ;  DBIA#  SUPPORTED
 ;  -----  ---------  --------------------------------------------------
 ;   2056      x      GETS^DIQ
 ;   3990      x      ICDCODE: $$ICDDX, $$CODEN, $$CODEC
 ;   3991      x      ICDAPIU: $$STATCHK
 ;  10082      x      Direct global/FM read of file 80 of entire 0th
 ;                    node, fields .01;3;5;9.5;100;102;10, & "BA" index
 ;  10096      x      All nodes in ^%ZOSF are uesable
 ;  10103      x      $$FMTE^XLFDT
 ;  10104      x      ^XLFSTR: $$TRIM, $$UP
 ;
 ;  Common notation on input parameters
 ;   CDT - opt - default to DT
 ;         date used by ICD code to return date specific data
 ;   FUN - opt - I +$G(FUN) then extrinisc function call
 ;                    do not return data in local array
 ;  VICD - req - ien to file 80 or ICD9 name
 ;   SRC - opt - Flag to indicate if Level III codes need to be screened
 ;               out. If SRC=0 or null, Level III codes not processed as
 ;               valid input; if SRC>0, Level III codes are accepted.
 ;
ICDGET(RET,VFD) ; RPC - [VFDC ICD GET]
 ; INPUT(n) - req where INPUT(n) = code ^ value for n=1,2,3,...
 ; 
 ; CODE    REQ VALUE
 ; ------- --- -------------------------------------------------------
 ; LOOKUP   Y  ICD Code (Can be multiple LOOKUPs)
 ; CDT      N  Date to compare code against.  DEFAULTS to TODAY
 ; FILTER   N  ICD Type e.g., 9~10~11. DEFAULTS to ALL
 ;
 ; OUTPUT: 
 ;   RET(n) = 80 IEN ^ ICD Code ^ ICD Type ^ Short Desc ^ Active
 ;             - OR -
 ;            -1 ^ Lookup Value ^ ^ ^
 ;    N = 1,2,3
 ;
 N X,Y,Z,CDT,CNT,FILTER,LOOKUP S CNT=1
 S X="" F  S X=$O(VFD(X)) Q:X=""  S Y=VFD(X) D
 .I $P(Y,U)="LOOKUP" S LOOKUP($P(Y,U,2))="",LOOKUP=1+$G(LOOKUP) Q
 .S @($P(Y,U))=$P(Y,U,2)
 I '$G(CDT) S CDT=DT
 I '$D(FILTER) S FILTER="9~10"
 I '$G(LOOKUP) S RET(0)="-1^No lookup value passed"
 S X="" F  S X=$O(LOOKUP(X)) Q:X=""  D
 .N ICD9,ICD10,QUIT
 .I FILTER[9 D  Q:$G(QUIT)
 ..S ICD9=$$ICDDX^ICDEX(X,CDT,1)
 ..I +ICD9>0 D ADD($$ICD(ICD9,9,CDT)) S QUIT=1
 .I FILTER[10 D  Q:$G(QUIT)
 ..S ICD10=$$ICDDX^ICDEX(X,CDT,30)
 ..I +ICD10>0 D ADD($$ICD(ICD10,10,CDT)) S QUIT=1
 .D ADD("-1^"_X_"^^^")
 Q
 ;
ICD(ICD,TYPE,CDT) ; Get Data from File 80
 N ACT S ACT=+$$SAI^ICDEX(80,+ICD,CDT)
 ;S ACT=$S(ACT'="":1,1:0)
 Q $P(ICD,U,1,2)_U_TYPE_U_$P(ICD,U,4)_U_ACT
 ;
ADD(STR) ;
 S RET(CNT)=STR,CNT=CNT+1
 Q
 ;
