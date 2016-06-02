VFDXLF ;DSS/SGM - MAIN ENTRY TO VFDXLF* ;02/04/2014 13:40
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**19,41**;07 Feb 2014;Build 8
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ;---------------------------------------------------------------------
 ;          CONVERT A PERSON'S NAME INTO VISTA STANDARD FORMAT
 ;RPC:                                          Extrinsic Function: Yes
 ;---------------------------------------------------------------------
NAMECNVT(VFDNAME,VFDIN,MAX,DELIM) ;
 D INIT("MAX^DELIM"),NAMECNVT^VFDXLFN1($Q)
 Q:$Q VFDNAME Q
 ; INPUT PARAMETERS
 ;  [.]VFDIN - req - see NAMECNVT^VFDXLFN1 for description
 ;     DELIM - opt - delimiter used to separate components in XLFNAME
 ;                   HL component type.  Defaults to "^"
 ;                   DELIM cannot be ',' or ' '
 ;       MAX - opt - max number of characters in a name or component.
 ;                   VistA limits value to a max of 256.
 ;                   Defaults to 256.
 ;
 ; RETURN: VFDNAME
 ; If called as an Extrinsic Function, then return
 ;    VFDNAME & $$NAMECNVT - VistA_Standardized_Name -OR- -1^msg
 ;    VFDNAME(key_term)    - value(s)
 ;
 ; If called as RPC or by D w/params, then return
 ;    VFDNAME(j) = key_term ^ value   for j=1,2,3,4,5,...
 ;
 ;---------------------------------------------------------------------
 ;           VALIDATE DD#, FIELD#, IENS IS POINTER TO FILE 20
 ;RPC:                                          Extrinsic Function: Yes
 ;---------------------------------------------------------------------
VALID(VFDA,VFILE,VFLD,VIENS) ;
 D INIT("VFILE^VFLD^VIENS")
 S VFDA=$$VALID^VFDXLFN1()
 Q:$Q VFDA Q
 ; Validate that ^DD(VFILE,VFLD,0) is a pointer field to file 20
 ; AND validate that VIENS is an actual record in that file
 ; INPUT PARAMETERS
 ;  VFILE - req - DD# or sub-DD#
 ;   VFLD - req - Field# at that VFILE level
 ;  VIENS - req - Valid IENS for record at VFILE,VFLD level
 ; RETURN: VFDA = 1 if valid, else -1^message
 ;
 ;---------------------------------------------------------------------
 ; Convert External Date/Time to FileMan Date/Time
 ;  **See VFDXLFDT for acceptable formats
 ;API:                                          Extrinsic Function: Yes
 ;---------------------------------------------------------------------
DT(X) ; VALIDATE AND ATTEMPT TO RETURN A VALID DATE
 Q $$DT^VFDXLFDT(X)
 ;
 ;---------------------------------------------------------------------
 ;Convert Inches to Centimeters
 ;Extrinsic Function: Yes
 ;---------------------------------------------------------------------
IN2CM(LEN,DEC,FLAG) ; Convert inches to meters
 ; INPUT PARAMTERS
 ;  LEN - req - Length in inches to be converted to meters
 ;  DEC - opt - # of decimal points to return, defaults to 4
 ; FLAG - opt - Boolean, I FLAG return WT_" m"
 ;                       I 'FLAG return +WT
 ;                       Defaluts to 0
 Q:'$G(LEN) ""
 Q $$IN2CM^VFDXLFMSMT(LEN,$G(DEC,4),$G(FLAG))
 ;
 ;---------------------------------------------------------------------
 ;Convert Inches to Meters
 ;Extrinsic Function: Yes
 ;---------------------------------------------------------------------
IN2M(LEN,DEC,FLAG) ; Convert inches to meters
 ; INPUT PARAMTERS
 ;  LEN - req - Length in inches to be converted to meters
 ;  DEC - opt - # of decimal points to return, defaults to 4
 ; FLAG - opt - Boolean, I FLAG return WT_" m"
 ;                       I 'FLAG return +WT
 ;                       Defaluts to 0
 Q:'$G(LEN) ""
 Q $$IN2M^VFDXLFMSMT(LEN,$G(DEC,4),$G(FLAG))
 ;
 ;---------------------------------------------------------------------
 ;Convert Pounds to Kilograms
 ;Extrinsic Function: Yes
 ;---------------------------------------------------------------------
LB2KG(WT,DEC,FLAG) ; Convert pounds to kilograms
 ; INPUT PARAMTERS
 ;   WT - req - Weight in pounds to be converted to kilograms
 ;  DEC - opt - # of decimal points to return, defaults to 4
 ; FLAG - opt - Boolean, I FLAG return WT_" kg"
 ;                       I 'FLAG return +WT
 ;                       Defaluts to 0
 ;
 Q:'$G(WT) ""
 Q $$LB2KG^VFDXLFMSMT(WT,$G(DEC,4),$G(FLAG))
 ;
 ;
 ;---------------------------------------------------------------------
 ;                        PRIVATE SUBROUTINES
 ;---------------------------------------------------------------------
INIT(STR) ;
 N I,X F I=1:1:$L(STR,U) S X=$P(STR,U,I) S:X'="" @(X_"=$G(@X)")
 Q
