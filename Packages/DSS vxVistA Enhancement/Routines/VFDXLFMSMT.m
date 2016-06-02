VFDXLFMSMT ;DSS/SMP - MEASUREMENT CONVERSIONS  ;03/26/2015 15:30
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**41**;07 Feb 2014;Build 8
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ; ***** This routine is only invoked via the VFDXLF routine *****
 ;
 ; ICR#  Use  Description
 ;-----  ---  ---------------------------------------------------------
 ;10143  Sup  ^XLFMSMT
 ;
IN2CM(LEN,DEC,FLG) ;
 S LEN=$$LENGTH(LEN,"in","cm",DEC)
 Q $S($G(FLAG):LEN_" CM",1:LEN)
 ;
IN2M(LEN,DEC,FLG) ;
 S LEN=$$LENGTH(LEN,"in","m",DEC)
 Q $S($G(FLAG):LEN_" M",1:LEN)
 ;
LB2KG(WT,DEC,FLAG) ;
 S WT=$$WEIGHT(WT,"lb","kg",DEC)
 Q $S($G(FLAG):WT_" KG",1:WT)
 ;
 ;=====================================================================
 ;                            C O M M O N
 ;=====================================================================
 ;
LENGTH(L,F,T,D) ;
 Q $P($FN($P($$LENGTH^XLFMSMT(L,F,T)," "),"T",D)," ")
 ;
WEIGHT(W,F,T,D) ;
 Q $P($FN($P($$WEIGHT^XLFMSMT(W,F,T)," "),"T",D)," ")
