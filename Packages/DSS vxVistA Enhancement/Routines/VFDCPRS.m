VFDCPRS ;DSS/LM - Miscellaneous vxCPRS support ;September 9, 2009
 ;;2013.1;DSS,INC VXVISTA OPEN SOURCE;**21**;28 Jan 2013;Build 3
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
VER(VFDRSLT,VFDCLVER) ;[RPC] VFD CPRS CLIENT VERSIONS
 ; VFDCLVER=[Required] Client version ID
 ; 
 ; Returns 1=TRUE (or YES) if and only if client version instance of
 ; parameter VFD CPRS CLIENT VERSIONS is valued 1=TRUE (or YES)
 ; 
 ; This RPC also sets application-wide variable ORWCLVER equal to
 ; the corresponding VA CPRS version ID, if the client version is
 ; supported.
 ;
 S VFDCLVER=$G(VFDCLVER)
 S VFDRSLT=+$$GET^XPAR("SYS","VFD CPRS CLIENT VERSIONS",VFDCLVER,"Q")
 S:VFDRSLT ORWCLVER=$P(VFDCLVER,".")_".0."_$P(VFDCLVER,".",2,3)
 Q
 ;
TABS(VFDRET) ; [RPC] VFD CPRS TABS SHOW
 D TABS^VFDCPRS1
 Q
 ;
LANG(VFDRSLT,DFN) ; [RPC] VFD LANG GET
 ; DFN=[Required] numeric IEN of patient
 ; KT-20150111
 ; 21601.02=Preferred Language; 21601.05=Needs Interpreter
 N VFDERR,VFDLANG
 S VFDLANG=$$GET1^DIQ(2,DFN_",",21601.02,,,$NA(VFDERR))
 S VFDRSLT=$S($L(VFDLANG):VFDLANG_U_$$GET1^DIQ(2,DFN_",",21601.05,,,$NA(VFDERR)),1:U)
 Q
