VFDXWB ;DSS/LM - RPC BROKER UTILITIES ; 09/30/2013 15:25
 ;;2011.1.2;DSS,INC VXVISTA OPEN SOURCE;;11 Jun 2013;Build 6
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
 ;  CALLED ENTRY POINTS
 ;----------------------  START BROKER LISTENER  ----------------------
STRT ; Option: VFD XWB START LISTENER
 ; also called from ^VFDVZST
 N I,R,X,Y,Z,PORT,TYPE
 S X=$$SELECT^VFDXWBB Q:X<9000
 S PORT=+X,TYPE=$P("OLD^NEW",U,1+$P(X,U,2))
 S R="STRT"_TYPE_"^VFDXWBB" D @R
 Q
 ;
STOP ; Option: VFD XWB STOP LISTENER
 ; also called from ^VFDVZST
 N I,X,Y,Z,IENS,PORT,TYPE
 S X=$$SELECT^VFDXWBB Q:X<9000
 S PORT=+X,TYPE=$P("OLD^NEW",U,1+$P(X,U,2)),IENS=$P(X,U,3)
 S R="STOP"_TYPE_"^VFDXWBB" D @R
 Q
 ;
 ;------------------------  RPC AUDIT SUPPORT  ------------------------ 
ARCHIVE ;[Option] VFD RPC AUDIT ARCHIVE
 ; Come here from tasked option to copy/purge VFD RPC AUDIT LOG
 D ARCHIVE^VFDXWBA
 Q
 ;
RPCLOG ; called from LOG^XWBDLOG
 D RPCLOG^VFDXWBA
 Q
 ;
 ;--------------------  CALLS FROM XWBTCPM ROUTINE  -------------------
PARM ; called from INIT, CLEANP
 D PARM^VFDXWBC
 Q
 ;
VLIST() ; called CONNTYPE
 Q $$VLIST^VFDXWBC
