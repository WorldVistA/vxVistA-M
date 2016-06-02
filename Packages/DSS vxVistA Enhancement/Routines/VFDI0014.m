VFDI0014 ;DSS/SMP - ENV/PRE/POST VXVISTA 2014.0 ; 06/23/2014 12:25
 ;;2014.0;DSS,INC VXVISTA OPEN SOURCE;;11 Jun 2013;Build 18
 ;Copyright 1995-2014,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
V ;;2014.0;;;;
 Q
 ;
 ; This is the entry point for the environmental check for vxVistA
 ; 2014.0 and the corresponding GUIs for vxCPRS, vxVitals, vxBCMA
 ;
 ;ICR #  SUPPORTED DESCRIPTION
 ;-----  --------------------------------------------
 ;
 ;
ENV ; environmental check for vxVistA 2014.0
 Q
 ;
PRE ; pre-install for vxVistA 2014.0
 K XUMF,VFDIDUZ S XUMF=1 I $G(DUZ(0))'="@" M VFDIDUZ=DUZ S DUZ(0)="@"
 Q
 ;
PREBCMA ; pre-install for vxBCMA 2014.0
 Q
 ;
PRECPRS ; pre-install for vxCPRS 2014.0
 Q
 ;
PREGMV ; pre-install for vxVitals 2014.0
 Q
 ;
POST ; post-install for vxVistA 2014.0
 D POST1,POST2,POST3,POST4 ; 4/28/2014
 K XUMF I $D(VFDIDUZ) K DUZ M DUZ=VFDIDUZ K VFDIDUZ
 Q
 ;
POSTBCMA ; post-install for vxBCMA 2014.0
 Q
 ;
POSTCPRS ; post-install for vxCPRS 2014.0
 Q
 ;
POSTGMV ; post-install for vxVitals 2014.0
 Q
 ;
 ;=====================================================================
 ;                        P R E - I N S T A L L
 ;=====================================================================
 ;
PRE1 ;
 Q
 ;
PRE2 ;
 Q
 ;
 ;=====================================================================
 ;                       P O S T - I N S T A L L
 ;=====================================================================
 ;
POST1 ; update the vxVistA Version Parameter
 N I,J,X,Y,Z,VER
 I $G(^%ZOSF("ZVX"))="" S ^%ZOSF("ZVX")="VXOS"
 S VER=$P($T(V),";",3) I VER>0 D CHG^VFDCXPR(,"SYS~VFD VXVISTA VERSION~1~"_VER)
 Q
 ;
POST2 ; update the PSB PATIENT ID LABEL to MRN
 N I,J,X,Y,Z
 D CHG^VFDCXPR(,"SYS~PSB PATIENT ID LABEL~1~MRN")
 Q
 ;
POST3 ; post-install
 N I,J,X,Y,RPC,SUP,VFDATA,VFDMES,VXS
 ; add RPCs to menu context(s) if they are missing
 ;   DSIC rpcs will not exist in vxVistA OS
 F I=1:1 S X=$P($T(@("T"_I)),";",3) Q:X=""  D
 . F J=1:1 S Y=$P($T(@("T"_I_"+"_J)),";;",2,99) Q:Y=""  D
 . . S SUP=$P(Y,";")="S",RPC=$P(Y,";")
 . . I RPC'="" S VFDATA(X,RPC)=""
 . . Q
 . Q
 D ADDRPC^VFDI0000(.VFDMES,,,.VFDATA)
 Q
 ;
POST4 ; 
 D PUT^XPAR("SYS","VFD PSO BYPASS PKI",1,1)
 Q
 ;
 ;-----------------------  PRIVATE SUBROUTINES  -----------------------
 ; T# line label ;;<name of OPTION to add RPCs to>
 ; T#+offset ;;<<S>;name of RPC to add
 ;     ;S should only be set if this RPC is for supported accounts only
 ; 
T1 ;;VFDVX CPRS GUI CHART
 ;;ORDEA CSVALUE
 ;;ORDEA DEATEXT
 ;;ORDEA HASHINFO
 ;;ORDEA LNKMSG
 ;;ORDEA ORDHINFO
 ;;ORDEA PINLKCHK
 ;;ORDEA PINLKSET
 ;;ORDEA PNDHLD
 ;;ORDEA SIGINFO
 ;;ORQQPL4 LEX
 ;;ORWPCE ICDVER
 ;;ORWPCE4 LEX
 ;;XUS PKI GET UPN
 ;;XUS PKI SET UPN
 ;;VFDC XPAR GETWP
 ;;
T2 ;;VFDVX GMV V/M GUI
 ;;GMV CLOSEST READING
 Q
 ;
 ; 
 ;---------------------------------------------------------------------
DATA(TAG,NOP) ; Process $T() lines generically
 N I,J,X,Y,Z
 S Z=$P($T(@TAG),";",3,999) K VFDATA Q:Z=""
 S J=0 I $G(NOP) S VFDATA(1)=Z
 E  F I=1:1:$L(Z,U) S X=$P(Z,U,I) S:$L(X) J=J+1,VFDATA(J)=X
 Q $D(VFDATA)
