VFDI0006 ;DSS/SGM - ENV/PRE/POST VXVISTA 2013.1 ; 6/11/2013 18:22
V ;;2013.1;;;;Build 15
 ;
 ; This is the entry point for the environmental check for vxVistA
 ; 2013.1 and the corresponding GUIs for vxCPRS, vxVitals, vxBCMA
 ;
MIN ;;2013.0
MAX ;;2013.1
 ;---------------------------------------------------------------------
ENVVER ; environmental check
 ; check for min/max vxvista version
 N MAX,MIN
 S MIN=$P($T(MIN),";",3),MAX=$P($T(MAX),";",3)
 I $T(VERCK^VFDI0000)="" S XPDABORT=1
 E  I $$VERCK^VFDI0000(,MIN,MAX,1)'>0 S XPDABORT=1
 ;
ENV ; environmental check for vxVistA 2013.1
 Q
 ;
PRE ; pre-install for vxVistA 2013.1
 Q
 ;
PREBCMA ; pre-install for vxBCMA 2013.1
 Q
 ;
PRECPRS ; pre-install for vxCPRS 2013.1
 Q
 ;
PREGMV ; pre-install for vxVitals 2013.1
 Q
 ;
POST ; post-install for vxVistA 2013.1
 D VER
 Q
 ;
POSTBCMA ; post-install for vxBCMA 2013.1
 Q
 ;
POSTCPRS ; post-install for vxCPRS 2013.1
 Q
 ;
POSTGMV ; post-install for vxVitals 2013.1
 Q
VER ; update the vxVistA Version Number Parameter
 N X,DATA,VER
 S VER=$P($T(V),";",3)
 S DATA("VFD VXVISTA VERSION","SYS",1,1)=U_VER_"^CHG"
 D EN^VFDXTPAR(,"CHG",.DATA)
 Q
