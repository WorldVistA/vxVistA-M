VFDI0010 ;DSS/SGM - ENV/PRE/POST VFD*2011.1.3*21 ; 12/30/2013 09:30
 ;;2011.1.3;DSS,INC VXVISTA OPEN SOURCE;**21**;16 Aug 2013;Build 3
 ;Copyright 1995-2013,Document Storage Systems Inc. All Rights Reserved
 ;
 ;
 ;ICR #  SUPPORTED DESCRIPTION
 ;-----  --------------------------------------------
 ;
 ;
ENV ; environmental check for VFD*2011.1.3*21
 Q
 ;
PRE ; pre-install for VFD*2011.1.3*21
 Q
 ;
POST ; post-install for VFD*2011.1.3*21
 ; Remove CLINIC from VFD PSO PROCESSING OPTION parameter setting
 N X,Y,Z,PARAM,VFDRET
 S PARAM="VFD PSO PROCESSING OPTION"
 K ^TMP("VFDC",$J),^TMP("VFDCX",$J)
 D ADD^VFDCXPR(.VFDRET,"SYS~"_PARAM_"~1~1;0;0")
 D GETALL^VFDCXPR(.VFDRET,"~"_PARAM_"~1")
 S X="" F  S X=$O(^TMP("VFDCX",$J,X)) Q:X=""  D
 .N DATA,VALUE S VALUE=^TMP("VFDCX",$J,X,1) Q:$L(VALUE,";")'=4
 .S DATA=X_"~"_PARAM_"~1~"_$P(VALUE,";",1,2)_";"_$P(VALUE,";",4)
 .D CHG^VFDCXPR(.VFDRET,DATA)
 K ^TMP("VFDC",$J),^TMP("VFDCX",$J)
 Q
 ;
 ;=====================================================================
 ;
