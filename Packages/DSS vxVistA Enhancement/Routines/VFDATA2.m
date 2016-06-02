VFDATA2 ;DSS/SMP - Data Table Utilities ; 11/03/2015 15:00
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;**26**;01 Dec 2009;Build 1
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
 ;
EXPORT(VFD,PATH) ; Export Table to HFS
 N X,Y,R,C,DATA,DELIM,ERR,FILE,SRC
 S NA=$NA(^TMP("VFDATA",$J,"DATA"))
 K ^TMP("VFDATA",$J)
 D GETS^DIQ(21611,VFD,".01;.05;.06;2*;3*;4*",,NA,"VFDERR")
 S DELIM=$G(@NA@(21611,VFD_",",.06)) I DELIM="" S DELIM=$C(9)
 I DELIM["$C(" S DELIM=$C(+$P(DELIM,"(",2))
 S SRC=$G(@NA@(21611,VFD_",",.05)) I SRC'="" S SRC="_"_SRC
 S FILE=@NA@(21611,VFD_",",.01)_SRC_".txt"
 D ROWH,COLH
 S NA=$NA(@NA@(21611.04))
 S X=0 F  S X=$O(@NA@(X)) Q:X=""  D
 .S R=$G(@NA@(X,.01)) Q:'R
 .S C=$G(@NA@(X,.02)) Q:'C
 .S DATA=$G(@NA@(X,.03))
 .D SET(R,C,DATA)
 I '$$GTF^%ZISH($NA(^TMP("VFDATA",$J,"OUTPUT",1)),4,PATH,FILE) D
 .W "Error!"
 E  W !!,FILE_" successfully written.",!
 K ^TMP("VFDATA",$J)
 Q
 ;
COLH ; Column Headers
 N X,Y,C,DATA,REF
 S REF=$NA(@NA@(21611.03))
 Q:'$D(@REF)
 S X=0 F  S X=$O(@REF@(X)) Q:X=""  D
 .S C=$G(@REF@(X,.01)) Q:'C
 .S DATA=$G(@REF@(X,.02)) Q:DATA=""
 .D SET(1,C,DATA)
 Q
 ;
ROWH ; Row Headers
 N X,Y,R,DATA,REF
 S REF=$NA(@NA@(21611.02))
 Q:'$D(@REF)
 S X=0 F  S X=$O(@REF@(X)) Q:X=""  D
 .S R=$G(@REF@(X,.01)) Q:'R
 .S DATA=$G(@REF@(X,.02)) Q:DATA=""
 .D SET(R,1,DATA)
 Q
 ;
SET(R,C,DATA) ; Set up TABLE
 S $P(^TMP("VFDATA",$J,"OUTPUT",R),DELIM,C)=DATA
 Q
