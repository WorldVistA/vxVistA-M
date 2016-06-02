VFDXTFND ;DSS/SMP - FIND WHEN COMPONENTS INSTALELD; 05/11/2015 13:40
 ;;15.0;DSS,INC VXVISTA OPEN SOURCE;;15 Sep 2015;Build 29
 ;Copyright 1995-2015,Document Storage Systems Inc. All Rights Reserved
 ;
 Q
 ;
EN ; option VFD IT FIND INSTALL HISTORY
 N X,Y,Z,COMP,FILE,FLAG
 F  D  Q:$G(FLAG)
 .S FILE=$$DIC(1,1) I FILE<0 S FLAG=1 Q
 .F  D  Q:$G(FLAG)
 ..N RPT,NOTE
 ..S COMP=$$DIC(+FILE) I COMP<0 S FLAG=1 Q
 ..D FIND(+FILE,$P(COMP,U,2)),RPT W !!!!
 .S FLAG=0
 Q
 ;
FIND(FILE,COMP) ; Find builds that export the component
 N X,Y,Z,NM
 S X=0 F  S X=$O(^XPD(9.6,X))  Q:'X  S NM=$P(^(X,0),U)  D
 .N ACT,IEN,INST
 .Q:'$D(^XPD(9.6,X,"KRN",FILE,"NM","B",COMP))
 .S IEN=$O(^XPD(9.6,X,"KRN",FILE,"NM","B",COMP,0))
 .S ACT=$P(^XPD(9.6,X,"KRN",FILE,"NM",IEN,0),U,3)
 .S INST=$$INST(NM) Q:'INST  I INST["*" S NOTE=1
 .S RPT(+INST,NM)=NM_U_$$ACT(ACT)_U_$$FMTE^XLFDT(+INST)_$S(INST["*":"*",1:"")
 Q
 ;
RPT ; Print report
 N X,Y
 I '$D(RPT) W "No matches found",! Q
 W "Build",?30,"Action",?55,"Install D/T",!,$$REPEAT^XLFSTR("-",79),!
 S X="RPT" F Y=1:1 S X=$Q(@X) Q:X=""  D
 .W $P(@X,U),?30,$P(@X,U,2),?55,$P(@X,U,3),!
 Q:'$G(NOTE)
 W !!,"NOTE:  An asterisk next to Install D/T indicates that a build was insatlled",!
 W "       more than once.  The date listed is the most recent install.",!!
 Q
 ;
 ;
DIC(FILE,SCR) ; DIC Lookup
 N X,Y,DIC,DTOUT,DUOUT
 S DIC=FILE,DIC(0)="AEQO"
 I $G(SCR) S DIC("S")=$P($T(SCR+SCR),";",3,99)
 D ^DIC W !!
 I $D(DUOUT)!$D(DTOUT)!(X="") Q -1
 I "^.4^.401^.402^.403^"[(U_FILE_U) S Y=Y_"    FILE #"_$$GETFILE(FILE,+Y)
 Q Y
 ;
SCR ; DIC Screen
 ;;I "^.4^.401^.402^.403^.5^.84^3.6^3.8^9.2^9.8^19^19.1^101^409.61^771^779.2^870^8989.51^8989.52^8994^"[(U_Y_U)
 Q
 ;
ACT(X) ; Action
 ;'0' FOR SEND TO SITE;
 ;'1' FOR DELETE AT SITE;
 ;'2' FOR USE AS LINK FOR MENU/ITEM/SUBSCRIBERS;
 ;'3' FOR MERGE MENU ITEMS;
 ;'4' FOR ATTACH TO MENU;
 ;'5' FOR DISABLE DURING INSTALL;
 I X=0 Q "Send to Site"
 I X=1 Q "Delete at Site"
 I X=2 Q "Use as Link for Menu/Item/Subscribers"
 I X=3 Q "Merge Menu Items"
 I X=4 Q "Attach to Menu"
 I X=5 Q "Disable During Install"
 Q "Error"
 ;
INST(NAME) ; Install D/T
 N I,X,Y,MULT,RET,VFD,VFDERR
 S RET=""
 D LIST^DIC(9.7,,"@;.01;17","PI",,,NAME,,,,"VFD","VFDERR")
 I $D(VFDERR)!'$D(VFD) Q RET
 I +VFD("DILIST",0)>1 S MULT=1
 F I=1:1:+VFD("DILIST",0) D
 .Q:$P(VFD("DILIST",I,0),U,2)'=NAME
 .S X=$P(VFD("DILIST",I,0),U,3)
 .I X>RET S RET=X
 Q RET_$S($G(MULT):"*",1:"")
 ;
GETFILE(FILE,IEN) ;
 I FILE=.403 Q $$GET1^DIQ(.403,IEN,7,"I")
 Q $$GET1^DIQ(FILE,IEN,4,"I")
