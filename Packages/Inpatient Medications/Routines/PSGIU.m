PSGIU ;BIR/CML3-GENERIC "APPLICATION PACKAGES' USE" SET ;16 SEP 97 / 9:22 AM
 ;;5.0; INPATIENT MEDICATIONS ;;16 DEC 97;Build 1
 ;
EN ;
 Q:$S('$D(PSIUDA):1,'$D(PSIUX):1,PSIUX'?1E1"^"1.E:1,1:'$D(^PSDRUG(PSIUDA,0)))  S PSIUO=$P($G(^(2)),"^",3) S PSIUT=$P(PSIUX,"^",2),PSIUT=$S($E(PSIUT,1,4)="UNIT":"",1:$E("N","AEIOU"[$E(PSIUT)))_" "_PSIUT,(%,PSIUQ)=PSIUO'[$E(PSIUX)+1
 F  W !!,"A",PSIUT," ITEM" D YN^DICN Q:%  D MQ S %=PSIUQ
 I %<0 S PSIUA="^" G DONE
 S PSIUA=$E("YN",%) G:%=PSIUQ DONE I %=1 S PSIUY=PSIUO_$P(PSIUX,"^"),$P(^PSDRUG(PSIUDA,2),"^",3)=PSIUY I $P(^(0),"^")]"" S ^PSDRUG("AIU"_$P(PSIUX,"^"),$P(^(0),"^"),PSIUDA)=""
 I %=2 S PSIUY=$P(PSIUO,$P(PSIUX,"^"))_$P(PSIUO,$P(PSIUX,"^"),2),$P(^PSDRUG(PSIUDA,2),"^",3)=PSIUY I $P(^(0),"^")]"" K ^PSDRUG("AIU"_$P(PSIUX,"^"),$P(^(0),"^"),PSIUDA)
 K:PSIUO]"" ^PSDRUG("IU",PSIUO,PSIUDA) S:PSIUY]"" ^PSDRUG("IU",PSIUY,PSIUDA)=""
 ;
DONE ;
 K PSIU,PSIUO,PSIUQ,PSIUT,PSIUY Q
 ;
MQ ;
 S X="Enter 'YES' (or 'Y') to mark this drug as a"_$S($E(PSIUT,1,2)="N ":"n"_$E(PSIUT,2,99),1:PSIUT)_" item.  Enter 'NO' (or 'N') to not mark (or unmark) this drug."
 W !!?2 F PSIU=1:1:$L(X," ") S Y=$P(X," ",PSIU) W:$X+$L(Y)>79 ! W Y," "
 Q
 ;
ENS ;
 Q:$S('$D(PSIUDA):1,'$D(PSIUX):1,'PSIUDA:1,$L($P(PSIUX,"^"))'=1:1,1:'$D(^PSDRUG(PSIUDA,0)))  S PSIU=$P(^(0),"^"),(PSIUO,PSIUY)=$P($G(^(2)),"^",3),PSIUT=$P(PSIUX,"^")
 I PSIUY'[PSIUT S PSIUY=PSIUY_PSIUT,$P(^PSDRUG(PSIUDA,2),"^",3)=PSIUY K:PSIUO]"" ^PSDRUG("IU",PSIUO,PSIUDA)
 S ^PSDRUG("IU",PSIUY,PSIUDA)="" I PSIU]"" S ^PSDRUG("AIU"_PSIUT,PSIU,PSIUDA)=""
 ;DSS/SMP - Begin Mod
 I $G(^%ZOSF("ZVX"))["VX" D VFD
 ;DSS/SMP - End Mod
 G DONE
 ;
END ;
 Q:$S('$D(PSIUDA):1,'$D(PSIUX):1,'PSIUDA:1,$L($P(PSIUX,"^"))'=1:1,1:'$D(^PSDRUG(PSIUDA,0)))  S PSIU=$P(^(0),"^"),(PSIUO,PSIUY)=$P($G(^(2)),"^",3),PSIUT=$P(PSIUX,"^")
 I PSIUY[PSIUT S PSIUY=$P(PSIUY,PSIUT)_$P(PSIUY,PSIUT,2),$P(^PSDRUG(PSIUDA,2),"^",3)=PSIUY K ^PSDRUG("IU",PSIUO,PSIUDA)
 S:PSIUY]"" ^PSDRUG("IU",PSIUY,PSIUDA)="" I PSIU]"" K ^PSDRUG("AIU"_PSIUT,PSIU,PSIUDA)
 ;DSS/SMP - Begin Mod
 I $G(^%ZOSF("ZVX"))["VX" D VFD1
 ;DSS/SMP - End Mod
 G DONE
 ;
VFD ; Called from ENS - MKN/TFF
 ; If all the file #50 entries pointed to by the file #101.43 that is
 ; pointed to  by *THIS* drug (PSIUDA) have the same Unit Dose flag 
 ; status, then update the file #101.43 entry 
 ;
 N VFD507,VFD10143,VFDFDA,VFDERR
 S VFD507=$P($G(^PSDRUG(PSIUDA,2)),U) I 'VFD507 G DONE
 ; Now set the MEMBERSHIP SET "UD RX" in field #9 of file #101.43
 S VFD10143=$O(^ORD(101.43,"ID",VFD507_";99PSP","")) I 'VFD10143 G DONE
 I $O(^ORD(101.43,VFD10143,9,"B","UD RX","")) G DONE   ; Only add it if new
 K VFDFDA S VFDFDA(101.439,"+1,"_VFD10143_",",.01)="UD RX"
 D UPDATE^DIE(,"VFDFDA",,"VFDERR")
 Q
 ;
VFD1 ; Called from END - MKN/TFF
 ; If all the file #50 entries pointed to by the file #101.43 that is
 ; pointed to  by *THIS* drug (PSIUDA) have the same Unit Dose flag
 ; status, then update the file #101.43 entry
 ;
 N VFD50,VFD507,VFD10143,VFDDO,VFDFDA,VFDERR,DIK,DA
 S VFDDO=1,VFD507=$P($G(^PSDRUG(PSIUDA,2)),U) I 'VFD507 G DONE
 ; Find all file #50 entries associated with the file #50.7 entry
 ; pointed to by this file #50 entry (but exclude this file #50 entry)
 S VFD50=0 F  S VFD50=$O(^PSDRUG("ASP",VFD507,VFD50)) Q:'VFD50  I PSIUDA'=VFD50 D
 . ;Is this file #50 entry also "marked"
 . I $P($G(^PSDRUG(VFD50,2)),U,3)["U" S VFDDO=0 Q
 G:'VFDDO DONE ; Not all the file #50 entries are marked
 ; Now DELETE the MEMBERSHIP SET "UD RX" from field #9 of file #101.43
 S VFD10143=$O(^ORD(101.43,"ID",VFD507_";99PSP",""))
 I VFD10143 S VFD9=$O(^ORD(101.43,VFD10143,9,"B","UD RX","")) I VFD9 D
 . S DA(1)=VFD10143,DA=VFD9,DIK="^ORD(101.43,"_VFD10143_",9," D ^DIK
 Q
 ;
 ;;2013.1;OPEN SOURCE - DOCUMENT STORAGE SYS;;20 Mar 2015;PATCH 35
