ESPORM1 ;DALISC/CKA - OFFENSE REPORT IN MAIL MESSAGE CONT. ;3/93
 ;;1.0;POLICE & SECURITY;;Mar 31, 1994
EN ;
COMP ;PRINT COMPLAINANTS
 D HDR:$Y+5>IOSL
 F ESPN=0:0 S ESPN=$O(^ESP(912,ESPID,20,ESPN)) Q:ESPN'>0  D
 .  S DIC="^ESP(912,"_ESPID_",20,",DA=ESPN,DR=".02;.03;1.01:1.06;2.01:2.06",DIQ(0)="E" D EN^DIQ1 Q:'$D(^UTILITY("DIQ1",$J,912.03,DA))
 .  S ESPX=" " D MSG F I=1:1:16 S ESPX=ESPX_"* "
 .  S ESPX=ESPX_"COMPLAINANT  DATA" F I=1:1:15 S ESPX=ESPX_" *"
 .  D MSG
 .  S ESPX=" " D MSG S ESPX="COMPLAINANT NAME: "_$G(^UTILITY("DIQ1",$J,912.03,DA,.02,"E")) D MSG
 .  S ESPX="STATUS: "_$G(^UTILITY("DIQ1",$J,912.03,DA,.03,"E")) D MSG
 .  S ESPX="HOME ADDRESS: "_$G(^UTILITY("DIQ1",$J,912.03,DA,1.01,"E")) D MSG
 .  I $G(^UTILITY("DIQ1",$J,912.03,DA,1.02,"E"))]"" S ESPX=$E(SPACES,1,14)_^("E") D MSG
 .  I $G(^UTILITY("DIQ1",$J,912.03,DA,1.03,"E"))]"" S ESPX=$E(SPACES,1,14)_^("E")_","
 .  S ESPX=ESPX_$G(^UTILITY("DIQ1",$J,912.03,DA,1.04,"E"))_" "_$G(^UTILITY("DIQ1",$J,912.03,DA,1.05,"E")) D MSG
 .  S ESPX="HOME PHONE: "_$G(^UTILITY("DIQ1",$J,912.03,DA,1.06,"E")) D MSG
 .  S ESPX="WORK ADDRESS: "_$G(^UTILITY("DIQ1",$J,912.03,DA,2.01,"E")) D MSG
 .  I $G(^UTILITY("DIQ1",$J,912.03,DA,2.02,"E"))]"" S ESPX=$E(SPACES,1,14)_^("E") D MSG
 .  I $G(^UTILITY("DIQ1",$J,912.03,DA,2.03,"E"))]"" S ESPX=$E(SPACES,1,14)_^("E")_","
 .  S ESPX=ESPX_$G(^UTILITY("DIQ1",$J,912.03,DA,2.04,"E"))_" "_$G(^UTILITY("DIQ1",$J,912.03,DA,2.05,"E")) D MSG
 .  S ESPX="WORK PHONE: "_$G(^UTILITY("DIQ1",$J,912.03,DA,2.06,"E")) D MSG
VIC ;PRINT VICTIMS
 G:'$D(^ESP(912,ESPID,30)) CONT
 D HDR:$Y+5>IOSL
 F ESPN=0:0 S ESPN=$O(^ESP(912,ESPID,30,ESPN)) Q:ESPN'>0  D
 .  S DIC="^ESP(912,"_ESPID_",30,",DA=ESPN,DR=".02:.07;1.01:1.06;2.01:2.06",DIQ(0)="E" D EN^DIQ1 Q:'$D(^UTILITY("DIQ1",$J,912.04,DA))
 .  S ESPX=" " D MSG F I=1:1:17 S ESPX=ESPX_"* "
 .  S ESPX=ESPX_"VICTIM DATA"
 .  F I=1:1:17 S ESPX=ESPX_" *"
 .  D MSG
 .  S ESPX=" " D MSG S ESPX="VICTIM NAME: "_$G(^UTILITY("DIQ1",$J,912.04,DA,.02,"E")) D MSG
 .  S ESPX="SEX: "_$G(^UTILITY("DIQ1",$J,912.04,DA,.03,"E"))
 .  S ESPX=$E(SPACES,1,20)_"RACE: "_$G(^UTILITY("DIQ1",$J,912.04,DA,.04,"E")) D MSG
 .  S ESPX="STATUS: "_$G(^UTILITY("DIQ1",$J,912.04,DA,.05,"E")) D MSG
 .  S ESPX="DRIVER'S LICENSE & STATE: "_$G(^UTILITY("DIQ1",$J,912.04,DA,.06,"E"))_" "_$G(^UTILITY("DIQ1",$J,912.04,DA,.07,"E")) D MSG
 .  S ESPX="HOME ADDRESS: "_$G(^UTILITY("DIQ1",$J,912.04,DA,1.01,"E")) D MSG
 .  I $G(^UTILITY("DIQ1",$J,912.04,DA,1.02,"E"))]"" S ESPX=$E(SPACES,1,14)_^("E")
 .  I $G(^UTILITY("DIQ1",$J,912.04,DA,1.03,"E"))]"" S ESPX=$E(SPACES,1,14)_^("E")_","
 .  S ESPX=ESPX_$G(^UTILITY("DIQ1",$J,912.04,DA,1.04,"E"))_" "_$G(^UTILITY("DIQ1",$J,912.04,DA,1.05,"E"))
 .  S ESPX="HOME PHONE: "_$G(^UTILITY("DIQ1",$J,912.04,DA,1.06,"E")) D MSG
 .  S ESPX="WORK ADDRESS: "_$G(^UTILITY("DIQ1",$J,912.04,DA,2.01,"E")) D MSG
 .  I $G(^UTILITY("DIQ1",$J,912.04,DA,2.02,"E"))]"" S ESPX=$E(SPACES,1,14)_^("E") D MSG
 .  I $G(^UTILITY("DIQ1",$J,912.04,DA,2.03,"E"))]"" S ESPX=$E(SPACES,1,14)_^("E")_","
 .  S ESPX=ESPX_$G(^UTILITY("DIQ1",$J,912.04,DA,2.04,"E"))_" "_$G(^UTILITY("DIQ1",$J,912.04,DA,2.05,"E")) D MSG
 .  S ESPX="WORK PHONE: "_$G(^UTILITY("DIQ1",$J,912.04,DA,2.06,"E")) D MSG
 .  S IEN=0,ESPX="MEDICAL TREATMENT:" D MSG
 .  F ESPN1=1:1 S IEN=$O(^ESP(912,ESPID,30,ESPN,10,IEN)) Q:IEN'>0  S ESPX=$E(SPACES,1,7)_^(IEN,0) D MSG
 .  S ESPX=" " D MSG
CONT G ^ESPORM2
EXIT K DA,DIC,DIQ,DIRUT,DR,DTOUT,DUOUT,ESPDTR,ESPI,ESPID,ESPL,ESPN,ESPN1,ESPX,I,IEN,PAGE,SPACES,X,Y
 K ^UTILITY("DIQ1",$J),^TMP($J,"UORM")
 QUIT
HDR ;PRINT HEADING
 S PAGE=PAGE+1 S ESPX=$E(SPACES,1,25)_"DEPARTMENT OF VETERANS AFFAIRS"_$E(SPACES,1,IOM-10)_"PAGE:  "_$J(PAGE,3)
 D MSG
 S ESPX=$E(SPACES,1,35)_"VA POLICE" D MSG
 S ESPX=$E(SPACES,1,28)_"UNIFORM OFFENSE REPORT"
 D MSG
 S ESPX=$E(SPACES,1,30)_"UOR# "_$E(ESPDTR,2,3)_"-"_$E(ESPDTR,4,5)_"-"_$E(ESPDTR,6,7)_"-"_$TR($E($P(ESPDTR,".",2)_"ZZZZ",1,4),"Z",0)
 D MSG
 S ESPX="VA Facility" D MSG
 I $D(^ESP(912,ESPID,5)),$P(^(5),U)'="" S ESPX=$S($D(^DIC(4,$P(^(5),U),0)):$P(^(0),U),1:"") D MSG
 S ESPX="Automated VA Form 10-1393" D MSG
 F ESPI=1:1:2 S ESPX=" " D MSG
 QUIT
MSG S ^TMP($J,"UORM",ESPL)=ESPX,ESPL=ESPL+1
 QUIT
