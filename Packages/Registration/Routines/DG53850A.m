DG53850A ;ALB/JRC - ICD-10 PRE-INIT ;3/12/11 7:21am
 ;;5.3;Registration;**850**;Aug 13, 1993;Build 171
 ;
 Q
 ;
EN ;Delete updated fields to remove screens.
 N DGFIELD,X,Y,DA,DIK,DGDUZSV,DGPATCH,DGIDX
 I $G(DUZ)="" W !,"Your DUZ is not defined.  It must be defined to run this routine." Q
 S DGDUZSV=DUZ(0),DUZ(0)="@"
 ;
 ;
 D EN^DDIOL("Cleaning up PTF file DD for PROCEDURE CODES")
 F DGFIELD=45.01:.01:45.05 D
 . S DIK="^DD(45,",DA=DGFIELD,DA(1)=45
 . D ^DIK
 ;
 D EN^DDIOL("Cleaning up PTF file DD for DIAGNOSIS CODES")
 F DGFIELD=79,79.16,79.17,79.18,79.19,79.201,79.21,79.22,79.23,79.24,79.241,79.242,79.243,79.244 D
 . S DIK="^DD(45,",DA=DGFIELD,DA(1)=45
 . D ^DIK
 ;
 ;W !!,"CPT DIAGNOSIS"
 F DGFIELD=.04 D
 . S DIK="^DD(45.06,",DA=DGFIELD,DA(1)=45.06 D ^DIK
 . D ^DIK
 ;
 ;W !!,"601 MOVEMENT"
 F DGFIELD=4,5,6,7,8 D
 . S DIK="^DD(45.05,",DA=DGFIELD,DA(1)=45.05
 . D ^DIK
 ;
 ;W !!,"401 MOVEMENT"
 F DGFIELD=8,9,10,11,12 D
 . S DIK="^DD(45.01,",DA=DGFIELD,DA(1)=45.01
 . D ^DIK
 ;
 ;W !!,"501 MOVEMENT"
 F DGFIELD=5,6,7,8,9,11,12,13,14,15 D
 . S DIK="^DD(45.02,",DA=DGFIELD,DA(1)=45.02
 . D ^DIK
 ;
 ;W !!,"INPATIENT CPT CODES"
 F DGFIELD=.04,.05,.06,.07,.21,.22,.23,.24 D
 . S DIK="^DD(46,",DA=DGFIELD,DA(1)=46
 . D ^DIK
 S DUZ(0)=DGDUZSV
 Q
 ;
 ;