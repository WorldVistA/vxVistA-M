NURAMHU ;HIRMFO/MD-MANHOURS TMP ROUTINE ;3/89
 ;;4.0;NURSING SERVICE;**7**;Apr 25, 1997
EN1 ;SUBROUTINE TO LOOKUP EXISTING OR CREATE A NEW 213.4 FILE ENTRY
 S NURX=X,DA=$O(^NURSA(213.4,"B",X,0)) Q:DA>0
 S NDAT=$P(^NURSA(213.4,0),"^",3,4),DA=$P(NDAT,"^")+1,NCNT=$P(NDAT,"^",2)
 F DA=DA:1 L +(^NURSA(213.4,"B",NURX),^NURSA(213.4,DA,0)):0 Q:$T&'$D(^NURSA(213.4,DA,0))
 S NDA=$O(^NURSA(213.4,"B",NURX,0)) I NDA>0 S DA=NDA L -(^NURSA(213.4,"B",NURX),^NURSA(213.4,DA,0)) Q
 S ^NURSA(213.4,DA,0)=NURX,DIK="^NURSA(213.4," D IX1^DIK K DIK
 L -(^NURSA(213.4,"B",NURX),^NURSA(213.4,DA,0)) S $P(^NURSA(213.4,0),"^",3,4)=DA_"^"_(NCNT+1) K ZA,NCNT
 Q
EN2 ; PURGE DATA FROM MANHOURS MULTIPLE IN SITE PARAMETER FILE
 S %DT="",X="T-31" D ^%DT S NURCHK=+Y F DA=0:0 S DA=$O(^DIC(213.9,1,"MAN",DA)) Q:DA'>0  I $P(^DIC(213.9,1,"MAN",DA,0),"^")'>NURCHK S DA(1)=1,DIK="^DIC(213.9,DA(1),""MAN""," D ^DIK
 K NURCHK,DIK,%DT,DA
 Q
