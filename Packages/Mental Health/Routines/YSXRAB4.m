YSXRAB4 ; COMPILED XREF FOR FILE #601.01 ; 10/15/04
 ; 
 S DA(1)=DA S DA=0
A1 ;
 I $D(DISET) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^YTT(601,DA(1),"S",DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^YTT(601,DA(1),"S",DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" S ^YTT(601,DA(1),"S","B",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,2)
 I X'="" N X1 S X1=$O(^DD("FUNC","B","STRIPBLANKS",0)) X ^DD("FUNC",X1,1) S ^YTT(601,DA(1),"S","C",X,DA)=""
 G:'$D(DIKLM) A Q:$D(DISET)
END Q
