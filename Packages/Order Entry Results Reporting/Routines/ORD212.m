ORD212 ; COMPILED XREF FOR FILE #100.045 ; 03/19/13
 ; 
 S DA=0
A1 ;
 I $D(DISET) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^OR(100,DA(1),4.5,DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^OR(100,DA(1),4.5,DA,0))
 S X=$P(DIKZ(0),U,4)
 I X'="" S ^OR(100,DA(1),4.5,"ID",$E(X,1,30),DA)=""
 G:'$D(DIKLM) A Q:$D(DISET)
END G ^ORD213
