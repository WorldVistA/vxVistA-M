IBDX02 ; COMPILED XREF FOR FILE #357.02 ; 10/15/04
 ; 
 S DA(1)=DA S DA=0
A1 ;
 I $D(DIKILL) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^IBE(357,DA(1),2,DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^IBE(357,DA(1),2,DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" K ^IBE(357,DA(1),2,"B",$E(X,1,30),DA)
 G:'$D(DIKLM) A Q:$D(DIKILL)
END Q
