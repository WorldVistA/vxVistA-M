PSOXZA10 ; COMPILED XREF FOR FILE #52.052311 ; 03/19/13
 ; 
 S DA=0
A1 ;
 I $D(DISET) K DIKLM S:DIKM1=1 DIKLM=1 G @DIKM1
0 ;
A S DA=$O(^PSRX(DA(1),"ICD",DA)) I DA'>0 S DA=0 G END
1 ;
 S DIKZ(0)=$G(^PSRX(DA(1),"ICD",DA,0))
 S X=$P(DIKZ(0),U,1)
 I X'="" S ^PSRX(DA(1),"ICD","B",$E(X,1,30),DA)=""
 S X=$P(DIKZ(0),U,1)
 I X'="" S ^PSRX("E",$E(X,1,30),DA(1),DA)=""
 G:'$D(DIKLM) A Q:$D(DISET)
END G ^PSOXZA11
