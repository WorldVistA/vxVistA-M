ACKQTE7 ; ;03/19/13
 S X=DG(DQ),DIC=DIE
 S ACKCAT=$E(^ACK(509850,X,0),1,2),ACKMO=$E(^ACK(509850.6,DA,0),1,5)_"00",$P(^(ACKCAT),U)=$S($D(^ACK(509850.6,"ATCAT",ACKMO,ACKCAT)):^(ACKCAT)+1,1:1)
 S X=DG(DQ),DIC=DIE
 S ACKCDR=+^ACK(509850,X,0),ACKMO=$E(^ACK(509850.6,DA,0),1,5)_"00",$P(^(ACKCDR),U)=$S($D(^ACK(509850.6,"ATCDR",ACKMO,ACKCDR)):^(ACKCDR)+1,1:1) K ACKMO,ACKCDR
 S X=DG(DQ),DIC=DIE
 S ^ACK(509850.6,"ADCI",$E($P(^ACK(509850.6,DA,0),U),1,5)_"00",X,DA)=""
