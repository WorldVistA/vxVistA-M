ECX8111 ; COMPILED XREF FOR FILE #727.811 ; 03/19/13
 ; 
 S DIKZK=2
 S DIKZ(0)=$G(^ECX(727.811,DA,0))
 S X=$P(DIKZ(0),U,3)
 I X'="" K ^ECX(727.811,"AC",$E(X,1,30),DA)
END Q
