A1CKC2 ; ;05/20/09
 S X=DE(10),DIC=DIE
 S DFN=DA D EN^DGMTCOR K DGMTCOR
 S X=DE(10),DIC=DIE
 S DFN=DA D EN^DGRP7CC
 S X=DE(10),DIC=DIE
 ;
 S X=DE(10),DIC=DIE
 D AUTOUPD^DGENA2(DA)
 S X=DE(10),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF="1901;" D AVAFC^VAFCDD01(DA)
 S X=DE(10),DIC=DIE
 D:($T(ADGRU^DGRUDD01)'="") ADGRU^DGRUDD01(DA)
