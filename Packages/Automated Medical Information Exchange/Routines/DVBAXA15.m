DVBAXA15 ; ;03/19/13
 S X=DE(3),DIC=DIE
 S A1B2TAG="PAT" D ^A1B2XFR
 S X=DE(3),DIC=DIE
 D EVENT^IVMPLOG(DA)
 S X=DE(3),DIC=DIE
 S IVMX=X,X="IVMPXFR" X ^%ZOSF("TEST") D:$T DPT^IVMPXFR S X=IVMX K IVMX
 S X=DE(3),DIC=DIE
 I ($T(AVAFC^VAFCDD01)'="") S VAFCF=".117;" D AVAFC^VAFCDD01(DA)
 S X=DE(3),DIIX=2_U_DIFLD D AUDIT^DIET
