NVSPDS3 ;emciss/maw-scramble patient data (cont) ; 09/01/02
 ;;6.0;EMC TEST ACCOUNT RESET UTILITIES; 01 Jun 1999;;Build 24
 ;
 N NVSDATA
 ;
 ; note:  the variable NVSDFN is set in the main loop through ^DPT(...)
 ;        that begins in routine ^NVSPDS.
 I '$D(NVSDFN) Q
 ;        
 ; scramble agent orange registration number...
 S NVSDATA=$G(^DPT(NVSDFN,.321))
 I $P(NVSDATA,U,10)'="" D PFEDIT^NVSPDSU(NVSDFN,".3211",$$SCR^NVSPDSU($P(NVSDATA,U,10)))
 K NVSDATA
 ;
 ; scramble emergency contact data...
 S NVSDATA=$G(^DPT(NVSDFN,.33))
 I NVSDATA'="" D
 .I $P(NVSDATA,U,1)]"" D PFEDIT^NVSPDSU(NVSDFN,".331",$$REVN^NVSPDSU($P(NVSDATA,U,1)))
 .I $P(NVSDATA,U,6)]"" D PFEDIT^NVSPDSU(NVSDFN,".336",$$CITY^NVSPDSU)
 .I $P(NVSDATA,U,7)]"" D PFEDIT^NVSPDSU(NVSDFN,".337",$P($$ST^NVSPDSU(),U,2))
 .I $P(NVSDATA,U,8)]"" D PFEDIT^NVSPDSU(NVSDFN,".338",$$SCR^NVSPDSU($P(NVSDATA,U,8)))
 .I $P(NVSDATA,U,9)]"" D PFEDIT^NVSPDSU(NVSDFN,".339",$$SCR^NVSPDSU($P(NVSDATA,U,9)))
 .I $P(NVSDATA,U,11)]"" D PFEDIT^NVSPDSU(NVSDFN,".33011",$$SCR^NVSPDSU($P(NVSDATA,U,11)))
 K NVSDATA
 ;
 ; scramble secondary emergency contact data...
 S NVSDATA=$G(^DPT(NVSDFN,.331))
 I NVSDATA'="" D
 .I $P(NVSDATA,U,1)]"" D PFEDIT^NVSPDSU(NVSDFN,".3311",$$REVN^NVSPDSU($P(NVSDATA,U,1)))
 .I $P(NVSDATA,U,6)]"" D PFEDIT^NVSPDSU(NVSDFN,".3316",$$CITY^NVSPDSU)
 .I $P(NVSDATA,U,7)]"" D PFEDIT^NVSPDSU(NVSDFN,".3317",$P($$ST^NVSPDSU(),U,2))
 .I $P(NVSDATA,U,8)]"" D PFEDIT^NVSPDSU(NVSDFN,".3318",$$SCR^NVSPDSU($P(NVSDATA,U,8)))
 .I $P(NVSDATA,U,9)]"" D PFEDIT^NVSPDSU(NVSDFN,".3319",$$SCR^NVSPDSU($P(NVSDATA,U,9)))
 .I $P(NVSDATA,U,11)["" D PFEDIT^NVSPDSU(NVSDFN,".331011",$$SCR^NVSPDSU($P(NVSDATA,U,11)))
 K NVSDATA
 ;
 ; scramble designee data...
 S NVSDATA=$G(^DPT(NVSDFN,.34))
 I NVSDATA'="" D
 .I $P(NVSDATA,U,1)]"" D PFEDIT^NVSPDSU(NVSDFN,".341",$$REVN^NVSPDSU($P(NVSDATA,U,1)))
 .I $P(NVSDATA,U,6)]"" D PFEDIT^NVSPDSU(NVSDFN,".346",$$CITY^NVSPDSU)
 .I $P(NVSDATA,U,7)]"" D PFEDIT^NVSPDSU(NVSDFN,".347",$P($$ST^NVSPDSU(),U,2))
 .I $P(NVSDATA,U,8)]"" D PFEDIT^NVSPDSU(NVSDFN,".348",$$SCR^NVSPDSU($P(NVSDATA,U,8)))
 .I $P(NVSDATA,U,9)]"" D PFEDIT^NVSPDSU(NVSDFN,".349",$$SCR^NVSPDSU($P(NVSDATA,U,9)))
 .I $P(NVSDATA,U,11)["" D PFEDIT^NVSPDSU(NVSDFN,".34011",$$SCR^NVSPDSU($P(NVSDATA,U,11)))
 K NVSDATA
 Q