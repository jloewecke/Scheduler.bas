

	! DEBUG MODE BITS (IN DEBUG%, TAKEN FROM LOGICAL: WIZARD$DEBUG)

	! BIT	CAUSES LOGIC-FLOW LOG FOR
	!------	-------------------------
	! 1	Main ACTIVITY loop
	! 2	TOKEN evaluation
	! 4	PRIORITY CALCULATIONS
	! 8	Collector assignment
	! 16	Letter series activity
	! 32	Letter activity
	! 64	NEWBIZ processing
	! 128	SPARE
	! 256	SPARE
	! 512	DEBT::CLI and PRI.CLI I/O
	! 1024	Promise processing
	! 2048	Payment processing
	! 4096	SPARE
	! 8192	Collector and Client Statistics
	! 16384	LOG closing every 256 ACTIVITY records
	! 32768	Startup and table-loading
	! 65536	Zero and negative balance logic
	! 131072 
	! ---------------------------------------------------------------

	%LET %EXPLICIT=1%
	%INCLUDE "SUB:SYSTEM.INC"
	%INCLUDE "$SSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	DECLARE LONG E%, EXIT.E%, DIAL.OUT.OF.STATE%, REVERTED%,        &
		BLITZ%, LOCKOUT%, T%, FLUSH%, FLUSH.CTR%, 		&
		REPRIORITIZE%, DOUBLE.ROLL%, ARCHIVE.CLOSED%, COLSUB%,	&
		MAKE.ZBAL.PIF.COND%, MAKE.ZBAL.PIF%, ARCHIVE.ZBAL%,	&
		ARCHIVE.NEGBAL%, INTSUB%, DIALWIN%, I%, EM.TEL%, X%, 	&
		STRAT.E%, STRAT.RECOUNT%, S%, HIGH.STRATEGY%,	&
		DONE%, J%, K%, MAX.COL%, MAX.STAT%, DEBTORS.PROCESSED%,	&
		PRECOLLECT%, TOTAL.PRECOLLECT%, T.STRAT%, STRATEGY%,	&
		T.STEP%, STEP%, ARCHIVE%, ERROR.IN.PROCESS%, ZBAL%,	&
		DELETE.ACT.REC%, NEGBAL%, FLUSH.TOT%, OLD.MAY.KEEP%,	&
		COLLECTOR%, AUDIT.CHANGE%, CLOSED%, ACT.LOOP.COUNT%,	&
		OPERATION.DONE%, STEPS.COMPLETED%, VALID%, DO.TRACE%,	&
		CONDITION%, OPERATION%, OK.TO.BUMP.STEP%, DO.IT%, D%,	&
		DEBT.SPECIFIC%, DEBT.E%, SER.I%, TOTAL.LOG.RECORDS%,	&
		CHOICE%, TOTAL.CLER.RECORDS%, TOTAL.SUPV.RECORDS%, T1%, &
		O.INT%, PREV.STATUS%, STATUS.INDEX%, T2%, ZIP.E%,	&
		NUM.HIST%, TOTAL.LETTERS%, TOTAL.ACTIVITY%, T3%,	&
		DIALER.PHONE%, OUT.OF.STATE%, TOTAL.DSCHD.RECORDS%,	&
		TOTAL.SCHD.RECORDS%, RELOAD%, ASSIGN%, ORIG.COL.QUAL%,	&
		DIALER.GETS.IT%, ALREADY.CHECKED.DIALER%, PICK%, ADD%,	&
		UPD.E%, PRIORITY%, OFFSET%, BIT%, CRITERIA%, BITVAL%,	&
		WAS.CLOSED%, TOTAL.NEWBIZ%, TOTAL.PAYMENTS%, H.FLAG%,	&
		SAVE.STATUS.INDEX%, SAVE.CLOSED%, R.TYPE%, COLL.CHG%,	&
		KEPT%, YY%, MM%, DD%, PC.FLAG%, XFER.OPENED%, C.TYPE%,	&
		PRECOLLECT.XFER%, CHARGE.TYPE%, ACK.OPENED%, MIN.RELEASE%, &
		MAX.RELEASE%, ORIGINAL.DEBUG%, TOTAL.STEPS.COMPLETED%,  &
		UPD.STAT.OPENED%, PRIORITY.CALCULATED%, GUESS%, Z%,     &
		HIST.DT.UPD%, HIST.E%, COMPUTE%, OFF%, PDC%, CAN%,	&
		STRAT23%, CRED.OPEN%, PROGRESS%, FOUND.IT%, DIALWIN2%
	
	DECLARE STRING TXT$, T$, DIALER.STATES$, BAD.STATE$,		&
		TYPE.TO.DELETE$, WANT.TYPE$, ZIP$, Z$, TMP$

	DECLARE DOUBLE KEPT.PCNT, AMT, T, COLL.CHG.BASE, COLL.PCT,	&
		EVAL, I, J

	%INCLUDE "SUB:ANCILBUF.INC"
	%INCLUDE "SUB:GETSYSTEM.INC"

	EXTERNAL LONG FUNCTION LIB$GET_SYMBOL

	MIN.RELEASE%=11%
	MAX.RELEASE%=12%
	%INCLUDE "SUB:RELEASE_CHECK.INC"

	DECLARE RFA	ACT.RFA

	DECLARE STRING CONSTANT C255.255 = "255"C+"255"C
	DECLARE STRING CONSTANT C255.254 = "255"C+"254"C
	DECLARE STRING CONSTANT ONE.AM$ = "1"C+"0"C
	DECLARE STRING CONSTANT C1000 = ZERO.3+"3"C+"232"C
	DECLARE LONG CONSTANT MCOL=1000%
	DECLARE LONG CONSTANT MSTAT=300%

	EXIT.E%=1%
	MAP (SKD_MAP) STRING FILL$=1%, AC$=3%, FILL$=1%, TEL$=8%, 	&
		LONG WANT.TYPE%
	MAP (SKD_MAP) STRING FILL$=2%, AC.MID$=1%
	MAP (SKD_MAP) STRING PHONE.WHOLE$=13%, BIT.ENCODED.COL.TYPES$=4%, &
		OFFICE$=1%, COLLECTOR$=2%, BASE.DATE$=3%, CURR.YM$=2%,    &
		DIALWIN$(3%)=2%, HOLD.ACT$=1%, DIALWIN2$(5%)=2%,	  &
		LETF.CHAN$=1%, LETT.CHAN$=1%, TODAY$=3%, YESTERDAY$=3%,   &
		EARLY$=2%, TOMORROW$=3%, TODAY.MINUS3$=3%, OLD30$=3%,	  &
		HIST.TODAY$=3%, TOKEN.DATE$=3%, TWO.MONTHS.AGO$=3%,    	  &
		NUMBERS.DOT$=256%, PRIO.RECORD$(32%)=106%, LETTER$=2%, 	  &
		MERGE.TYPE$(255%)=1%, 					  &
		COL.OFF.NUM$(MCOL)=3%,		! Collector num		  &
		DOUBLE COL.HI(MCOL),		! Hi bal	  	  &
		COL.LO(MCOL),			! Lo bal		  &
		LONG COL.TYPE%(MCOL),		! Type			  &
		COL.WORK%(MCOL),		! Work flag		  &
		COL.ASSIGN%(MCOL),		! "assign" count	  &
		COL.TOO.OLD%(MCOL),		! Work count	 	  &
		COL.WAIT.DAYS%(MCOL),		! Days to wait	  	  &
		OFF.LOCK%(99%),						  &
		STRING COL.PRIO$(MCOL)=1%,		! Prior table	  &
		COL.MAXLET$(MCOL)=1%,		! Max letters		  &
		COL.STRAT.TYPE$(MCOL)=1%,	! Strategy type		  &
		COL.MAX.NEW$(MCOL)=2%,		! Max Dialer New	  &
		REASON$=14%,			! Reassign reason	  &
		STAT.CODE$(MSTAT)=3%,					  &
		STAT.CAT$(MSTAT)=1%,					  &
		STAT.GRP$(MSTAT)=1%,					  &
		STAT.STS.CHNG.TO$(MSTAT)=3%,			 	  &
		STAT.STRA.CHNG.TO$(MSTAT)=1%,				  &
		STAT.FLAGS$(MSTAT)=1%
		
	DIM LONG POWER%(5%), DIALLIN%(3%), DIALLIN2%(5%), WEIGHT%(3%),	  &
		PRIO.TABLE%(32%)

	DIM DOUBLE TOKEN(57%)

	POWER%(I%)=2%**I%   FOR I%=0% TO 5%

	PROGRESS%=0%
	NUMBERS.DOT$=SPACE$(45%)+"-. 0123456789"+SPACE$(198%)

	! All strategies by name
	%INCLUDE "SUB:STRATEGIES.INC"

1000	! File Opens
	OPEN "LOG:SCHEDULER.LOG" FOR OUTPUT AS FILE #LOG.CHAN,		&
			SEQUENTIAL, ACCESS WRITE, RECORDSIZE 132%,	&
			FILESIZE 50%, EXTENDSIZE 50%

	TXT$="SCHEDULER V11.5E Batch run commencing on "	&
						+DATE$(0%)+" at "+TIME$(0%)
	PRINT TXT$
	CALL LOG(TXT$)

	%INCLUDE "SUB:DEBUG.INC"
	ORIGINAL.DEBUG%=DEBUG%

	E%=LIB$SYS_TRNLOG("DIALER$STATES",,DEBUG$)
	IF E%=1%
	THEN	DIALER.STATES$=DEBUG$
	ELSE	DIALER.STATES$=""
	END IF

	E%=LIB$SYS_TRNLOG("DIAL$OUT_OF_STATE",,DEBUG$)
	IF E%=1%
	THEN	DIAL.OUT.OF.STATE%=VAL%(DEBUG$)
	ELSE	DIAL.OUT.OF.STATE%=FALSE
	END IF

	DIM STRING DIAL.LOCKOUT.CITIES$(10%)
	E%=LIB$SYS_TRNLOG("DIAL$LOCKOUT_CITIES",,DEBUG$)
	IF E%=1%
	THEN	LOCKOUT%=0%
		T%=INSTR(0%,DEBUG$,",")
		WHILE T%
			LOCKOUT%=LOCKOUT%+1%
			DIAL.LOCKOUT.CITIES$(LOCKOUT%)=LEFT$(DEBUG$,T%-1%)
			DEBUG$=RIGHT$(DEBUG$,T%+1%)
			T%=INSTR(0%,DEBUG$,",")
		NEXT
		LOCKOUT%=LOCKOUT%+1%
		DIAL.LOCKOUT.CITIES$(LOCKOUT%)=DEBUG$
	ELSE	LOCKOUT%=FALSE
	END IF

	! Return a printable bit mask
1100	DEF STRING FNBITS$(STRING Z$)
		TMP$=""
		FOR Z%=1% TO LEN(Z$)*8%
			TMP$=TMP$+CHOICE(Z$,Z%,1%,"0,1")
			TMP$=TMP$+" "   UNLESS Z% AND 7%	! SPACE BYTES
		NEXT Z%
		FNBITS$=TMP$
	FNEND

	! Mail-merge logic must come before anything...
	MERGE.TYPE$(I%)=CHR$(I%)   FOR I%=0% TO 255%

2000	ON ERROR GOTO UNEXPECTED_TRAP

	%INCLUDE "DDL:LETPENDNG.INC"
	MAP (LETP_MAP) LET_PENDING_RECORD LETP

	WHEN ERROR IN			! Sequential? 20 Byte?
		OPEN "LETPENDNG$SEQ" FOR INPUT AS FILE #LETP.CHAN,	&
			SEQUENTIAL FIXED, 	MAP LETP_MAP,		&
			RECORDSIZE 20%,					&
			ACCESS APPEND,		ALLOW NONE
	USE
		OPEN "LETPENDNG$SEQ" FOR OUTPUT AS FILE #LETP.CHAN,	&
			SEQUENTIAL FIXED, 	MAP LETP_MAP,		&
			RECORDSIZE 20%,					&
			ACCESS APPEND,		ALLOW NONE,		&
			FILESIZE 200%,		EXTENDSIZE 50%
		CALL LOG("%-2000 Wrong format LETPENDNG$SEQ *OVERWRITING*!!!")
	END WHEN

	%INCLUDE "DDL:TRANSFER.INC"
	MAP (PRECOLLECT_XFER_MAP) PRECOLLECT_XFER_RECORD XFER

	%INCLUDE "DDL:NEWBIZACK.INC"
	MAP (ACK_MAP) NEWBIZ_ACK_RECORD ACK

	%INCLUDE "DDL:UPDSTAT.INC"
	MAP (UPDSTAT_MAP) UPD_STAT_RECORD UPD

2005	%INCLUDE "DDL:ANCIL.INC"
	MAP (ANCI_MAP) ANCILLARY_RECORD ANCI

	! This "OPEN" is to ensure that the NEW_ACTIVITY file exists...
2010	%INCLUDE "DDL:ACTIVITY.INC"
	MAP (ACT_MAP) ACTIVITY_RECORD ACT
	WHEN ERROR IN
		OPEN "DATA:NEW_ACTIVITY.DAT" AS FILE #TMP5.CHAN,	&
			SEQUENTIAL FIXED,	MAP ACT_MAP,		&
			ACCESS APPEND,		ALLOW MODIFY
		CLOSE #TMP5.CHAN
	USE
		CLOSE #TMP5.CHAN
	END WHEN

2020	OPEN "ACTIVITY$DAT" FOR INPUT AS FILE #ACT.CHAN,		&
		INDEXED,						&
		ACCESS MODIFY,		ALLOW MODIFY,			&
		BUFFER 12%,		MAP ACT_MAP,			&
		PRIMARY ACT::KEY0	DUPLICATES
	RESTORE #ACT.CHAN, KEY #1%		! Required

2030	MAP (TMP1_MAP) ACTIVITY_RECORD ACT1
	MAP (REVERT_MAP) ACTIVITY_RECORD REVERT
	OPEN "ACTIVITY$DAT" FOR INPUT AS FILE #TMP1.CHAN,		&
		INDEXED,						&
		ACCESS MODIFY,		ALLOW MODIFY,			&
		BUFFER 12%,		MAP TMP1_MAP,			&
		PRIMARY ACT1::KEY0	DUPLICATES

2040	%INCLUDE "DDL:STRATEGY.INC"
	MAP (STRA_MAP) STRATEGY_RECORD STRA
	OPEN "STRATEGY$DAT" FOR INPUT AS FILE #STRAT.CHAN,		&
		INDEXED,						&
		ACCESS READ,		ALLOW MODIFY,			&
		MAP STRA_MAP,		BUFFER 12%,			&
		PRIMARY STRA::KEY0

2050	MAP (TMP2_MAP) ACTIVITY_RECORD ACT2
	OPEN "ACTIVITY$DAT" FOR OUTPUT AS FILE #TMP2.CHAN,		&
		SEQUENTIAL FIXED,	MAP TMP2_MAP

2070	%INCLUDE "DDL:SCHEDULE.INC"
	MAP (SCHD_MAP) SCHEDULE_RECORD SCHD
	OPEN "TEMP:COLLECTOR.SKD" FOR OUTPUT AS FILE #SCHD.CHAN,	&
		SEQUENTIAL VARIABLE,					&
		ACCESS APPEND,		ALLOW NONE,			&
		FILESIZE 200%,		EXTENDSIZE 200%,		&
		BUFFER 2%,		MAP SCHD_MAP

2080	%INCLUDE "DDL:MASTER.INC"
	MAP (MAST_MAP) MASTER_RECORD MAST
	OPEN "MASTER$DAT" FOR INPUT AS FILE #MAST.CHAN,			&
		INDEXED,						&
		ACCESS MODIFY,	ALLOW MODIFY,				&
		BUFFER 24%,	MAP MAST_MAP,				&
		PRIMARY MAST::KEY0

2100	%INCLUDE "DDL:DEBT.INC"
	MAP (DEBT_MAP) DEBT_RECORD DEBT
	OPEN "DEBT$DAT" FOR INPUT AS FILE #DEBT.CHAN,			&
		INDEXED,						&
		ACCESS MODIFY,	ALLOW MODIFY,				&
		BUFFER 24%,	MAP DEBT_MAP,				&
		PRIMARY DEBT::KEY0
	
	%INCLUDE "DDL:PAYHIST.INC"
	MAP (PAYH_MAP) PAYHIST_RECORD PAYH
	OPEN "PAYHIST$DAT" FOR INPUT AS FILE #PAYH.CHAN,		&
		INDEXED,						&
		ACCESS READ,		ALLOW MODIFY,			&
		MAP PAYH_MAP,		BUFFER 12%,			&
		PRIMARY PAYH::KEY0

2110	%INCLUDE "DDL:COMBINATION.INC"
	MAP (COMBO_MAP) COMBINATION_RECORD COMBO
	OPEN "COMBO$DAT" FOR INPUT AS FILE #COMBO.CHAN,			&
		INDEXED,						&
		ACCESS READ,		ALLOW MODIFY,			&
		BUFFER 24%,		MAP COMBO_MAP,			&
		PRIMARY COMBO::KEY0

	%INCLUDE "DDL:COMBO_LETFORMAT.INC"
	MAP (COMBO_MAP) COMBO_LET_FORMAT_RECORD COMBO_LETF
	LETF.CHAN$=CHR$(LETF.CHAN)

	%INCLUDE "DDL:COMBO_LETTEXT.INC"
	MAP (COMBO_LETT_MAP) COMBO_LET_TEXT_RECORD COMBO_LETT
	LETT.CHAN$=CHR$(LETT.CHAN)

	%INCLUDE "DDL:COMBO_ATTORNEY.INC"
	MAP (COMBO_ATTY_MAP) COMBO_ATTY_FWDR_RECORD COMBO_ATTY

2120	%INCLUDE "DDL:CLIENT.INC"
	MAP (CLI_MAP)  CLIENT_RECORD CLI
	OPEN "CLIENT$DAT" FOR INPUT AS FILE #CLI.CHAN,			&
		INDEXED,						&
		ACCESS READ,	ALLOW MODIFY,				&
		BUFFER 24%,	MAP CLI_MAP,				&
		PRIMARY CLI::KEY0

2130	%INCLUDE "DDL:FEE.INC"
	MAP (FEE_MAP) FEE_RECORD FEE
	OPEN "FEE$DAT" FOR INPUT AS FILE #FEE.CHAN,			&
		INDEXED,						&
		ACCESS READ,		ALLOW MODIFY,			&
		MAP FEE_MAP,		BUFFER 24%,			&
		PRIMARY FEE::KEY0

2140	MAP (CLI1_MAP)  CLIENT_RECORD CLI1
	OPEN "CLIENT$DAT" FOR INPUT AS FILE #TMP4.CHAN,			&
		INDEXED,						&
		ACCESS READ,	ALLOW MODIFY,				&
		BUFFER 24%,	MAP CLI1_MAP,				&
		PRIMARY CLI1::KEY0,					&
		CONNECT CLI.CHAN

2150	%INCLUDE "DDL:MISC.INC"
	MAP (MISC_MAP) MISC_RECORD MISC
	OPEN "MISC$DAT" FOR INPUT AS FILE #MISC.CHAN,			&
		INDEXED,						&
		ACCESS READ,	ALLOW MODIFY,				&
		BUFFER 24%,	MAP MISC_MAP,				&
		PRIMARY MISC::KEY0

	%INCLUDE "DDL:ZIPCODE.INC"
	MAP (ZIP_MAP) ZIPCODE_RECORD ZIP
	OPEN "ZIPCODE$DAT" FOR INPUT AS FILE #ZIP.CHAN,			&
		INDEXED FIXED,						&
		ACCESS MODIFY, 		ALLOW MODIFY,			&
		MAP ZIP_MAP,		BUFFER 4%,			&
		PRIMARY ZIP::KEY0

2160	FLUSH%=	CHOICEVAL(SYST::INVOICE.FORM,3%,1%)=1%	
	IF FLUSH%
	THEN	OPEN "SYSTEM$DAT" FOR INPUT AS FILE #SYST.CHAN,		&
			SEQUENTIAL FIXED,				&
			ACCESS MODIFY,		ALLOW MODIFY,		&
			MAP SYST_MAP,		RECORDSIZE 512%

		E%=GETREC(SYST.CHAN,NX.SINGLE.WAIT,0%,"")
		FLUSH%=MAX(ASC(SYST::FLUSH.DAYS)-1%,0%)
		CALL CHOICESET(SYST::INVOICE.FORM,3%,1%,FLUSH%>0% AND 1%)
		SYST::FLUSH.DAYS=CHR$(FLUSH%)
		E%=UPDREC(SYST.CHAN,0%)
		FLUSH%=FLUSH%+1%
		FLUSH.CTR%=0%
		CLOSE #SYST.CHAN
	END IF
	! Is double promise roll enabled or inhibited
	DOUBLE.ROLL%=		CHOICEVAL(SYST::INVOICE.FORM,8%,1%)=0%
	REPRIORITIZE%=		CHOICEVAL(SYST::INVOICE.FORM,5%,1%)=1%
	ARCHIVE.CLOSED%=	CHOICEVAL(SYST::NAME.FLAGS,4%,1%)=1%
	MAKE.ZBAL.PIF.COND%=	CHOICEVAL(SYST::NAME.FLAGS,6%,1%)=1%
	ARCHIVE.ZBAL%=		CHOICEVAL(SYST::NAME.FLAGS,7%,1%)=1%
	MAKE.ZBAL.PIF%=		CHOICEVAL(SYST::FLAGS2,2%,1%)=1%
	ARCHIVE.NEGBAL%=	CHOICEVAL(SYST::FLAGS2,3%,1%)=1%

	KEPT.PCNT=EXPAND(SYST::KEPT.PCNT)/100.
	KEPT.PCNT=1.   IF KEPT.PCNT<.01		! Default to 100% if zero
	COLSUB%=ASC(SYST::FLAGS(0%))-1%
	INTSUB%=ASC(SYST::FLAGS(1%))-1%
	DIALWIN%=-1%
	FOR I%=0% TO 3%
		T$=LEFT$(SYST::DIALWIN(I%),2%)
		ITERATE   IF EDIT$(T$,6%)=""
		ITERATE   IF ANCILDEF(T$)
		DIALWIN%=DIALWIN%+1%
		DIALWIN$(DIALWIN%)=T$
		DIALLIN%(DIALWIN%)=MIN(4%,MAX(-1%,BAD.NUM(RIGHT$(SYST::DIALWIN(I%),3%))-1%))
	NEXT I%

	DIALWIN2%=-1%
	FOR I%=0% TO 5%
		T$=LEFT$(SYST::DIALWIN2(I%),2%)
		ITERATE   IF EDIT$(T$,6%)=""
		ITERATE   IF ANCILDEF(T$)
		DIALWIN2%=DIALWIN2%+1%
		DIALWIN2$(DIALWIN2%)=T$
		DIALLIN2%(DIALWIN2%)=MIN(4%,MAX(-1%,BAD.NUM(RIGHT$(SYST::DIALWIN2(I%),3%))-1%))
	NEXT I%

	BAD.STATE$=","
	X%=0%
	UNTIL X%=17%
		IF EDIT$(SYST::RESTR.STATE(X%),6%)=""
		THEN	X%=17%
			ITERATE
		END IF
		BAD.STATE$=BAD.STATE$+EDIT$(SYST::RESTR.STATE(X%),6%)+","
		X%=X%+1%
	NEXT

2170	%INCLUDE "DDL:LETSERIES.INC"
	MAP (LETS_MAP) LET_SERIES_RECORD LETS
	OPEN "LETSERIES$DAT" FOR INPUT AS FILE #LETS.CHAN,		&
		INDEXED,						&
		ACCESS READ,		ALLOW MODIFY,			&
		MAP LETS_MAP,		BUFFER 12%,			&
		PRIMARY LETS::KEY0

2180	%INCLUDE "DDL:HISTORY.INC"
	MAP (HIST_MAP) HISTORY_RECORD HIST
	OPEN "HISTORY$DAT" FOR INPUT AS FILE #HIST.CHAN,		&
		INDEXED,						&
		ACCESS MODIFY,		ALLOW MODIFY,			&
		MAP HIST_MAP,		BUFFER 24%,			&
		PRIMARY HIST::KEY0

	%INCLUDE "DDL:REPORTS.INC"
	MAP (REPORTS_MAP) REPORTS_RECORD RPT

2300	! Load some memory resident tables...
	! Cache strategies ... will try to hold all the strategies it can
	! Table size based on an average of 50 steps/strat and 200 strategies
	! Important: table will not handle 256 strategies with 256 steps each

	DECLARE LONG CONSTANT	NBR.STEPS=10000%
	MAP (STRATEGIES) STRING STATUS1$=3%
	MAP (STRATEGIES) STRING FILL$=2%, R2.COND.QUAL$=2%, 		&
			 FILL$=2%, R1.OPER.QUAL$=1%
	MAP (STRATEGIES) STRING L2.COND.QUAL$=2%, M3.COND.QUAL$=1%,	&
			 FILL$=1%, L2.OPER.QUAL$=2%
	MAP (STRATEGIES) STRING L1.COND.QUAL$=1%, M23.COND.QUAL$=2%,	&
			 FILL$=1%,				&
			 L1.OPER.QUAL$=1%, M23.OPER.QUAL$=2%
	MAP (STRATEGIES) STRING COND.QUAL$=4%, OPER.QUAL$=3%,		&
			 STRAT.COND.QUAL$(NBR.STEPS)=4%
	MAP (STRATEGIES) LONG BALANCE%, STRING FILL$=3%,		&
			 LONG 	STRAT.BALANCE%(NBR.STEPS),		&
				STRAT.PTR%(256%),			&
			 STRING	STRAT.CODE$(NBR.STEPS)=1%,		&
				STRAT.CONDITION$(NBR.STEPS)=1%,		&
				STRAT.OPER.QUAL$(NBR.STEPS)=3%

	! Clear the pointer array
	STRAT.PTR%(I%)=-1%   FOR I%=0% TO 256%
	! The pointer array contains the starting subscript of each strategy.
	! For any strategy followed by an unused strategy, the pointer array
	! element for the unused strategy is =
	! -(subscript of END step of used strategy) - 1.

	STRAT.E%=GETREC(STRAT.CHAN,NX.REGARD,0%,"")
	S%=ASC(STRA::NUMBER)

	J%=0%					! Running pointers
	UNTIL J%>=NBR.STEPS OR STRAT.E%		! For each strategy...
		STRAT.PTR%(S%)=J%		! Save running pointer
		CALL LOG("!-2300 STRAT ("+NUM1$(S%)+" AT "		&
					+NUM1$(J%)+")")   IF DEBUG% AND 32768%
		DONE%=FALSE
		! Copy each step...
		FOR I%=0% UNTIL I%>255% OR J%>=NBR.STEPS OR DONE%
			STRAT.CODE$(J%)=STRA::OP(I%)::CODE
			STRAT.CONDITION$(J%)=STRA::OP(I%)::CONDITION
			STRAT.COND.QUAL$(J%)=STRA::OP(I%)::COND.QUAL
			STRAT.OPER.QUAL$(J%)=STRA::OP(I%)::QUALIFIER
			DONE%=STRA::OP(I%)::CODE=ZERO.1		! END?
			IF DONE%
			THEN	STRAT.CONDITION$(J%)=ZERO.1	! No cond on end
				HIGH.STRATEGY%=S%	! Save highest stored
							! Strategy
				STRAT.PTR%(S%+1%)=-J%-1%
			END IF
			J%=J%+1%			! Incr running ptr
		NEXT I%				! Next step
		ITERATE   IF J%>=NBR.STEPS
		! Get the next strategy...
		STRAT.E%=GETREC(STRAT.CHAN,NX.REGARD,0%,"")
		S%=ASC(STRA::NUMBER)
		STRAT.RECOUNT%=RECOUNT   UNLESS STRAT.E%
	NEXT						! NEXT STRATEGY
	GOTO UNEXPECTED_ERROR   IF STRAT.E%<>0% AND STRAT.E%<>11%
	PRINT "HIGHEST CACHED STRATEGY IS "+NUM1$(HIGH.STRATEGY%)
	STRAT23%=ABS%(STRAT.PTR%(24%))-ABS%(STRAT.PTR%(23%))>1%	AND	&
							STRAT.PTR%(23%)>0%

2400	! Cache all priorities (1-32)...

	%INCLUDE "DDL:PRIORITY.INC"
	MAP (PRIO_MAP) PRIORITY_RECORD PRIO
	OPEN "PRIORITY$DAT" FOR INPUT AS FILE #PRIO.CHAN,		&
		INDEXED,						&
		ACCESS READ,		ALLOW MODIFY,			&
		MAP PRIO_MAP,		BUFFER 4%,			&
		PRIMARY PRIO::KEY0

	PRIO.TABLE%(I%)=FALSE   FOR I%=0% TO 32%	! Clear the table

	E%=GETREC(PRIO.CHAN,NX.REGARD,0%,"")
	J%=ASC(PRIO::TABLE)

	UNTIL J%>32% OR E%			! For each record...
		PRIO.TABLE%(J%)=TRUE
		CALL LOG("!-2400 PRIOR ("+NUM1$(J%)+")")   IF DEBUG% AND 32768%
		PRIO.RECORD$(J%)=PRIO::WHOLE
		! Get the next record...
		E%=GETREC(PRIO.CHAN,NX.REGARD,0%,"")
		J%=ASC(PRIO::TABLE)
	NEXT						! Next priority table
	GOTO UNEXPECTED_ERROR   IF E%<>0% AND E%<>11%
	CLOSE #PRIO.CHAN

2500	! Collectors (all of them)...
	%INCLUDE "DDL:COLLECTOR.INC"
	MAP (COL_MAP) COLLECTOR_RECORD COL
	OPEN "COLLECTOR$DAT" FOR INPUT AS FILE #COL.CHAN,		&
		INDEXED,						&
		ACCESS READ,	ALLOW MODIFY,				&
		BUFFER 4%,	MAP COL_MAP,				&
		PRIMARY COL::KEY0

	%INCLUDE "DDL:COLLECTORS.INC"
	MAP (COLS_MAP) COLLECTORS_RECORD COLS
	OPEN "COLLECTORS$DAT" FOR INPUT AS FILE #COLS.CHAN,		&
		INDEXED,						&
		ACCESS READ,	ALLOW MODIFY,				&
		BUFFER 4%,	MAP COLS_MAP,				&
		PRIMARY COLS::KEY0

	! Collector account assignment weighting table...
	WEIGHT%(0%)=1%				! HEAVY
	WEIGHT%(1%)=2%				! MEDIUM
	WEIGHT%(2%)=4%				! LIGHT
	WEIGHT%(3%)=9999999%			! NONE

	COL.OFF.NUM$(I%)=ZERO.3   FOR I%=0% TO MCOL

	E%=GETREC(COL.CHAN,NX.REGARD,0%,"")
	E%=GETREC(COLS.CHAN,NX.REGARD,0%,"")   UNTIL COLS::KEY0>=COL::KEY0 OR E%

	J%=0%					! Running pointer
	UNTIL J%>=MCOL OR E%			! For each collector...
		CALL LOG("!-2500 COLLECTOR ("+CEXPAND(COL::OFFICE,	&
					COL::NUMBER)+")")   IF DEBUG% AND 32768%

		S%=CHOICEVAL(COL::FLAGS,6%,2%)	! Isolate work flags
		! S%:  0%=normal, 1%=limit, 2%=reload, 3%=retire

		! Ignore if retired or house
		IF S%<3% AND COL::NUMBER<>ZERO.2
		THEN	COL.OFF.NUM$(J%)=COL::KEY0
			COL.HI(J%)=INT(COL::HIGH.AMT*100.+.00001)
			COL.LO(J%)=INT(COL::LOW.AMT*100.+.00001)
			COL.TYPE%(J%)=COL::TY.PE
			COL.MAXLET$(J%)=COL::MAX.LET
			COL.PRIO$(J%)=COL::PRIO.TABLE

			! K%=DIALER LIMITING
			!	0=NO LIMIT
			!	512=H/M/L
			!	1024=NEWBIZ SIZE
			!	1536=BOTH
			K%=CHOICEVAL(COL::FLAGS2,3%,2%)*512%
			! Documentation for COL.WORK% bit values:
			!        256 bit: 0=single office, 256=multi-office
			!	 128 bit: 0=normal, 128=predictive dialer
			!	  64 bit: 64=allow dialer to get no-phone accts
			! 32 and 16 bits: 0=bal, 16=age, 32=either, 48=both
			!   8 and 4 bits: 0=normal, 4=limit, 8=reload, 12=retire
			!   2 and 1 bits: 0=heavy, 1=normal, 2=light, 3=none

			COL.WORK%(J%)=K%+			  !1536 &
				(ASC(COL::FLAGS2) AND 4%)*64%+	  ! 256	&
				(ASC(COL::FLAGS) AND 128%)+	  ! 128	&
				(ASC(COL::FLAGS2) AND 64%)+	  !  64	&
				(ASC(COL::FLAGS2) AND 3%)*16%+	  !  48	&
				CHOICEVAL(COL::FLAGS,2%,2%)+S%*4% ! +15
								  ! ---
								  !2047
			COL.ASSIGN%(J%)=4%		! Start here
			! Dialers get a head start...
			COL.ASSIGN%(J%)=0%   IF COL.WORK%(J%) AND 128%
			COL.STRAT.TYPE$(J%)=COL::STRATEGY.TYPE
			COL.WAIT.DAYS%(J%)=EXPAND(COL::WAIT.DAYS)
			COL.MAX.NEW$(J%)=COLS::MAX.NEWBIZ
			COL.TOO.OLD%(J%)=EXPAND(COL::TOO.OLD)
			CALL LOG("COL-OFF,STRATTYP,WAITDAYS,FLAGS:"+	&
				CEXPAND(COL::OFFICE,COL::NUMBER)+" "+	&
				COL::STRATEGY.TYPE+" "+			&
				NUM1$(EXPAND(COL::WAIT.DAYS))+" "+	&
				NUM1$(COL.WORK%(J%)))   IF DEBUG% AND 32768%
			J%=J%+1%
		ELSE	CALL LOG("!-2500 RETIRED COLLECTOR")		&
					   IF S%=3% AND DEBUG% AND 32768%
		END IF
		E%=GETREC(COL.CHAN,NX.REGARD,0%,"")
		E%=GETREC(COLS.CHAN,NX.REGARD,0%,"")   			&
					UNTIL COLS::KEY0>=COL::KEY0 OR E%
	NEXT
	MAX.COL%=J%-1%
	E%=2500%   UNLESS E%			! Substitute e%=2500% for e%=0%
	GOTO UNEXPECTED_ERROR   IF E%<>11%	! E%=0% is invalid (table full)
	CLOSE #COL.CHAN, COLS.CHAN

	! Passed: Collector to locate
	! Returns: Index into collector array
	DEF LONG FNCOL.INDEX%(STRING Z$)
		GUESS%=BINSEARCH(Z$,0%,MAX.COL%,0%,COL.OFF.NUM$())
		STOP   IF GUESS%<-1%
		FNCOL.INDEX%=GUESS%
	FNEND

2600	! Cache all misc statuses
	CALL LOG("!-2600  Caching status codes")   IF DEBUG% AND 32768%
	J%=0%
	E%=GETREC(MISC.CHAN,EQ.REGARD,0%,"1"C)
	UNTIL J%>MSTAT OR E% OR MISC::TY.PE<>"1"C
		STAT.CODE$(J%)=MISC::CODE3
		STAT.CAT$(J%)=MISC::STATUS.CAT
		STAT.GRP$(J%)=MISC::STATUS.GROUP
		STAT.STS.CHNG.TO$(J%)=MISC::CHANGE.STATUS
		STAT.STRA.CHNG.TO$(J%)=MISC::CHANGE.STRATEGY
		STAT.FLAGS$(J%)=MISC::FLAGS
		CALL LOG("!-2600  Status code="+MISC::CODE3		&
			+" Cat="+MISC::STATUS.CAT			&
			+"  Group="+MISC::STATUS.GROUP			&
			+" New Status="+MISC::CHANGE.STATUS		&
			+" New Strategy="+MISC::CHANGE.STRATEGY)	&
							   IF DEBUG% AND 32768%
		J%=J%+1%
		E%=GETREC(MISC.CHAN,NX.REGARD,0%,"")
	NEXT
	MAX.STAT%=J%-1%
	E%=11%   UNLESS E% OR J%>MSTAT
	E%=2600%   UNLESS E%			! Substitute e%=2600% for e%=0%
	GOTO UNEXPECTED_ERROR   IF E%<>11%	! E%=2600% is table full
	CLOSE #MISC.CHAN

2700	%INCLUDE "DDL:OFFICE.INC"
	MAP (OFF_MAP) OFFICE_RECORD OFF
	OPEN "OFFICE$DAT" FOR INPUT AS FILE #OFF.CHAN,			&
		INDEXED,						&
		ACCESS READ,		ALLOW MODIFY,			&
		MAP OFF_MAP,		BUFFER 4%,			&
		PRIMARY OFF::KEY0

	E%=GETREC(OFF.CHAN,NX.REGARD,0%,"")
	UNTIL E%
		OFF.LOCK%(ASC(OFF::KEY0))=CHOICEVAL(OFF::FLAGS,1%,2%)
		E%=GETREC(OFF.CHAN,NX.REGARD,0%,"")
	NEXT
	CLOSE #OFF.CHAN

2900	! Build table of office break flags for letter formats
	DIM LONG OB.FLAG%(255%)

	OB.FLAG%(I%)=FALSE   FOR I%=0% TO 255%
	E%=GETREC(COMBO.CHAN,EQ.REGARD,0%,LETF.CHAN$)

	WHILE E%=0% AND COMBO_LETF::TY.PE=LETF.CHAN$
		MERGE.TYPE$(ASC(COMBO_LETF::LET.CODE))=COMBO_LETF::MERGE.FORMAT
		OB.FLAG%(ASC(COMBO_LETF::LET.CODE))=TRUE		&
					IF CHOICEVAL(COMBO_LETF::FLAGS,3%,1%)=1%
		E%=GETREC(COMBO.CHAN,NX.REGARD,0%,"")
	NEXT

	TODAY$=CONTROLDATE
	EARLY$="8"C+ZERO.1	! (08:00) time field for reminders in past

	PRINT " GENERATING SCHEDULE FOR **** ";FDATEX(TODAY$,0%);" ****"
	TOMORROW$=DAYADD(1%,TODAY$)
	! We must now compute YESTERDAY$ for use in deciding whether or
	! not to send contract reminder letters...
	YESTERDAY$=DAYADD(-1%,TODAY$)
	TODAY.MINUS3$=DAYADD(-3%,TODAY)	! must use today (no dollar sign)
	HIST.TODAY$,BASE.DATE$=TODAY
	BASE.DATE$=DAYADD(-1%,BASE.DATE$)   IF NOW<"12"C+ZERO.1
	YESTERDAY$=BASE.DATE$   IF BASE.DATE$<YESTERDAY$
	OLD30$=DAYADD(-30%,TODAY$)
	TWO.MONTHS.AGO$=MONADD(-2%,TODAY$)
	CALL LOG("!-2900 "+FDATEX(TODAY$,0%))	! Announce cycle date
	CALL LOG("!-2900 STARTUP LOGIC COMPLETE")   IF DEBUG% AND 32768%

	OPEN "REPORT:SCHEDULER.RPT" FOR OUTPUT AS FILE #RPT.CHAN,	&
		SEQUENTIAL, ACCESS WRITE, RECORDSIZE 132%

2950	MAP (ZERO_MAP) ACTIVITY_RECORD ACTZ
	MAP (ZERO_MAP) HISTORY_RECORD HISTZ
	MAP (ZERO_MAP) SCHEDULE_RECORD SCHDZ
	MAP (ZERO_MAP) UPD_STAT_RECORD UPDZ

3000
 FIRST_ACTIVITY_LOOP:
	E%=GETREC(ACT.CHAN,NX.REGARD,0%,"")	! Indexed Activity: KEY1
	UNTIL E% OR ACT::D.NEXT.ACT>TODAY$	! Have we gone far enough?
		ACT.RFA=GETRFA(ACT.CHAN)
		CALL LOG("!-3000 GOT ("+ACT::CODE+			&
				") ACTIVITY record for account "+	&
				CEXPAND(ZERO.1,ACT::DEBTOR))   IF DEBUG% AND 1%

		DELETE.ACT.REC%=TRUE		! Assume activity rec goes

		IF ACT::DEBTOR=ZERO.4		! Bad record?
		THEN	CALL LOG("%-3000 ZERO ACT REC... SKIPPING...")
			ERROR.IN.PROCESS%=TRUE
			GOTO SKIP_UPDATE
		END IF

		E%=GETREC(MAST.CHAN,EQ.SINGLE.WAIT,0%,ACT::DEBTOR)
		IF E%
		THEN	CALL LOG("%-3000 ERROR ("+NUM1$(E%)		&
				+"ON MASTER RECORD #"			&
				+CEXPAND(ZERO.1,ACT::DEBTOR)+" - Skipping." )
			DELETE.ACT.REC%=FALSE   UNLESS E%=155%
			ERROR.IN.PROCESS%=TRUE
			GOTO SKIP_UPDATE
		END IF
		DEBTORS.PROCESSED%=DEBTORS.PROCESSED%+1%
		PROGRESS%=PROGRESS%+1%

		! See if this guy is a precollect account
		PRECOLLECT%= CHOICEVAL(MAST::STATUS2,22%,1%)<>0%
		TOTAL.PRECOLLECT%=TOTAL.PRECOLLECT%+1%   IF PRECOLLECT%

		! WARNING: the following two lines should be one of *only*
		! two places that MAST::STRATEGY and MAST::STEP.NUM
		! are evaluated.  Hereafter, strategy% and step%
		! should be used, since they are the two variables
		! that are copied back into the master record.
		T.STRAT%,STRATEGY%=ASC(MAST::STRATEGY)
		T.STEP%,STEP%=ASC(MAST::STEP.NUM)
		ARCHIVE%= STRATEGY%=45% OR STRATEGY%=46% OR STRATEGY%=107%

		! The following line evaluates the status of the account
		! and loads the STATUS.INDEX% and CLOSED% variables.
		GOSUB EVALUATE_STATUS

		ZBAL%,NEGBAL%,ERROR.IN.PROCESS%=FALSE
		! If we have a zero/negative balance then set ZBAL% and NEGBAL%
		GOSUB HANDLE_ZERO_AND_NEG_BALANCES			&
				IF MAST::CURR.BAL<100. AND ACT::CODE<>"L"

	 	! Closed status that should be in ARCHIVE and isn't in ARCHIVE?
		IF CLOSED% AND ARCHIVE.CLOSED% AND NOT ARCHIVE%
		THEN	DELETE.ACT.REC%=TRUE	! No...
			TYPE.TO.DELETE$="CDHWLZ"
			GOSUB DELETE_PENDING_ACTIVITY	! Do it
			T.STRAT%,STRATEGY%=45%	! Force ARCHIVE
			T.STRAT%,STRATEGY%=107%   IF PRECOLLECT%
			ARCHIVE%=TRUE
			T.STEP%,STEP%=0%
			ACT::CODE="W" ! Force a pass through WHATS_NEXT
			CALL LOG("%-3000 C/R Status account "+	&
				CEXPAND(ZERO.1,ACT::DEBTOR)+	&
				" - Forcing to Archive")
		END IF
		IF PROGRESS%>=10000%
		THEN	PRINT TIME$(0%)+" "+NUM1$(DEBTORS.PROCESSED%)
			PROGRESS%=0%
		END IF

		SELECT ACT::CODE
		    CASE "C"		! "C" - COLLECT "n" DAYS: (check
					! for "n" satisfied or COLL got it)
					! "C" - FORCECOL: (check if he got it)
			T%=ASC(MAST::COL.DAYS)

			! Either online cleared or time ran out making T%=0%
			IF T%=0% OR CLOSED% OR ARCHIVE%
			THEN	GOSUB WHATS_NEXT	! Check strategy
			ELSE	DELETE.ACT.REC%=FALSE	! Don't clear activity
				MAST::COL.DAYS=CHR$(T%-1%)   UNLESS T%=128%
				IF T%=128% AND ACT::D.NEXT.ACT<OLD30$ AND FLUSH%
				THEN	FLUSH.CTR%=FLUSH.CTR%+1%
					IF FLUSH.CTR%>=FLUSH%
					THEN	FLUSH.CTR%=0%
						MAST::COL.DAYS=ZERO.1
						FLUSH.TOT%=FLUSH.TOT%+1%
					END IF
				END IF
				GOSUB PUT_IN_QUEUE	! When putting in sched
			END IF
	
			! Dont change D.LAST.UPD if this is just a collect
			! step, or else a reprioritize will happen every night..
			E%=UPDREC(MAST.CHAN,0%)
			GOTO SKIP_UPDATE

		    CASE "L"		! "L" - Asynchronous letter due
			LSET LETTER$=ACT::LETTER
			LETP::USER=ACT::USER	! Propagate for history
			MAST::LAST.LS.LTR=LETTER$   IF ASC(ACT::FLAGS) AND 1%
			GOSUB SEND_A_LETTER	! Put in pending letter file
			GOTO SKIP_UPDATE   UNLESS ASC(ACT::FLAGS) AND 1%

		    CASE "W"		! "W" - Scheduled wait expired
			GOSUB WHATS_NEXT

		    CASE "P"		! "P" - Payment received
			GOSUB PROCESS_PAYMENT

		    CASE "R"		! "R" - Reminder (queued in past)
			GOSUB PROCESS_REMINDERS
			GOTO SKIP_UPDATE

		    CASE "D"		! "D" - Due date on promise
			GOSUB PROCESS_PROMISE

		    CASE "S"		! "S" - Reinstate initiated
			IF PRECOLLECT%
			THEN	T.STRAT%,STRATEGY%=PRECOLLECT.REINSTATE
				ARCHIVE%=FALSE
				T.STEP%,STEP%=0%
				GOSUB WHATS_NEXT
			ELSE	IF CHOICEVAL(ACT::FLAGS,1%,1%)=0%	&
						OR STRATEGY%=0%	! No strategy
				THEN	T.STRAT%,STRATEGY%=REINSTATED	! Reinstate strategy
					ARCHIVE%=FALSE
					T.STEP%,STEP%=0%
					GOSUB WHATS_NEXT
				END IF
			END IF

		    CASE "H"		! "H" - Holds
			GOSUB PROCESS_REMINDERS
			GOTO SKIP_UPDATE

		    CASE "Z"		! "Z" - Retired-collector reminder due
			! Is there a collector of record?
			COLLECTOR%=FNCOL.INDEX%(MAST::COL.OFF.NUM)
			IF COLLECTOR%=-1%
			THEN	WANT.TYPE$=""		! No type preference
				OLD.MAY.KEEP%=FALSE
				AUDIT.CHANGE%= MAST::COL.NUM<>ZERO.2
				REASON$="Retired"
				GOSUB ASSIGN_COLLECTOR	! Assign new collector to debtor
			END IF
			GOSUB PROCESS_REMINDERS

		    CASE "N"		! "N" - NEWBIZ (Obsolete in this loop)
			GOSUB PROCESS_NEWBIZ	! Should never happen

		    CASE ELSE		! ??? - unknown code
			CALL LOG("%-3000 Bad activity code ("+ACT::CODE+")")
			ERROR.IN.PROCESS%=TRUE
			DELETE.ACT.REC%=TRUE
		END SELECT
		MAST::D.LAST.UPD=TODAY$

		E%=UPDREC(MAST.CHAN,0%)		! Update/unlock master rec

 SKIP_UPDATE:	IF ERROR.IN.PROCESS%
		THEN	T$=""
			CALL LOG("%%%%%% OFFENDING ACTIVITY RECORD FOLLOWS:")
			T$=T$+" "+NUM1$(ASC(MID$(ACT::WHOLE,I%,1%)))	&
				   FOR I%=1% TO LEN(ACT::WHOLE)
			CALL LOG("%%%%%%"+T$)	! Dump the whole thing
		END IF

 END_1ST_ACTLOOP:
		! Since we've have made to here, we can delete the act rec...
		IF DELETE.ACT.REC%
		THEN	CALL LOG("!-3000 DELETING CURRENT ACTIVITY RECORD") &
						   IF DEBUG% AND 1%
			E%=GETBYRFA(TMP1.CHAN,EQ.SINGLE.WAIT,ACT.RFA)
			E%=DELREC(TMP1.CHAN)   UNLESS E% ! Delete unless gone
			! (Might be gone due to a global activity delete that
			!  occurred during the operation.)
		END IF

		GOSUB DO_LOG_CHAN   IF DEBUG% AND 16384%
		E%=GETREC(ACT.CHAN,NX.REGARD,0%,"")	! Activity
	NEXT

	CALL LOG("?-3000 UNEXPECTED ERROR ("+NUM1$(E%)+			&
		") ON ACT CHAN - (ABORTING)")				&
			IF E% AND E%<>11%	! Something BAD happened
3100	WHEN ERROR IN
		OPEN "DATA:NEW_ACTIVITY.DAT" FOR INPUT AS FILE #TMP5.CHAN,  &
			SEQUENTIAL FIXED,		MAP ACT_MAP,	    &
			ACCESS READ,			ALLOW NONE
		E%=FALSE
	USE
		CLOSE #TMP5.CHAN
		SLEEP 120%	! Wait for NEWBIZ	
		E%=TRUE
	END WHEN
	GOTO 3100   IF E%
	
3200	E%=GETREC(TMP5.CHAN,NX.REGARD,0%,"")
	UNTIL E%			! Process the whole file
		CALL LOG("!-3200 GOT ("+ACT::CODE+") ACTIVITY RECORD")	&
							IF DEBUG% AND 1%

		IF ACT::DEBTOR=ZERO.4		! Bad record?
		THEN	CALL LOG("%-3200 ZERO ACT REC... SKIPPING...")
			GOTO END_2ND_ACTLOOP
		END IF

		E%=GETREC(MAST.CHAN,EQ.SINGLE.WAIT,0%,ACT::DEBTOR)
		IF E%
		THEN	CALL LOG("%-3200 ERROR ("+NUM1$(E%)		&
				+"ON MASTER RECORD #"			&
				+CEXPAND(ZERO.1,ACT::DEBTOR)+" - Skipping." )
			GOTO END_2ND_ACTLOOP
		END IF

		DEBTORS.PROCESSED%=DEBTORS.PROCESSED%+1%
		PROGRESS%=PROGRESS%+1%

		! See if this guy is a precollect account
		PRECOLLECT%= CHOICEVAL(MAST::STATUS2,22%,1%)<>0%
		TOTAL.PRECOLLECT%=TOTAL.PRECOLLECT%+1%   IF PRECOLLECT%

		! WARNING: the following two lines should be one of *only*
		! two places that MAST::STRATEGY and MAST::STEP.NUM
		! are evaluated.  Hereafter, strategy% and step%
		! should be used, since they are the two variables
		! that are copied back into the master record.
	
		T.STRAT%,STRATEGY%=ASC(MAST::STRATEGY)
		T.STEP%,STEP%=ASC(MAST::STEP.NUM)
		ARCHIVE%= STRATEGY%=45% OR STRATEGY%=46% OR STRATEGY%=107%

		! The following line evaluates the status of the account
		! and loads the STATUS.INDEX% and CLOSED% variables.
		GOSUB EVALUATE_STATUS

		ERROR.IN.PROCESS%=FALSE		! Prepare to clean up afterward
		DELETE.ACT.REC%=TRUE		! NEW_ACTIVITY all goes bye-bye

		GOSUB PROCESS_NEWBIZ		! This is all that's in the file
		MAST::D.LAST.UPD=TODAY$
		E%=UPDREC(MAST.CHAN,0%)		! Update/unlock master rec

		IF ERROR.IN.PROCESS%
		THEN	T$=""
			CALL LOG("%%%%%% OFFENDING ACTIVITY RECORD FOLLOWS:")
			T$=T$+" "+NUM1$(ASC(MID$(ACT::WHOLE,I%,1%)))	&
					   FOR I%=1% TO LEN(ACT::WHOLE)
			CALL LOG("%%%%%%"+T$)	! Dump the whole thing
		END IF
 END_2ND_ACTLOOP:
		GOSUB DO_LOG_CHAN   IF DEBUG% AND 16384%
		E%=GETREC(TMP5.CHAN,NX.REGARD,0%,"")
	NEXT

	CALL LOG("%-3200 UNEXPECTED ERROR ("+NUM1$(E%)+			&
			") ON TMP5 CHAN - (ABORTING)")   IF E%<>11%

	PRINT "NEWBIZ done at ";TIME$(0%)
	CALL LOG("!-3200 NEWBIZ done!")

	CLOSE #TMP5.CHAN
	! The following KILL always works because the OPEN creates
	! a file if one is not there already...
	NAME "DATA:NEW_ACTIVITY.DAT;" AS "DATA:NEW_ACTIVITY.DONE"
	CALL LOG("****** RUN COMPLETED")
	GOTO END_OF_RUN
3600
 DO_LOG_CHAN:
	! This routine keeps a count of the number of activity loop iterations
	! and closes the LOG.CHAN every 256 times.  
	ACT.LOOP.COUNT%= (ACT.LOOP.COUNT%+1%) AND 255%
	RETURN   IF ACT.LOOP.COUNT%

	CLOSE #LOG.CHAN
	OPEN "LOG:SCHEDULER.LOG" AS FILE #LOG.CHAN, ACCESS APPEND,	&
			SEQUENTIAL, RECORDSIZE 132%
	RETURN
4000
 WHATS_NEXT:	! What operation is next on this debtor (driven by strategy)
		! Passed: strategy% and step%
		! Step% points to the *next* step to perform
	OPERATION.DONE%=FALSE	! Clear initially.  set when there is no
				! More to do on this guy.
	REVERTED%=FALSE		! Has not reverted(yet)
	E%,STEPS.COMPLETED%=0%	! Keep track of the number of steps completed
				! To break infinite loops in strategies.
	GOSUB VALIDATE_STRATEGY_STEP
	GOSUB GET_PRIMARY_CLIENT   UNLESS MAST::PRI.CLI.OFF.NUM=CLI::KEY0
	GOSUB ASSIGN_STRATEGY   UNLESS VALID%

	UNTIL OPERATION.DONE% OR STEPS.COMPLETED%>255%	! Increased to 255
							! Was 50
		IF DO.TRACE%
		THEN	CALL LOG("*TRACE* DEBTOR="+			&
			CEXPAND(ZERO.1,MAST::DEBTOR)+			&
			"  STRATEGY="+NUM1$(STRATEGY%)+"/"+NUM1$(STEP%) &
			+"  STEPS COMPLETED="+NUM1$(STEPS.COMPLETED%))
		END IF

		IF STRATEGY%<=HIGH.STRATEGY%	! Do we have it in memory?
		THEN	I%=STRAT.PTR%(STRATEGY%)+STEP%	! Compute table index
			CONDITION%=ASC(STRAT.CONDITION$(I%))
			OPERATION%=ASC(STRAT.CODE$(I%))

			! BALANCE% and STATUS1$ are loaded by the next line...
			COND.QUAL$=STRAT.COND.QUAL$(I%)
			OPER.QUAL$=STRAT.OPER.QUAL$(I%)
		ELSE	E%=0%
			E%=GETREC(STRAT.CHAN,EQ.REGARD,0%,CHR$(STRATEGY%)) &
				   UNLESS CHR$(STRATEGY%)=STRA::NUMBER
			IF E%
			THEN	CALL LOG("%-4000 ERROR ("+NUM1$(E%)	&
					+") GETTING STRATEGY RECORD...")
				GOSUB LOG_VITAL_STATISTICS
				OPERATION.DONE%=TRUE	! Force loop exit
				ITERATE			! Leave
			END IF

			CONDITION%=ASC(STRA::OP(STEP%)::CONDITION)
			OPERATION%=ASC(STRA::OP(STEP%)::CODE)

			! BALANCE% and STATUS1$ are loaded by the next line...
			COND.QUAL$=STRA::OP(STEP%)::COND.QUAL
			OPER.QUAL$=STRA::OP(STEP%)::QUALIFIER
		END IF

		IF (STRATEGY%=38% OR STRATEGY%=105%) AND STEP%=0%
		THEN	CALL LOG("!-4000 ENTERING BROKEN PROMISE STRATEGY:"+  &
			    CEXPAND(ZERO.1,MAST::DEBTOR))   IF DEBUG% AND 1024%
			CALL CMPINC(MAST::PROM.MISSED)
			IF MAST::POST.PROM.LTR<>ZERO.2
			THEN	LSET LETTER$=MAST::POST.PROM.LTR
				LETP::USER="SCHD"
				GOSUB SEND_A_LETTER
			END IF
		END IF
		OK.TO.BUMP.STEP%=TRUE	! Initially it is ok. IF an
			! Operation changes the strategy or step, it is
			! No longer ok to increment the step

		! Go do operation (after checking condition%)...
		GOSUB PERFORM_OPERATION
		STEPS.COMPLETED%=STEPS.COMPLETED%+1%

		! Only bump step if prior operation didn't mess with it...
		ITERATE   UNLESS OK.TO.BUMP.STEP%

		T.STRAT%=STRATEGY%		! Target=current
		T.STEP%=STEP%+1%		! Target=current+1
		GOSUB VALIDATE_STRATEGY_STEP	! Shouldn't be needed

		IF VALID%
		THEN	STRATEGY%=T.STRAT%
			STEP%=T.STEP%
		ELSE	OPERATION.DONE%=TRUE	! Force loop exit
		END IF
	NEXT
	GOSUB LOAD_STRATEGY_INTO_MASTER

	IF STEPS.COMPLETED%>255%	! Infinite loop detected in strategy?
	THEN	CALL LOG("%-4000 INFINITE-LOOP DETECTED IN STRATEGY")
		GOSUB LOG_VITAL_STATISTICS
	END IF

	TOTAL.STEPS.COMPLETED%=TOTAL.STEPS.COMPLETED%+STEPS.COMPLETED%	! Tally

	! Always exits pointing to *next* scheduler operation to perform
	! In the case of debtors that hit the end of a strategy, they stay
	! Where they are (for lack of anything better to do with them).
	! Exception: hit the end of any promise/contract/pdc strategy, and
	! The debtor goes directly to "broken" (38 or 105).
	RETURN
4500
 VALIDATE_STRATEGY_STEP:		! Passed: (target): T.STRAT%, T.STEP%
	VALID%=TRUE				! Assume valid
	GOTO UNCACHED_STRATEGY   IF T.STRAT%>HIGH.STRATEGY%
	S%=STRAT.PTR%(T.STRAT%)
	GOTO BAD_STRATEGY_STEP   IF S%<0%	! Strategy ok?

	RETURN   IF S%+T.STEP%<ABS%(STRAT.PTR%(T.STRAT%+1%))

 BAD_STRATEGY_STEP:
	VALID%=FALSE
	RETURN   IF T.STRAT%=0%
	CALL LOG("%-4500 BAD STRATEGY ("+NUM1$(T.STRAT%)		&
		+") OR STEP("+NUM1$(T.STEP%)+")")
	GOSUB LOG_VITAL_STATISTICS
	RETURN

 UNCACHED_STRATEGY:	! Only gets here for strategy numbers>HIGH.STRATEGY%
	IF T.STRAT%<>ASC(STRA::NUMBER)		! Already have it?
	THEN	E%=GETREC(STRAT.CHAN,EQ.REGARD,0%,CHR$(T.STRAT%))
		IF E%			! Huh? how did he get that strategy?
		THEN	CALL LOG("%-4500 ERROR ("+NUM1$(E%)		&
				+") GETTING STRATEGY RECORD...")
			GOSUB LOG_VITAL_STATISTICS
			GOTO BAD_STRATEGY_STEP
		ELSE	STRAT.RECOUNT%=RECOUNT
		END IF
	END IF

	! Warning: for speed, the following code computes the number of
	! steps in a strategy by using the bytes in the strategy
	! record.  Should the record layout change, the following
	! algorithm must change also.  The following removes the
	! first 31 byte (fixed control) portion of the strategy,
	! divides the result by 9 (the number of bytes per step),
	! and adds one, (the last step is always "end", so it is
	! never stored, although it is a legal step).
	T%=(STRAT.RECOUNT%-31%)/9%+1%

	GOTO BAD_STRATEGY_STEP   IF T.STEP%>T%
	RETURN
5000
 PERFORM_OPERATION:
	GOSUB EVALUATE_CONDITION
	RETURN   UNLESS DO.IT%		! Return unless we should do it

	IF OPERATION%>127%			! Change client?
	THEN 	AC.MID$=CHR$(OPERATION%-128%)
		OPERATION%=38%			! Fake the operation
	END IF
	! Debt specific operation?
	GOTO DEBT_SPECIFIC_OPERATIONS   IF OPERATION%=54%
	GOTO OPERATION_BRANCH   IF OPERATION%<34% OR OPERATION%>38%

	! NOTE ** NOTE ** NOTE ** Below are a series of debt specific
	! OPERATIONS.  These OPERATIONS will perform on all debts on the
	! debtor *unless* the CONDITION governing this step was debt
	! specific... in that case, the OPERATION is only performed on
	! debts that meet the CONDITION...
 DEBT_SPECIFIC_OPERATIONS:
	IF DEBT.SPECIFIC%			! Debt specific CONDITION?
	THEN	DEBT.E%=GETBYRFA(DEBT.CHAN,EQ.SINGLE.WAIT,GETRFA(DEBT.CHAN))
		UNTIL DEBT.E% OR DEBT::DEBTOR<>MAST::DEBTOR
			IF DO.IT%
			THEN	GOSUB OPERATION_BRANCH
				DEBT.E%=UPDREC(DEBT.CHAN,0%)
			END IF
			IF DEBT::SEQ="255"C	! Just did last debt?
			THEN	DEBT.E%=11%	! Yes. Force loop exit
			ELSE	DEBT.E%=GETREC(DEBT.CHAN,NX.SINGLE.WAIT,0%,"")
			END IF
			ITERATE   IF DEBT.E% OR DEBT::DEBTOR<>MAST::DEBTOR
			DO.IT%=FALSE		! Assume we won't do it.
			GOSUB CONDITION_BRANCH	! Will we?
		NEXT
		UNLOCK #DEBT.CHAN   UNLESS DEBT.E%
		! Recompute PRICLI for OPERATION 38
		IF OPERATION%=38%
		THEN	CALL PRICLI
			GOSUB GET_PRIMARY_CLIENT   UNLESS MAST::PRI.CLI.OFF.NUM=CLI::KEY0
		END IF

	ELSE	! Wasn't a debt specific condition, so do all of the debts...
		DEBT.E%=GETREC(DEBT.CHAN,EQ.SINGLE.WAIT,0%,MAST::DEBTOR)
		UNTIL DEBT.E% OR DEBT::DEBTOR<>MAST::DEBTOR
			GOSUB OPERATION_BRANCH
			DEBT.E%=UPDREC(DEBT.CHAN,0%)
			IF DEBT::SEQ="255"C	! Just did last debt?
			THEN	DEBT.E%=11%	! Yes. Force loop exit
			ELSE	DEBT.E%=GETREC(DEBT.CHAN,NX.SINGLE.WAIT,0%,"")
			END IF
		NEXT
		UNLOCK #DEBT.CHAN   UNLESS DEBT.E%
		IF OPERATION%=38%
		THEN	MAST::PRI.CLI.NUM=OPER.QUAL$
			MAST::PRI.CLI.OFF=AC.MID$
			GOSUB GET_PRIMARY_CLIENT
		END IF
	END IF
	RETURN

 O_BAD:	CALL LOG("%-5000 BAD OPERATION ("+NUM1$(OPERATION%)+")")
	GOSUB LOG_VITAL_STATISTICS
	!OPERATION.DONE%=FALSE (already the case)
	RETURN

 O_END:					! At end of strategy?
	OPERATION.DONE%=TRUE		! Yes. hang it out here (forever)
	OK.TO.BUMP.STEP%=FALSE		! Can't let step change

	! If this is a precollect account then if we are at the end
	! of the main precollect strategy we want to transfer him to
	! a real collection situation.  Otherwise, just let it hang out
	! where it is.
	IF PRECOLLECT% AND STRATEGY%=100%
	THEN	GOSUB PRECOLLECT_TRANSFER
		GOSUB GET_PRIMARY_CLIENT
		RETURN
	END IF

	! Return unless at end of promise/contract/PDC strategy...
	! ** OR ** at end of broken promise strategy... (02/03 RAB)
	RETURN   UNLESS INSTR(0%,"/34/35/37/38/103/105/106/","/"+NUM1$(STRATEGY%)+"/")
	CALL LOG("O_END: "+NUM1$(STRATEGY%)+":"+CEXPAND(ZERO.1,MAST::DEBTOR)) &
							IF DEBUG% AND 1024%
	IF MAST::D.PROM>TODAY$
	THEN	CALL LOG("O_END: PREMATURE PROMISE EXPIRATION:"+	&
			CEXPAND(ZERO.1,MAST::DEBTOR)+"...RECYCLED!")
		STEP%=0%
		TYPE.TO.DELETE$="CDHWLZ"
		GOSUB DELETE_PENDING_ACTIVITY
		ACT2::WHOLE=ACTZ::WHOLE
		ACT2::DEBTOR=MAST::DEBTOR
		ACT2::CODE="D"
		ACT2::D.NEXT.ACT=MAST::D.PROM
		GOSUB PUT_ACTIVITY
		RETURN
	END IF

	IF MAST::AMT.PROM*KEPT.PCNT<=MAST::PROM.ACCUM
	THEN	CALL LOG("O_END: *PROMISE KEPT*")   IF DEBUG% AND 1024%
		CALL CMPINC(MAST::PROM.KEPT)
		MAST::PROM.ACCUM=MAX(MAST::PROM.ACCUM-MAST::AMT.PROM,0)
		IF MAST::PROM.DAYS=ZERO.1
		THEN	TYPE.TO.DELETE$="CDHWLZ"
			GOSUB DELETE_PENDING_ACTIVITY
			GOSUB O_REVERT
			OPERATION.DONE%=FALSE
		ELSE	! Restore step in case it moved...
			STEP%=0%
			TYPE.TO.DELETE$="WDL"	! Remove skd waits, prom due, letters
			GOSUB DELETE_PENDING_ACTIVITY
			GOSUB ROLL_CONTRACT_FORWARD
		END IF
	ELSE	OPERATION.DONE%=FALSE
		IF STRATEGY%=38% OR STRATEGY%=105%	! Broken
		THEN	GOSUB O_REVERT
			RETURN
		END IF
		IF STRATEGY%=34%		! PDC
		THEN	CALL LOG("WARNING: Unposted PDC "+CEXPAND(ZERO.1,MAST::DEBTOR))
			GOSUB O_REVERT
			RETURN
		END IF
	
		! Can't hang out... must go to "broken"...
		STRATEGY%=38%
		STRATEGY%=105%   IF PRECOLLECT%
		STEP%=0%
	END IF
	RETURN

 O_WAIT:OPERATION.DONE%=TRUE		! Hang it out here for awhile
	D%=EXPAND(OPER.QUAL$)		! Get (calendar) days from strategy
	ACT2::WHOLE=ACTZ::WHOLE		! Clear buffer
	ACT2::DEBTOR=MAST::DEBTOR	! Load debtor number
	ACT2::CODE="W"			! Wait
	ACT2::D.NEXT.ACT=DAYADD(D%,TODAY$)
	GOSUB PUT_ACTIVITY
	RETURN

 O_LETTER:
	RSET LETTER$=OPER.QUAL$		! Get letter from strategy (must rset)
	LETP::USER="SCHD"		! Scheduler generated letter
	GOSUB SEND_A_LETTER
	RETURN

 O_SERIES:
	S%=EXPAND(OPER.QUAL$)	! Get series from strategy
	IF S%					! Hardcoded in strategy?
	THEN	T$=CHR$(S%)			! Yes.
	ELSE	GOSUB GET_PRIMARY_CLIENT   UNLESS MAST::PRI.CLI.OFF.NUM=CLI::KEY0
		T$=CLI::SMALL.LET		! Assume small
		T$=CLI::LARGE.LET		! Large ?		&
		   IF MAST::CURR.BAL>INT(CLI::SMALL.AMT*100.+.0001)
	END IF

	E%=0%
	E%=GETREC(LETS.CHAN,EQ.REGARD,0%,T$)   UNLESS LETS::KEY0=T$
	IF E%					! Error?
	THEN	CALL LOG("%-5000 ERROR ("+NUM1$(E%)			&
			+") GETTING SERIES ("+NUM1$(S%)+")")
		GOSUB LOG_VITAL_STATISTICS
		RETURN
	END IF

	BASE.DATE$=ACT::D.NEXT.ACT
	MAST::LAST.SERIES=T$
	MAST::LAST.LS.LTR=ZERO.2

	T%=EXPAND(LETS::AFTER.DAYS(0%))			! Expand wait days
	BASE.DATE$=DAYADD(T%,BASE.DATE$)   IF T%	! Add to base date
	BASE.DATE$=TODAY$   IF BASE.DATE$<TODAY$

	! We have to skip the first letter if the activity is newbiz.
	! This is because the debt(s) send the first letter.
	IF ACT::CODE="N"			! Newbiz initiated?
	THEN	S%=1%				! Yes. Debt sent first letter
		MAST::LAST.LS.LTR=LETS::LETTER(0%)
	ELSE	IF BASE.DATE$>TODAY$		! 1st notice date in future?
		THEN	BASE.DATE$=ACT::D.NEXT.ACT	! Yes. Reset date and
			S%=0%				! send it below.
		ELSE	LSET LETTER$=LETS::LETTER(0%)	! No. Send it now.
			CALL LOG("!-5000 SENDING FIRST LETTER IN SERIES") &
						   IF DEBUG% AND 16%
			LETP::USER="SCHD"
			MAST::LAST.LS.LTR=LETTER$
			GOSUB SEND_A_LETTER
			S%=1%			! Send the rest below
		END IF
	END IF
	ACT2::WHOLE=ACTZ::WHOLE
	ACT2::DEBTOR=MAST::DEBTOR

 O_SERIES_LOOP:
	FOR SER.I%=S% TO 9%
		EXIT O_SERIES_LOOP   IF LETS::LETTER(SER.I%)=ZERO.2
		T%=EXPAND(LETS::AFTER.DAYS(SER.I%))	! Expand wait days
		BASE.DATE$=DAYADD(T%,BASE.DATE$)   IF T%! Add to base date
		IF BASE.DATE$>TODAY$			! Still in the future?
		THEN	ACT2::LETTER=LETS::LETTER(SER.I%)
			ACT2::D.NEXT.ACT=BASE.DATE$
			ACT2::FLAGS="1"C		! Set series flag
			ACT2::CODE="L"
			ACT2::USER="SCHD"
			CALL LOG("!-5000 QUEUING LETTER IN SERIES ("	&
				+CEXPAND(ZERO.1,LETS::LETTER(SER.I%))+")" ) &
					   IF DEBUG% AND 16%
			GOSUB PUT_ACTIVITY
		ELSE	LSET LETTER$=LETS::LETTER(SER.I%) ! No. send it now
			CALL LOG("!5000 SENDING NOTICE")		&
						   IF DEBUG% AND 16%+32%+64%
			LETP::USER="SCHD"
			MAST::LAST.LS.LTR=LETTER$
			GOSUB SEND_A_LETTER
			BASE.DATE$=TODAY$
		END IF
	NEXT SER.I%
	RETURN

 O_PURGE:
	OPERATION.DONE%=TRUE		! Yes. hang it out here (forever)
	OK.TO.BUMP.STEP%=FALSE		! Can't let step change
	DELETE.ACT.REC%=TRUE
	! To process this, a recursive call to WHATS_NEXT could be done,
	! But recursion was removed to save on programmer sanity... hehehe
	IF STRATEGY%<>PURGE.RETURN 		 ! Already in purge strategy?
	THEN	STRATEGY%=PURGE.RETURN	 	! No, put it there
		STEP%=0%
		ARCHIVE%=TRUE
		OK.TO.BUMP.STEP%=FALSE
	END IF

	! Ensure that the current activity record gets blown away...
	TYPE.TO.DELETE$=""			! Setup to delete other activity
	GOSUB DELETE_PENDING_ACTIVITY		! Delete it all

	! If the status is not a (R)eturn or (C)ancel type already, put in
	! AEX or SKP status codes.
	IF NOT CLOSED%
	THEN	PREV.STATUS%=STATUS.INDEX%
		! Change based on mail-return bit...
		MAST::STATUS1=CHOICE(MAST::STATUS2,1%,1%,"AEX,SKP")
		GOSUB EVALUATE_STATUS
		GOSUB AUDIT_STATUS
		GOSUB STATUS_CHANGED
	END IF
	RETURN

 O_COLLECTOR:
	D%=EXPAND(OPER.QUAL$)		! Get (cycle) days from strategy
	MAST::COL.DAYS=CHR$(D%-1%)	! Indicate number of days to send
	GOTO FINISH_GIVING_TO_COLLECTOR

 O_BLITZ:
	BLITZ%=TRUE
 O_QUEUE_ONE:
	PRIORITY%=EXPAND(OPER.QUAL$)	! Get priority
	MAST::COL.DAYS=ZERO.1		! One day
	GOTO FINISH_GIVING_TO_COLLECTOR
	
 O_FORCECOL:
	MAST::COL.DAYS="128"C		! Indicate "forced"
	! Fall through...

 FINISH_GIVING_TO_COLLECTOR:
	OPERATION.DONE%=TRUE		! Hang it out here for awhile

	GOSUB PUT_IN_QUEUE
	BLITZ%=FALSE
	ACT2::WHOLE=ACTZ::WHOLE
	ACT2::DEBTOR=MAST::DEBTOR
	ACT2::D.NEXT.ACT=TOMORROW$	! Tickler queued for tomorrow
	ACT2::CODE="C"			! Code "C" keeps him re-scheduling
	ACT2::COL.OFF=MAST::COL.OFF	! Put these in place for statistics
	ACT2::COL.NUM=MAST::COL.NUM
	GOSUB PUT_ACTIVITY
	RETURN

 O_STEP:T.STEP%=EXPAND(OPER.QUAL$)-1%	! Get target step from strategy
	T.STRAT%=STRATEGY%			! Same strategy
	GOTO FINISH_STRATEGY_STEP_CHANGE

 O_GOTO:	! Goto a specific strategy and step
	T.STRAT%=ASC(L1.OPER.QUAL$)
	T.STEP%=ASC(RIGHT$(OPER.QUAL$,2%))-1%
	T.STEP%=0%   IF T.STEP%<0%
	! need to subtract one on the step
	! because scheduler is zero based but strategy
	! maint starts at one.
	GOTO FINISH_STRATEGY_STEP_CHANGE

 O_STRATEGY:
	T.STRAT%=EXPAND(OPER.QUAL$)	! Get target strategy from strategy
	T.STEP%=0%			! First step
	! Fall through...

 FINISH_STRATEGY_STEP_CHANGE:
	GOSUB VALIDATE_STRATEGY_STEP
	RETURN   UNLESS VALID%
	STRATEGY%=T.STRAT%
	STEP%=T.STEP%

	ARCHIVE%= STRATEGY%=45% OR STRATEGY%=46% OR STRATEGY%=107%
	! Once we have validated the strategy/step, we can't let the
	! Step get bumped, else the target step will not get executed...
	OK.TO.BUMP.STEP%=FALSE
	RETURN

 O_LOG:					! Make a log entry...
	T$="XX"
