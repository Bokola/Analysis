/* Using and creating Macro variables in SQL */
%let path=c:/users/basil/google drive (basil.okola@student.uhasselt.be)/msc. stats hasselt/sas/advanced sas/sql;
 
libname sq "&path/data"/*Complete the LIBNAME Statement*/;
proc sql;
	select avg(PopEstimate1)
		into: AvgEst1
		from sq.statepopulation;
quit;
* display macro variable value;
%put &=AvgEst1;
* or;
%put AVG value is &AvgEst1;
* to format a macro fariable in the title you can use
%left(%qsysfunc(putn(&AvgEst1, dollar16.)));
title "Average Estimated population for Next Year: 
%left(%qsysfunc(putn(&AvgEst1, dollar16.)))";
options symbolgen; /* print what the macro variable evaluates to */
proc sql;
	select name, PopEstimate1
		from sq.statepopulation
		where PopEstimate1 > &AvgEst1;
quit;
options nosymbolgen;
title;
/* creating multiple Macro Variables */
proc sql;
	select avg(PopEstimate1), min(PopEstimate1), max(PopEstimate1),
		count(PopEstimate1)
		into: AvgEst1, :MinEst1, :MaxEst1, :TotalCount
		from sq. statepopulation;
quit;
* removing leading or trailing blanks;
proc sql;
	select avg(PopEstimate1)
		into: AvgEst1 trimmed
		from sq.statepopulation;
quit;
/* concatenating values in Macro Variables */
proc sql;
	select quote(Name) 
		into: statelist separated by ","
		from sq.statepopulation
		where division = "3";
quit;
%put &=statelist;
 *title "States in Division 3: &statelist";
 proc sql;
 	create table division3 as
	select * 
		from sq.customer
		where State in (&statelist);
quit;
title;
/* using formats when creating Macro Variables */
* format = $quotew.;
proc sql noprint;
	select Name format = $quote4.
		into: statelist separated by ","
		from sq.statepopulation
		where division = "3";
quit;
%put &=statelist;
