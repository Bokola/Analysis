%let home = C:\Users\basil\Analysis\SAS-advanced-certification;
%let data = C:\Users\basil\Analysis\SAS-advanced-certification\data;
%let output = C:\Users\basil\Analysis\SAS-advanced-certification\output;

libname _all_ clear;
libname d "&data";
*libname dd "&data";
libname o "&output";
ods listing;

/* chap 7: Macros */
* use symbolgen option to debug;
options symbolgen;
%let cartype = Wagon;
proc print data = sashelp.cars;
	var make model type msrp;
	where type = "&cartype";
run;
option nosymbolgen;
* using put to debug;
%put car = &cartype;
* delimiting a macro variable with a period;
%let cartype = Wagon;
%let lib = sashelp;
title "&cartype.s from &lib..cars table";
proc freq data = &lib..cars;
	tables origin/nocum;
	where type = "&cartype";
run;
/* chap8: Text handling with macros */

*substr;
proc print data=d.schedule;
where begin_date between
"30%substr(&sysdate9, 3)"d and
"&sysdate9"d;
title ;
run;
* index function;
%let a = a very long text;
%let b = %index(&a, v);
%put The charater v appears at position &b;
*scan;
%let a=one:two-three four;
%put First word is %scan(&a,1);
%put Second word is %scan(&a,2, :-);
%put Last word is %scan(&a,-1);

*%sysfunc - runs sas macro functions;
%let name = WILLIAM smitH;
%put %sysfunc(propcase(&name));

*eval;
%put %eval(12+45);
* for floats use sysevalf;
%put %sysevalf(12.0+34);
* FIGUREIT performs all types of conversions for %SYSEVALF values;
%macro figureit(a, b);
	%let y = %sysevalf(&a+&b);
	%put the result with SYSEVALF is : &y;
	%put the boolean value is: %sysevalf(&a + &b, boolean);
	%put the ceil valus is: %sysevalf(&a + &b, ceil);
	%put the floor value is: %sysevalf(&a + &b, floor);
	%put the integer value is: %sysevalf(&a + &b, int);
%mend figureit;

%figureit(100, 1.597);

* mask special characters with %str;
%let text = %str(Joan%'s Report);
%let text = Joan%str(%')s Report;
proc print data = d.courses;
 	where days >3;
	title "&text";
run;

* use %nrstr function to escape & and %;
%let period = %str(May&Jun);
%put period resolves to: &period;
%let Period=%nrstr(May&Jun);
%put Period resolves to: &period;
*EXAMPLE: USING THE %SUPERQ FUNCTION;
data _null_;
call symputx('mv1','Smith&Jones');
call symputx('mv2','%macro abc;');
run;
%let testmv1=%superq(mv1);
%let testmv2=%superq(mv2);
%put Macro variable TESTMV 1 is &testmv1;
%put Macro variable TESTMV2 is &testmv2;
*EXAMPLE: USING THE %BQUOTE FUNCTION;
data _null_;
call symputx('text',"Sally's Seashell Store at Old Towne's Beach");
run;
data _null_;
put "%bquote(&text)";
run;

*Q macro functions;
* example %QUPCASE;
%let a=%nrstr(Address&name);
%put QUPCASE produces: %qupcase(&a);
*EXAMPLE: USING THE %QSYSFUNC FUNCTION;
title "Report produced on %sysfunc(left(%qsysfunc(today(), worddate.)))";
* creating macro variables during sql proc with select into;
/*proc sql;
	select sum(fee) format = dollar10.
		into: totalfee trimmed
		from d.all;
quit;

proc means data = d.all sum maxdec = 0;
	class course_title;
	var fee;
	title "Grand Total for all courses is &totalfee";
run;*/

* Example: Creating Variables with the INTO Clause;
proc sql noprint;
	select course_code, location, begin_date format=mmddyy10.
		into: crsid1- ,
		: place1- ,
		: date1- 
		from d.schedule
		where year(begin_date) = 2019
		order by begin_date;
quit;

%put There are &sqlobs courses in 2019;
%put _user_;
* The %PUT statement at the end of the program shows 
the names and values of all the macro variables that
are created in the SELECT statement;

* Example: Creating a Delimited List of Values;
proc sql noprint;
	select distinct location
		into: sites separated by ' '
		from d.schedule;
quit;
%put Sites: &sites;
*EXAMPLE: USING THE CALL SYMPUTX ROUTINE;
/*%let crsnum=3;
data revenue;
set certad v.all end=final;
where course_number=&crsnu m;
total+1;
if paid= 'Y' then paidup+1;
if final then do;
call symputx(' crsname',course_title);
call symputx('date',put(begin_date,mm ddyy10.));
call symputx('due',put(fee*(total-paidup),dolla r8.));
end;
run;
proc print data=revenue;
var student_name stud ent_company paid;
title "Fee Status for &crsname (#&crsn um) Held &date";
footnote "Note: &due in Unpaid Fees";
run;*/

* EXAMPLE: CREATING MULTIPLE MACRO VARIABLES WITH CALL SYMPUTX;
* The macro processor creates a new macro variable for each
course_title;
data _null_;
	set d.courses;
	call symputx(course_code, course_title);
run;
%put _user_;

* Referencing Macro Variables Indirectly;
data _null_;
	set d.courses;
	call symputx(course_code, (course_title));
run;

%let crsid = C002;
proc print data = d.schedule noobs label;
	where course_code = "&crsid";
	var location begin_date teacher;
	title1 "Schedule for ????";
	*Here is the correct solution:;
title1 "Schedule for &&&crsid";
run;
* page 315/ 577;
* Example: Creating a Series of Macro Variables;
options symbolgen;
data _null_;
set d.schedule;
call symputx(cats('teach',course_number),teacher);
run;

%let crs=3;
proc print data=d.register noobs;
where course_number=&crs;
var student_name paid;
title1 "Roster for Course &crs";
title2 "Taught by &&teach&crs";
run;

/* chap9: Working with macro programs */
%let dat = sashelp.cars;
%macro printit;
	proc print data = &dat (obs = 5);
		title "Listing of &dat data set";
	run;
	%mend printit;
	%printit *no need for semi-colon;

* macros with positional arguments;
%macro printdsn(dsn);
	proc print data = &dsn;
	title "Listing of %upcase(&dsn) data set";
	run;
%mend printdsn;

%printdsn(d.schedule)

* Example: Using Keyword Parameters to Create Macro Variables;
%macro printdsn(dsn = d.schedule, vars = course_code teacher);
	proc print data = &dsn;
	var &vars;
	title "Listing of %upcase(&dsn) data set";
	run;
%mend printdsn;
*run with default values;
%printdsn()
* run with set of position arguments;
%printdsn(vars=teacher course_code begin_date, dsn=d.schedule)
*Example: Using Mixed Parameters to Create Macro Variables;
*You can use a combination of positional and keyword parameters
to create the macro variables in the Printdsn macro definition;
%macro printdsn(dsn, vars=course_title course_code days);
	proc print data=&dsn;
		var &vars;
	title "Listin g of %upcase(&dsn) data set";
	run;
%mend;
%printdsn(d.schedule, vars=teacher location begin_date)
*EXAMPLE: USING %GLOBAL STATEMENT;
%macro printdsn;
	%global dsn vars;
	%let dsn=d.courses;
	%let vars=course_title course_code days;
	proc print data=&dsn;
		var &vars;
	title "Listin g of &dsn data set";
	run;
%mend printdsn;
%printdsn
* Note: You use the %SYMDEL statement to delete a macro variable 
	from the global symbol table;
%symdel dsn;
* debug macro with mprint option;
%macro prtlast;
	proc print data = &syslast (obs = 5);
		title "Llisting of &syslast dataset";
	run;
%mend prtlast;

data sales;
	price_code = 1;
run;
options mprint;
%prtlast
* conditional execution;
*Example: Using %IF-%THEN, %DO-%END with IF-THEN Statements;
data sports;
	set sashelp.cars;
	where lowcase(type) = "sports";
	avgMPG = mean(mpg_city, mpg_highway);
run;
%if &syserr ne 0 %then %do;
	%put Error: The rest of the program will not run;
%end;
%else %do;
title "SPorts Cars";
proc print data = sports noobs;
	var make model avgmpg msrp;
run;
%end;
* Example: Controlling Text Copied to the Input Stack;
/*
%macro choice(status);
	data fees;
		set d.all;
		%if &status = PAID %then %do;
			where paid = "Y";
			keep student_name course_code begin_date totalfee;
		%end;
		%else %do;
			where paid = "N";
			keep student_name course_code
				begin_date totalfee latechg;
				latechg = fee * .10;
		%end;
		if location = 'Boston' then totalfee = fee * 1.06;
		else if location = 'Seattle' then totalfee = fee * 1.025;
		else if location = "Dallas" then totalfee = fee * 1.05;
	run;
	%mend choice
options mprint mlogic;
%choice(PAID)
*/
* EXAMPLE: USING MLOGIC SYSTEM OPTION;
data sales;
	price_code = 1;
run;
options nomprint mlogic;
%prtlast

* iterative processing;
* Example: Using the %DO Statement;
proc sql;
	select teacher 
		into: teach1-
		from d.schedule;
run;

%macro putloop;
	%local i;
	%do i=1 %to &sqlobs
		%put TEACH&i is &teach&i;
	%end;
%mend;
%putloop
option nomprint nomlogic nosymbolgen;
proc sql noprint;
select teacher
into :teach1-
from d.schedule;
run;
%macro putloop;
	%local i;
	%do i=1 %to &sqlobs;
		%put TEACH&i is &&teach&i;
	%end;
%mend;
%putloop
/*data _null_;
       set d.schedule end=no_more;
       call symput('teach'||left(_n_),(trim(teacher)));
       if no_more then call symput('count',_n_);
    run;

    %macro putloop;
       %local i;
       %do i=1 %to &count;
          %put TEACH&i is &&teach&i;
       %end;
    %mend putloop;

    %putloop
proc sql feedback;
	select * from d.schedule;
quit;
*/

* Example: Generating Complete Steps;
*Suppose you want to generate a roster for each of the 18 
classes that you have. You can use a %DO statement to
create a loop that creates a roster for each class;
%macro rosters;
	%do class=1 %to 6;
		title "Rosters for class #&class";
		proc print data = d.schedule;
		where course_number = &class;
	run;
	%end;
	%mend;
%rosters;

/* chap 10: Advanced macro techniques */

* %include statement to store macr definitions externaly;
%macro prtlast;
%if &syslast ne _NULL_ %then %do;
	proc print data = &syslast (obs = 5);
	title "Listing of &syslast data set";
run;
%end;
%else
	%put No data set has been created yet.;
%mend;
filename prtlast "C:\Users\basil\Analysis\SAS-advanced-certification\prtlast.sas";
*%let f = \prtlast.sas;
option nosymbolgen nomprint;
%include prtlast/source2;
* you need to create the file first;
proc sort data = d.courses out = work.bydays;
	by days;
run;
%prtlast
* EXAMPLE: ACCESSING AUTOCALL MACROS;
options mautosource sasautos = ("&home", sasautos);
%prtlast
* To see what SASAUTOS is set to, run the following statements;
%put %sysfunc(getoption(sasautos));
%put %sysfunc(pathname(sasautos));
* data driven macro calls using dosuble function in data step;
%macro delayreport(empid);
	title "Flight delays for Employee &empid";
	proc sql;
		select delaycategory, count(*) as count
			from d.flightdelays d
			inner join
			d.flightschedule s
			on s.date = d.date and s.flightnumber = d.flightnumber
			where empid = "&empid"
			group by delaycategory;
		quit;
	title;
	%mend;
proc print data = d.flightschedule (obs = 5) noobs;
run;
* The DOSUBL function uses the value found in EmpID concatenated between
	'%DelayReport (' and ')' to generate a valid macro call;
data _null_;
	set d.flightcrewnew;
	rc = dosubl(cats('%DelayReport(', empid, ')'));
run;

/* chap 11: Defining and processing arrays */
proc print data = d.patdata (obs = 5) noobs;
run;

data highcounts;
	set d.patdata;
	array health[5] weight -- bp;
	do i = 1 to 5;
		if health[i] = "High" then highcount+1;
	end;
	run;

proc print noobs;
run;
* can above be done for each patient name;

* Example: Assigning Initial Values to Arrays;
data report (drop = i);
	set d.qsales;
	array sale[4] sales1-sales4;
	array goal[4] (9000 9300 9600 9900); /* initial values */
	array achieved[4];
	do i = 1 to 4;
		achieved[i] = 100 * sale[i] / goal[i];
	end;
run;
proc print data = report noobs;
run;
* example temporary array elements: rotate data;
proc print data = d.qtrsales (obs = 5) noobs;
run;

data yrsales;
	set d.qtrsales;
	array Yr[4] salesq1-salesq4;
	do quarter = 1 to 4;
		sales= Yr[quarter];
		output;
	end;
run;
proc print data = yrsales (obs =10);
run;

* two dim arrays;
* Suppose you have the Certadv.StCoup data set, which contains unique data about
a store�s most recent customer order. You are asked to create a coupon value
for each of the store�s customers. Customers will receive coupons ranging 
from 10% to 40% off their next purchase based on the type and size of their
last order;
proc print data = d.stcoup  (obs = 4) noobs;
run;

data customercoupons;
	array cpnvalue[3,4] _temporary_ (.10, .15, .20, .25,
									.30, .40, .10, .15,
									.20, .25, .15, .10);
	set d.stcoup (keep = customerid ordertype quantity);
	couponvalue = cpnvalue[ordertype, quantity];
	format couponvalue percent10.;
run;
title "Coupon for October 2019";
proc print data = customercoupons;
run;
title;
* Example: Creating a Two-Dimensional Array to Perform Table Lookup;
* Suppose you are asked to combine two SAS data sets, Certadv.US_Goals and
Certadv.US_Sales, and find the difference between the quarterly sales amount
and the quarterly goal;
proc print data = d.us_sales (obs = 4) noobs;
run;
proc print data = d.us_goals (obs = 4) noobs;
run;
data diffsales;
	array yrsales[2014:2018, 4] _temporary_;
	if _N_ = 1 then do yr = 2014 to 2018;
		set d.us_sales;
		array qtrsal[4] salesq1-salesq4;
		do qtr = 1 to 4;
			yrsales[yr, qtr] = qtrsal[qtr];
		end;
	end;
	set d.us_goals;
	sales = yrsales[year, qtrnum];
	difference = sales - goal;
	drop yr qtr salesq1 - salesq4;
run;
proc print data = diffsales;
	format goal sales difference dollar14.2;
run;
