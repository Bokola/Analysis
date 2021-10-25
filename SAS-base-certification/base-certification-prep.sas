* base SAS certification;
* verify code that reads data;
libname cert 'C:\Users\basil\Google Drive (basil.okola@student.uhasselt.be)\MSc. Stats Hasselt\SAS\base-guide-practice-data\cert';
*options obs=5;
proc import datafile = 'C:\Users\basil\Google Drive (basil.okola@student.uhasselt.be)\MSc. Stats Hasselt\SAS\base-guide-practice-data\cert\boot.csv'
dbms = csv
out = shoes
replace;
getnames = no;
run;

proc print data = work.shoes;
run;

* set option obs = max to read the full data set again;
options obs = max;

* using data step after importing data;
data boots;
	set shoes;
	where var1 = "South America" OR var1 = "Canada";
run;
 proc print data = boots;
 run;

* Using XLSX engine to read;
 libname certxl XLSX 'C:\Users\basil\Google Drive (basil.okola@student.uhasselt.be)\MSc. Stats Hasselt\SAS\base-guide-practice-data\cert\exercise.xlsx';
 data work.stress;
 	set certxl.ActivityLevels; /* read ActivityLevel worksheet */
run;

proc print data = stress (obs = 10);
run;

 data work.stress;
 	set certxl.ActivityLevels; /* read ActivityLevel worksheet */
	where ActLevel = 'HIGH';
run;
* creating Excel worksheets;
libname excelout xlsx 'C:\Users\basil\Google Drive (basil.okola@student.uhasselt.be)\MSc. Stats Hasselt\SAS\base-guide-practice-data\cert\newExcel.xlsx';
data excelout.HighStress;
	set work.stress;
run;

* Chap5: Identifying and correcting SAS language errors;
proc import datafile = 'C:\Users\basil\Google Drive (basil.okola@student.uhasselt.be)\MSc. Stats Hasselt\y2 sem1\LDA\project continuous\data\renal.txt';
	*dbms=tab;
	out = cert.renal
replace;
delimiter ='09'x; *tab separator in ascii form;
getnames = yes;
run;

proc print data = cert.renal (obs = 10);
run;


* using putlog;

data work.grades;
set cert.class;
Homework = Homework*2;
AverageScore = MEAN(Score1 + Score2+ Score3 + Homework);
* use the putlog before if statement to determine where data step
received incorrect instructions;
putlog Name= Score1= Score2= Score3= Homework= AverageScore=;
if AverageScore < 70;
run;

proc contents data = cert.class;
run;

proc print data = cert.class;
run;

* Chap 11: Do loops;
* Using Explicit OUTPUT statements;
data work.earn;
	Value = 2000;
	do year = 1 to 20;
		interest = value * 0.075;
		value + interest;
		output; * creates an obs for each iteration of the do loop;
	end;
run;

proc print data = work.earn;
run;
* Nesting Do loops;
/* compute value of one-year investment that earns 7.5% annual interst
compounded monthly */

data cert.earn;
	capital = 2000;
	do month = 1 to 12;
		interest = capital * (0.075 / 12);
		capital + interest;
		output;
	end;
run;

proc print data = cert.earn;
run;

/* Assume that the same amount of capital is to be added
to the investment each year for 20 years */

data cert.earn2;
	do year = 1 to 20;
		capital = 2000;
		do month =1 to 12;
			interest = capital * (.075/ 12);
			capital + interest;
			output;
		end;
	end;
run;

proc print data = cert.earn2;
run;

/* Iteratively processing observations from a data set */
* Suppose you want to compare how much each CD earns at
maturity with an investment of $5,000;

data cert.compare (drop = i);
	input Type $ 1-7 AnnualRate Months;
    Investment = 5000;
    do i = 1 to Months;
    	Investment + (AnnualRate/12)*Investment;
	end;
    *format Investment dollar8.2;
    DATALINES;
03Month  0.01980  3
06Month  0.02230  6
09Month  0.02230  9
12Month  0.02470 12
18Month  0.02470 18
24Month  0.02570 24
36Month  0.02720 36
48Month  0.02960 48
60Month  0.03445 60

;
run;
proc print data = cert.compare;
run;

* conditionally executing do loops;
/* do untill - when expression evaluates to true, the do loop stops*/
/* Assume you want to know how many years it takes to earn
$50,000 if you deposit $2,000 each year into an account that 
earns 10% interest */
data work.invest;
	do until (capital >= 50000);
		capital+2000;
		capital + capital * .10;
		year+1;
		output;
	end;
run;

proc print data = work.invest;
run;

/* do while expression - stops if the expression evaluates to false */
data work.invest2;
	do while (capital <= 50000);
		capital+2000;
		capital + capital*0.10;
		year+1;
	end;
run;

proc print data = work.invest2;
run;

/* Suppose you also want to limit the number of years you invest
your capital to 10 years */

data work.invest;
	do year = 1 to 10 until (capital >=50000);
		capital+2000;
		capital+capital*.10;
	end;
run;

proc print data = work.invest;
run;

data simple;
length date $15;
input date;
cards; 
10/08/2021
12/08/20
;
run;
/* get contents of a library */
proc contents data = cert._ALL_ nods; * nods suppresses printing of details;
run;


/* chapter 12: formats and informats */

data simple2;
	set simple;
	date2 = input(strip(date), MMDDYY10.);
	*format date2 MMDDYY10.;
	run;

proc print data = simple2;
run;

* defining user formats - specify library;
proc format library =cert;
value gender
1 = 'Male'
2 = 'Female';
value agegroup
13 -< 20 = 'Teen'
20 -< 65 = 'Adult'
65 - HIGH = 'Senior';
value $col
'W' = 'Moon White'
'B' = 'Sky Blue'
'Y' = 'Sunburst Yellow'
'G' = 'Rain Cloud Gray';
run;
options fmtsearch =(cert); * set where to look for formats;

/* displaying user defined formats */
proc format library = cert fmtlib;
run;

/* chap 14: SAS date formats and informats */
* sas stores date valus as numeric count of days since Jan 1, 1960;
data tempdate;
date = input("2018/12/15", YYMMDD10.);
run;

proc print data = tempdate;
run;

/* INTCK function for interval of values calculation */
data intck;
week = intck('week', '31dec2017'd, '01jan2018'd);
months = intck('month', '31dec2017'd, '01jan2018'd);
years = intck('year',  '31dec2017'd, '01jan2018'd);
run;

proc print data = intck;
run;

/* INTNX function for multiples of a given date/time */
data intnx;
targetyear = intnx('year', '20Jul18'd, 3);
targetmonth = intnx('semiyear', '01Jan18'd, 1);
run;
proc print data = intnx noobs;
run;

* with the alignment argument;
/*
BEGINNING Alias: B
MIDDLE Alias: M
END Alias: E
SAME Alias: SAMEDAY or S
*/
data intnxalign;
Monthb=intnx('month','01jan2018'd,5,'b');
Monthm=intnx('month','01jan2018'd,5,'m');
Monthe=intnx('month','01jan2018'd,5,'e');
format monthb monthm monthe worddate12.; * format as Jun 1, 2018 for example;
run;

proc print data = intnxalign noobs;
run;
/*
These INTNX statements count five months from January,
but the returned value depends on whether alignment specifies
the beginning, middle, or end day of the resulting month */

/*DATDIF and YRDIF Functions */

data _null_;
x=yrdif('16feb2016'd,'16jun2018'd,'30/360');
put x; * print x to console;
run;

/* TEXT manipulation functions */

/* find nth character using SCAN */
data _null_;
s = "this is a sentence";
x = scan(s, 4, " ");
put x;
run;

/* find a pattern with substr */
data _null_;
s = "this is a sentence";
x = substr(s, 1, 4);
put x;
run;

/* find and replace with substr 
syntax: substr(char, from, n) = "whatever" 
*/
data substr;
s = "this is a sentence";
run;

data _null_;
set substr;
substr(s, 1, 4) = "";
put s;
run;

/* search char value by index function */
data _null_;
set substr;
this = index(s, "this");
put this;
run;

/* chap15: proc means and proc freq */
*output statement;
/* You can use the NOPRINT option in the PROC MEANS statement
to suppress the default report */
proc means data = cert.compare maxdec = 2;
var investment;
class type;
output out = work.investment_by_type
mean = avgInv
min = minInv;
run;

%let title = investments by type;
proc print data = work.investment_by_type ;
	title1 "&title";
run;

* proc freq;
proc freq data = cert.compare;
run;
* suppress display of cumulative freq using nocum option;
proc freq data = cert.compare;
tables annualrate / nocum;
run;

/* chap16: Creating output */
* (a) HTML output with ods html;
%let lib = C:\Users\basil\Google Drive (basil.okola@student.uhasselt.be)\MSc. Stats Hasselt\SAS\base-guide-practice-data\cert;
%let out = out.html;
%let filepath = &lib.&out; * combine two macro objects to extend path variable;
%put &filepath; * print to log;
%let toc = \toc.html;
%let frame = \frame.html;
%put "&lib&toc";

*ods html body = %sysfunc(quote(&lib));

/* specifications. However, it is simpler to specify the path
once in the PATH= option and to specify URL=NONE.*/
ods html path = "&lib" file = "&out" (url = none); *using the two macros to specify path and file;
proc print data = cert.compare;
run;
ods html close;
ods html;* path = "%qsysfunc(pathname(work))"; * get html output to default work library;
/* creating html output with table of contents */
* you need only specify path once on the ods statement;
ods html path = "&lib" file = "data.html"
	contents = "&toc"
	frame = "&frame";
proc print data = cert.compare;
	label AnnualRate = "Annual interest rate";
run;
proc freq data = cert.compare;
	tables annualrate;
run;
ods html close;

ods html;* path = "%qsysfunc(pathname(work))"; * get html output to default work library;

* Adding URLs = Uniform Resource locater;
/* creating html output with table of contents */
* you need only specify path once on the ods statement;
ods html path = "&lib" file = "data.html" (url = 'data.html')
	contents = "&toc" (url = 'toc.html')
	frame = "&frame";
proc print data = cert.compare;
	label AnnualRate = "Annual interest rate";
run;
proc freq data = cert.compare;
	tables annualrate;
run;
ods html path = "%qsysfunc(pathname(work))"; * get html output to default work library;
ods html close;

/* change output style */

* list all sas style templates;
proc template;
	list styles/store=sashelp.tmplmst;
run;

* do not enclose style name in quotation marks;

* you need only specify path once on the ods statement;
ods html path = "&lib" file = "data.html"  (url = none) style = Word
	contents = "&toc"
	frame = "&frame";
proc print data = cert.compare;
	label AnnualRate = "Annual interest rate";
run;
proc freq data = cert.compare;
	tables annualrate;
run;
ods html close;

ods html path = "%qsysfunc(pathname(work))"; * get html output to default work library;

/* PDF output with ODS PDF */
%let sample = \sample.pdf;
%let sampleTOC = \sample_with_TOC
ods html close;
ods pdf file = "&lib&sample"; * using macros to specify file path;
proc freq data = sashelp.cars;
	tables origin*type;
run;
ods pdf close;

/* creating printable table of contents with contents = yes option */
ods html close;
title "create a Table of Contents";
options nodate;
ods pdf file = "&lib&sampleTOC" contents = yes bookmarklist = hide style = journal;
proc freq data=sashelp.cars;
	tables origin*type;
run;
proc print data=sashelp.cars (obs = 15);
run;
ods pdf close;
ods html path = "%qsysfunc(pathname(work))";

/* RTF / word output with ODS RTF */
%let rtf = SampleRTF;
title ;
ods html close;
ods rtf file = "lib&rtf" style = journal;
proc freq data = sashelp.cars;
	tables origin*type;
run;
ods rtf close;
ods html path = "%qsysfunc(pathname(work))";


proc contents data = sashelp.cars;
run;
proc sort data = sashelp.cars out = cert.cars;
by make;
run;

/* EXCEL output with ODS excel */

* customizing your excel output;
%let xlsx = \excelsheet.xlsx;
ods excel file = "&lib&xlsx";
	options (sheet_interval = "bygroup" /*creates new worksheet for each bygroup */
		suppress_bylines = 'yes' /* suppresses the by lines for each by group */
		sheet_label = 'Make' /* customizes the worksheet label */
		embedded_titles='yes');
title 'Weight by Make'; /* embeds the title that is created by title statement */
proc means data = cert.cars;
	by make;
	var weight;
run;
ods excel close;
