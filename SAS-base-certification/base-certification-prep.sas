* base SAS certification;
* verify code that reads data;
libname cert 'C:\Users\basil\Google Drive (basil.okola@student.uhasselt.be)\MSc. Stats Hasselt\SAS\base-guide-practice-data\cert';
options obs=5;
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
proc import datafile = 'C:\Users\basil\Google Drive (basil.okola@student.uhasselt.be)\MSc. Stats Hasselt\SAS\base-guide-practice-data\cert\class.txt'
	dbms=dlm
	out = cert.class
replace;
delimiter ='09'x; *tab separator in ascii form;
getnames = yes;
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
	format date2 MMDDYY10.;
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
