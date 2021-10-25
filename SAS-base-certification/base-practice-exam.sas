libname cert "C:\Users\basil\Google Drive (basil.okola@student.uhasselt.be)\MSc. Stats Hasselt\SAS\cert\input";
libname results "C:\Users\basil\Google Drive (basil.okola@student.uhasselt.be)\MSc. Stats Hasselt\SAS\cert\output";
ods html;
/*
1.

This project will use data set cert.input04. At any time, 
you may save your program as program04 in cert\programs.

Write a SAS program that will create the data set results.output04. 

In this program, complete the following mathematical actions,
in the following order:

Round VAR1 and VAR2 to the nearest integer values.
Multiply the rounded VAR1 by the rounded VAR2 and assign the new value to VAR3.
Add VAR12 through VAR19 (8 variables) together, ignoring missing values.
Assign the sum to VAR20.
*/
proc print data = cert.input04;
run;

data results.output04;
	set cert.input04;
	var1 = round(var1);
	var2 = round(var2);
	var3 = var1*var2;
	*format var1 var2 3.;
	var20 = sum(of var9--var12); * sum an array of columns;
run;
proc print data = results.output04;
run;

* extract the 16th obs from the data;
data _16;
	set results.output04 (firstobs = 16 obs = 16);
run;

proc print data = _16;
run;

/*
2.
This project will use data set cert.input08a and cert.input08b.
At any time, you may save your program as program08 in cert\programs.
Both data sets contain a common numeric variable named ID.

Write a program that will use a SAS DATA Step to:

Combine data sets cert.input08a and cert.input08b by matching 
values of the ID variable.
Write only observations that are in both data sets to
a new data set named results.match08.
Write all other non-matching observations from either
data set to a new data set named results.nomatch08.
Exclude all variables that begin with "ex" from results.nomatch08.
*/

proc print data = cert.input08a;
run;
proc print data = cert.input08b;
run;

* sort by id variable before merging;
proc sort data = cert.input08a out = input08a;
by id;
run;

proc sort data = cert.input08b out = input08b;
by id;
run;

* match - merge;
data results.match08 results.nomatch08 (drop = ex:); /* use colon operator to specify 
a prefix */
	merge input08a (in = ina)
		input08b (in = inb);
		by id;
	if ina and inb then output results.match08;
	else output results.nomatch08;
run;
/* How many observations (rows) are in results.match08? */
data _NULL_;
 if 0 then set results.match08 nobs=n;
 put "no. of observations =" n;
 stop;
run;
/* How many variables (columns) are in results.match08? */
proc contents data = results.match08;
run;

proc contents data = results.nomatch08 varnum;
run;

/* Write a SAS program that will:

Create an output data set results.output12.
Read cert.input12 as input.
Increase the salary variable by 5.65% annually until it is greater than $500,000.
Increment the year variable by 1 with each annual increase.
Create an output data set results.output12 that has one observation
for each value of year. Each observation should have a year and salary variable. */

proc print data = cert.input12;
run;
data results.output12;
	set cert.input12;
	do until (salary > 500000);
		salary = salary + (salary*0.0565);
		year +1;
		output;
	end;
run;

proc print data = results.output12;
run;
/* converting types with put */
data _null_;
	x = 50000;
	y = put(x, dollar7.);
	z = put(x, comma5.);
	put y;
	put z;
run;

/* This project will use data set cert.input13. At any time, you may save 
your program as program13 in cert\programs.
This data set contains 1001 observations and 2 variables:

Date1, a numeric variable representing an unformatted SAS date value.
Example: 12001.
Charnum, a character variable representing a monetary amount. Example: $50,000.
Write a SAS program that will:

Save the new data set as results.output13.
Create a new variable Chdate that converts the date1 variable to a character
variable that is in the format ddmonyyyy, such as 11NOV1992.
Create a new variable num1 that converts the Charnum variable to a numeric variable.
Run the program and use additional SAS procedure steps to answer the next 2
questions. */

data results.output13;
	set cert.input13;
	Chdate = put(date1, date9.);
	num1 = input(charnum, comma7.);
run;

proc print data = results.output13 (obs = 10);
run;

/* What is the value of Chdate for observation 52? */
data _52;
	set results.output13 (firstobs = 52 obs = 52);
run;

proc print data = _52;
run;

/* What is the average (mean) of the num1 variable for the entire data set? */
proc means data = results.output13;
var num1;
output out = sumnum1
mean = mean
std = sd
min = min
max = max;
run;

proc print data = sumnum1;
run;

data _ll;
	set sumnum1;
	mean1 = round(mean);
run;

proc print data = _ll;
run;

/* Write a SAS program that will:
Create output data set results.output27a as a subset of cert.input27
where the country variable's value is "US" (any variation of case, such as US or us).

Sort results.output27a:
first by the variable state in ascending order
then by Postal_Code in descending order
and finally by employee_ID in ascending order.
Run the program and use the results to answer the question below.

*/ 

data results.output27a;
	set cert.input27;
	if upcase(country) = "US";
run;

 proc sort data = results.output27a;
 	by state descending postal_code employee_id;
run;

proc print data = results.output27a (firstobs =100 obs = 100) ;
var employee_id;
run;

/* Continuing with the previous program (program27), add a PROC SORT
step that satisfies the following criteria:
Creates the output data set results.output27b from cert.input27.
Sorts the observations by Postal_Code in descending order.
Removes duplicate values of Postal_Code, keeping only the first
occurrence found during the sort.
Run the program and use the results to answer the next 2 questions. */

proc sort data = cert.input27 nodupkeys out = results.output27b;
	by descending postal_code;
run;
/* What is the value of Employee_ID for observation 98 in results.output27b? */
proc print data = results.output27b (firstobs = 98 obs = 98);
	var employee_id;
run;
/*What is the value of Employee_ID for observation 181 in results.output27b? */
proc print data = results.output27b (firstobs = 181 obs = 181);
	var employee_id;
run;

/* This project will use data set cert.input36. At any time,
you may save your program as program36 in cert\programs.

Write a SAS program that will clean the data in cert.input36 as follows:

Step 1:
create a temporary data set, cleandata36.
In this data set, convert all group values to upper case.
Then keep only observations with group equal to 'A' or 'B'.
Step 2:
Determine the MEDIAN value for the Kilograms variable for each group (A,B)
in the cleandata36 data set. Round MEDIAN to the nearest whole number.
Step 3:
create results.output36 from cleandata36
Ensure that all values for variable Kilograms are between 40 and 200, inclusively.
If the value is missing or out of range, replace the value with the MEDIAN
Kilograms value for the respective group (A,B) calculated in step 2.
Run the program and use the results to answer the next 3 questions. 
*/

proc print data = cert.input36 (obs = 10);
run;

data cleandata36;
	set cert.input36;
	group = upcase(group);
	if group IN ("A", "B");
run;

proc means data = cleandata36 p50 nomiss;
	var kilograms;
	class group;
	*output out = mediankilo;
	*p50 = median;
run;

data results.output36;
	set cleandata36;
	if (kilograms < 40 or kilograms >200 or kilograms = .) and group = 'A' 
	then kilograms = 79;
	else if (kilograms < 40 or kilograms >200 or kilograms = .) and group = 'B' 
	then kilograms = 89;
	else kilograms = kilograms;

run;

proc means data = results.output36;
run;
/* How many observations are in results.output36? */
proc contents data = results.output36;
run;
/* What is the MEAN Kilograms value for group=‘A’ 
in the results.output36 data set? */
proc means data = results.output36 mean;
var kilograms;
class group;
run;

/* Open the existing program, program44.sas from folder cert\errors.
At any time, you may save your corrected program as program44 in cert\programs.

This program is intended to:

Create a new data set using the cert.input44 set as input
Drop variables: bp_status, weight_status, and smoking _status.
Create a new column, chol_status, based on the following values of cholesterol:
less than 200: "Safe"
200-239: "High - Borderline"
240 and higher: "High"
Should not calculate chol_status for missing cholesterol values
There are multiple errors in the program. These may be syntax errors,
logic errors, or problems with the program structure. Logic errors 
might not produce messages in the log, but will cause the program to
produce results different than intended.

Correct the errors, run the program, and then use the results to
answer the next 3 questions.
*/

data out;
	length chol_status $32;
	set cert.input44 (drop=bp_status weight_status smoking_status);;
	*if cholesterol =. then chol_status = " ";
	if cholesterol < 200  and cholesterol > . then chol_status='Safe';
	else if cholesterol <= 239 and cholesterol > . then chol_status='High-Borderline';
	else if cholesterol >= 240 and cholesterol > . then chol_status='High';
	
run;

data out2;
	length chol_status $32;
	set cert.input44 (drop=bp_status weight_status smoking_status);;
	if cholesterol =. then delete;
	if cholesterol < 200 then chol_status='Safe';
	else if cholesterol <= 239 then chol_status='High-Borderline';
	else if cholesterol >= 240 then chol_status='High';
	
run;


proc contents data=out;
run;

proc freq data = out; 
	table chol_status;
run;

/* Open the existing program, program48.sas from folder cert\errors.
At any time, you may save your corrected program as program48 in cert\programs.

This program is intended to:
Create 3 groups for Cvar: A-G is Group=1; H-N is Group=2; O-Z is Group=3 .
All variations of the variable should be in the same group, i.e. “A” and “a”
should be in Group=1.
Calculate the average of X and Y by Group.
There are multiple errors in the program. These may be syntax errors, logic errors, or problems with the program structure. Logic errors might not produce messages in the log, but will cause the program to produce results different than intended.

Correct the errors, run the program, and then use the results to answer the
next 2 questions.
*/

data groups (drop = y);
set cert.input48;
if upcase(cvar) in ('A','B','C','D','E','F','G') then group=1;
else if upcase(cvar) in ('H','I','J','K','L','M','N') then group=2;
else if upcase(cvar) in ('O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
then group=3;
yy = input(y, comma10.);
run;

data groups2; *(drop = yy) ;
	set groups;
	y = yy;
	drop yy;
run;

/* Calculate the average of X and Y by Group */
/* What is the average of X and Y for Group 2? */

proc contents data = groups2;
run;

proc print data = groups (obs = 5);
run;

proc means data=groups2 mean maxdec=2;
class group;
var x y;
run;

data sasuser.one;
   x=1;
   y=27;
   output one;
run;

proc print data = sasuser.one;
run;


 data WORK.NEW;
     year=2011;
     amount=5000;
     do i=1 to 5;
        year=year+1;
        do qtr=1 to 4;
           amount=amount*1.1;
        end;
     end;
  run;
  proc print data=WORK.NEW noobs;
  run;

  data work.look;
   x=2;
   if x=1 then y=100;
   if x=2 then y=200;
   if x=3 then y=300;
   else y=27;
run;

proc print data = look;
run;

data work.test;
   type='SQL'; 
   if type='SAS' then description='SAS Program';
   else description='other';
   length description 8; 
run;
