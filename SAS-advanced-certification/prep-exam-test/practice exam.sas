libname cert "C:\Users\basil\Analysis\SAS-advanced-certification\cert\input";
/* 
Write a SAS program that will:
Perform an SQL query to create a table named 
work.SQL02 by concatenating cert.prdsal2 and cert.prdsal3. These data sets
contain the same variables.
Work.SQL02 must contain:
One row for all the unique combinations of 
PRODUCT and PRODTYPE when combining cert.
prdsal2 and cert.prdsal3.
Only the two columns PRODTYPE and PRODUCT.
Order the data in descending order by PRODTYPE.
Run your program and troubleshoot as necessary. 
When you are finished with the project, 
answer the next four questions.
*/

proc sql noprint;
	create table sql02 as 
	select prodtype, product 
	from cert.prdsal2 
	union
	select prodtype, product
	from cert.prdsal3
	order prodtype desc;
quit;
/* How many rows are in the output data set? Enter
your numeric answer in the space below: */
proc contents data = sql02;
run;
proc print data = sql02;
run;

* macro do loops;

%macro test(%global x);
	%let x=1.5;
	%do %while (&x<=2);
		%put the value of x is &x;
		%let x=%sysevalf(&x+0.25);
	%end;

%mend test;
%test(1);

%macro test(x);
%do %while (%sysevalf(&x<=2));
  %put the value of x is &x;
  %let x=%sysevalf(&x+0.25);
%end;
&x.
%mend test;
data want;
  myvar=%test(1);
run;

%macro test(x);
%do %while (%sysevalf(&x<=2));
  %put the value of x is &x;
  %let x=%sysevalf(&x+0.25);
%end;
%if not %symexist(y) %then %global y;
%let y=&x;
%mend test;
%test(1);
data a;
	%test(1);
	z = &y;
	output;
run;