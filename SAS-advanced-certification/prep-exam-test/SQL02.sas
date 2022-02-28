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
proc print data = cert.prdsal2 (obs = 5) noobs;
run;
proc print data = cert.prdsal3 (obs = 5) noobs;
run;
proc sql;
	create table sql02 as
	select prodtype, product
		from cert.prdsal2 
	union
	select prodtype, product
		from cert.prdsal3
	order by prodtype desc;
quit;

/* What is the value of the variable PRODTYPE
in the first row of the output data set? 
Enter your answer in the box below.*/
proc print data = sql02 (obs =5);
run;
/* How many rows/cols are in the output data set?
Enter your numeric answer in the space below:
*/
proc contents data = sql02;
run;

