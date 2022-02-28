libname cert "C:\Users\basil\Analysis\SAS-advanced-certification\cert\input";
/*
Write a SAS program that will:
Create work.act09 from the cert.custid09 data set.
Use pearl regular expression functions to output only observations 
with valid national ID numbers (variable National_ID). A valid 
national ID number is in the form: nnn-nn-nnnn where n is any digit
0-9.
*/
proc print data = cert.custid09 (obs=5);
run;
proc contents data = cert.custid09;
run;

data n;
	set cert.custid09;
	id =  "/\d{3}-\d{2}-\d{4}/"; 
	*exp = "/(" || id || ")/";
	re = prxparse(id);
	if prxmatch(re, national_id) then 
	output;
run;
/* What is the value of variable CustomerID 
for observation 100 in work.act09? */
proc print data = n (firstobs = 100 obs=100);
run;
