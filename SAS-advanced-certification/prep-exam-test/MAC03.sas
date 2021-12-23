  
libname cert "C:\Users\basil\Analysis\SAS-advanced-certification\cert\input";

/*
Write a SAS program that will:

Define a macro named combo03. This macro should append the data
sets to each other, in numeric order (data1, then data2, then 
data3, etc) into one data set named work.combined03.
Call your combo03 macro. */

proc print data = cert.data100 (obs = 5);
run;
proc contents data = cert.data100;
run;
* https://communities.sas.com/t5/SAS-Health-and-Life-Sciences/How-to-append-100-datasets-having-similar-names/td-p/6559;
%macro combo3(lib = ,dsn=, start = , stop=);
	%do i = &start %to &stop;
		&lib..&dsn.&i
	%end;
	%mend combo3;

options mprint symbolgen;
data combined03;
	set %combo3(lib = cert,dsn = data, start= 1, stop=100);
run;

/* How many observations are in the output data set? Enter 
your numeric answer in the space below: */
proc contents data = combined03;
run;

/* What is the value of variable VAR4 in observation 2000?
Enter your numeric answer in the space below: */
proc print data = combined03 (firstobs = 2000 obs=2000);
	var var4;
run;
