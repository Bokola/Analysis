/*
Open a new programming window to create ACT22.sas in cert\programs.

Write a SAS program to load the cert.division22 data set into a 
HASH object. Use the following guidelines:
DIV is the key component containing the Division code
League and Division are the data components and should be defined 
with a character variable length of 10 bytes.
Perform a lookup on the cert.teams22 data set to combine the 
Division codes with their description. Store the results in the 
work.ACT22 data set.
*/
libname cert "C:\Users\basil\Analysis\SAS-advanced-certification\cert\input";
proc print data = cert.division22 (obs =5) noobs;
run;


data division22;
	length division league $10;
	set cert.division22;
	if _n_ = 1 then do;
		/* Avoid uninitialized variable notes */
		call missing(Div, League, Division);
		declare hash lg (dataset: "work.division22",
		multidata: 'yes', ordered: 'yes');
		declare hiter iter ('lg');
		lg.definekey("Div");
		lg.definedata("League", "Division");
		lg.definedone();
		
	end;
	*do until(eof2);
		set cert.teams22;* end = eof2;
			rc = iter.first();
			do while (rc =0);
				if rc = 0 then output;
			rc = iter.next();

			end;
	
run;

/* How many rows are in the output data set? Enter your numeric 
answer in the space below: */
/* What is the value of variable DIV for the observation that has 
variable team = Boston ? */


proc contents data = division22 ;
run;
proc print data = division22;
run;
