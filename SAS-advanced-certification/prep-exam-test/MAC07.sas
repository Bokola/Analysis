
/* Write a DATA step to create work.MAC07 that reads in the 
cert.mac_in07 data set and stores the value of the NAME 
variable in a macro variable named FNAME. 

Do not use %let to define the macro variable. The macro 
variable must be defined from within the DATA Step */

proc print data = cert.mac_in07 (obs = 5);
run;

data mac07;
	set cert.mac_in07;
	call symput('fname', name);
run;
/* What is the value of the FNAME macro variable?
Enter your answer in the space below: */
%put &fname;
