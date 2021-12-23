libname cert "C:\Users\basil\Analysis\SAS-advanced-certification\cert\input";
/* 1 */
proc print data = cert.prdsal2 (obs = 5) noobs;
run;
proc print data = cert.prdsal3 (obs = 5) noobs;
run;
data sql02 (keep = prodtype product);
	set cert.prdsal2 cert.prdsal3;
run;
proc sort data = sql02 out = sql022;
	by descending prodtype;
run;
proc print data = sql02 (obs =5);
run;
proc contents data = sql022;
run;

