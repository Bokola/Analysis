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
