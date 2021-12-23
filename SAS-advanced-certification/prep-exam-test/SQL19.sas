/*
Write a SAS program that will:

Perform an SQL query to create an output table named work.SQL19 from
table cert.class19.
Calculate the average Height for each combination of values of 
Gender and Age.
Store the average Height in a column named AvgHeight.
Order the results first by Gender and then by Age.
Include only the columns Gender, Age, and AvgHeight.
Include only rows in which the AvgHeight is greater than 60.
Display the AvgHeight with only 2 decimal places.
*/

proc print data = cert.class19 (obs = 5) noobs;
run;

proc sql;
	create table sql19 as 
	select avg(height) as avgheight format = 8.2, gender, age
	from cert.class19
	group by gender, age
	having avg(height) > 60
	order by gender, age;
quit;
proc print data = sql19;
run;
