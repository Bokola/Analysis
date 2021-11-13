%let data = C:\Users\basil\Analysis\SAS-advanced-certification\data;
%let output = C:\Users\basil\Analysis\SAS-advanced-certification\output;

libname d "&data";
libname o "&output";
/* select statement */
*  creates a new column bonus;
proc sql;
	select empid, jobcode, salary, salary*.06 as bonus
		from d.payrollmaster
		where salary < 32000
		order by jobcode;
quit;

* use * to select all coluns;
proc sql feedback;
	select * 
		from d.staffchanges;
quit;

* eliminate duplicates with distinct;
proc sql;
	select distinct flightnumber, destination
		from d.internationalflights;
quit;

* using additions to create new column;
proc sql outobs = 10; * limit to 10 obs, like sql's limit statement;
	select flightnumber, date, destination,
			boarded + transferred + nonrevenue as total
		from d.marchflights;
quit;
* you can use calculated column in a condition but must include
calculated statement to avoid errors;

proc sql outobs = 10; * limit to 10 obs, like sql's limit statement;
	select flightnumber, date, destination,
			boarded + transferred + nonrevenue as total
		from d.marchflights
		where calculated total < 100;
quit;

* you can also use calculated in other parts of the query;
proc sql outobs = 10; * limit to 10 obs, like sql's limit statement;
	select flightnumber, date, destination,
			boarded + transferred + nonrevenue as total,
			calculated total/2 as half
		from d.marchflights
		where calculated total < 100;
quit;
/* conditional operators */

/*1. between and */
proc sql outobs = 15;
	select flightnumber, date, destination,
		boarded + transferred + nonrevenue as total,
		calculated total/2 as half
		from d.marchflights
		where calculated total between 0 and 50;
	quit;
/* 2. contains/? */
proc sql outobs = 10;
	select name
		from d.frequentflyers
		where name contains 'ER';
quit;

* or;
proc sql outobs = 10;
	select name
		from d.frequentflyers
		where name ? 'ER';
quit;

/* 3. missing */
proc sql feedback outobs = 10;
	select *
		from d.marchflights
		where boarded is missing;
quit; 

/* Alternatively, you can specify missing values without using 
the IS MISSING or IS NULL
operator, as shown in the following examples:
where boarded = .
where flight = ' '
However, the advantage of using the IS MISSING or IS
NULL operator is that you do not
have to specify the data type (character or numeric) of the column
*/

/* 4. LIKE */

/* underscore matches one character, percent sign matches a 
sequence of zero or more characters */

proc sql;
	select ffid, name, address
		from d.frequentflyers
		where address like '%P%PLACE';
quit;

/* 5. sounds like =* */
* e.g where lastname =* 'Smith';

/* group by clause */
proc sql ;
	select membertype, sum(milestraveled) as totalmiles 
		from d.frequentflyers
		group by membertype;
quit;

* specifying one column;
proc sql;
	select sum(boarded), sum(transferred), sum(nonrevenue)
		as total
		from d.marchflights;
quit;
/* apply function to whole table */
proc sql;
	select jobcode, avg(salary)
		as AvgSalary
		from d.payrollmaster;
quit;

/* total number of passangers for each flight */
proc sql;
	select flightnumber, sum(boarded, transferred, nonrevenue)
		as total
		from d.marchflights;
quit;

/* you can format within select statement */
proc sql;
	select jobcode, avg(salary) as avgsalary format = dollar11.2
		from d.payrollmaster
		group by jobcode;
quit;
/* count function */
proc sql;
	select count(*) as n
		from d.payrollmaster;
quit;
* count distinct;
proc sql;
	select count(distinct jobcode) 
		from d.payrollmaster;
quit;

* count rows withon groups of data;
proc sql;
	select substr(jobcode, 1, 2)
		label 'Job Category',
		count(*) as n
		from d.payrollmaster
		group by 1;/* the results are to be grouped by the
		first defined column, which is
		referenced by 1 because the column was not assigned a name.*/
quit;

/* having clause - filter grouped summaries */
proc sql;
	select jobcode, avg(salary) as avgsalary format = dollar11.2
		from d.payrollmaster
		group by jobcode
		having avg(salary) > 56000;
quit;

/* order by clause */
proc sql;
	select empid, jobcode, salary,
		salary*.06 as bonus
	from d.payrollmaster
	where salary < 32000
	order by jobcode descending;
quit;

/* you can order columns by thier position in the select clause */
proc sql;
	select empid, jobcode, salary, dateofhire
		from d.payrollmaster
		where salary < 32000
		order by 4, 2;
quit;

/* validating query with NOEXEC and VALIDATE options */
proc sql noexec;
	select empid, jobcode, salary
		from d.payrollmaster
		where jobcode ? 'NA'
		order by salary;
quit;

proc sql;
	validate
	select empid, jobcode, salary
		from d.payrollmaster
		where jobcode ? 'NA'
		order by salary;
quit; 
