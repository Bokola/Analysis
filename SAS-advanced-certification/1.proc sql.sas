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

/* chap2: creating and managing tables */

/*a) create blank table*/

proc sql;
	create table discount
		(Destination char(3),
		BeginDate num format=date9.,
		EndDate num format=date9.,
		Discount num);
quit;

/* b) using LIKE clause - columns but no rows*/
proc sql;
	create table flightdelays2
	like d.flightdelays;
quit;
/* c) using AS keyword - to create a table from query */
proc sql;
	create table ticketagents as
		select lastname, firstname,
			jobcode, salary
		from d.payrollmaster, d.staffmaster
		where payrollmaster.empid = staffmaster.empid /* an inner join */
			and jobcode contains 'TA';
quit;

* describe a table's structure with describe table statement;

proc sql;
	describe table ticketagents;
quit;

/* INSERT statement */
	* insert by column name by using set;
proc sql;
	insert into discount
		set destination = 'LHR',
			begindate = '05MAR2018'd,
			discount = .33
		set destination = 'CPH',
			begindate = '03MAR2018'd,
			discount = .15;
quit;

	* insert using values clause;

proc sql;
	insert into discount(destination,
			begindate, enddate, discount)
		values ('LHR', '01MAR2018'd, '05MAR2018'd, .33)
		values ('CPH', '03MAR2018'd, '10MAR2018'd, .15);
quit;

	* insert using rows copied from a query result;
proc sql;
	create table payrollchanges2
		like d.payrollmaster;
quit;

proc sql;
	insert into payrollchanges2
		select *
			from d.payrollmaster
			where empid in ('1919', '1350', '1401');
quit;

/* using Dictionary tables */

proc sql;
	describe table dictionary.tables; * for tables;
run;

/* chap3: Joining tables */

* a) cartesian product;
proc sql;
	select *
		from d.one, d.two;
quit;
*b) inner join with from clause;
proc sql;
	select *
		from d.one inner join d.two
			on one.x = two.x;
quit;
* eliminating duplicate columns - specify just one of the colums
in the select statement;
proc sql;
	select one.x, a, b
		from d.one inner join d.two
		on one.x = two.x;
quit;

* alternative with asterisk (*);
proc sql;
	select one.*, b
		from d.one inner join d.two
		on one.x = two.x;
quit;
* renaming column by using column alias;
proc sql;
	select one.x as ID, two.x, a,b
		from d.one inner join d.two
		on one.x = two.x;
quit;
* specify a table alias with table.column-name */;
proc sql;
	select staffmaster.empid, lastname, firstname, jobcode
		from d.staffmaster  as s inner join d.payrollmaster as p
		on s.empid = p.empid;
quit;

* Suppose you want to create a report where the name is displayed 
with first initial and last name (R.Long), JobCode, and ages of 
all employees who live in New York. The report also should be sorted by
JobCode and Age;

proc sql;
	describe table d.payrollmaster, d.staffmaster;
quit;

proc sql outobs = 15;
	title 'New York Employess';
	select substr(firstname,1,1) || '.' || lastname
		as Name,
		jobcode,
		int((today() - dateofbirth) / 365.25)
		as Age
		from d.payrollmaster as p inner join
			d.staffmaster as s
		on p.empid = s.empid
		where state = 'NY'
		order by 2, 3;
quit;

* inner join with summary functions ;
* summarizes columns for New York employees 
in each job code: number of employees and
average age;
proc sql outobs = 15;
	title "Average age of New York Employess";
	select jobcode, count(distinct p.empid) as Employees,
		avg(int(today() - dateofbirth)/ 365.25) 
		format = 4.1 as AvgAge
		from d.payrollmaster as p inner join 
			d.staffmaster as s
			on p.empid = s.empid
		where state = "NY"
		group by jobcode
		order by jobcode desc;
quit;

/* natural join */

proc sql;
	select *
		from d.schedule natural join 
			d.courses ;
quit;

/* outer join */
/* a) left join */
proc sql;
	select *
		from d.one left join
			d.two
			on one.x = two.x;
quit;

* elimiating duplicate colums - using table.column;
proc sql;
	select one.x, a, b
		from d.one left join d.two
			on one.x = two.x;
quit;

/* b) right join */
proc sql;
	select *
		from d.one right join d.two
		on one.x = two.x;
quit;
	
/* c) full join */

proc sql;
	select *
		from d.one full join d.two
		on one.x = two.x;
quit;

/* complex outer join */

/* Suppose you want to list all of an airline's 
flights that were scheduled for March, along with
any available corresponding delay information.
Each flight is identified by both a flight date
and a flight number. Your output should display 
the following data: flight date, flight number, 
destination, and length of delay in minutes */

proc sql;
	select m.date,
		m. flightnumber label= 'Flight Number',
		m.destination label = "Left",
		f.destination label = "Right",
		delay label = "Delay in minutes"
		from d.marchflights as m left join 
			d.flightdelays as f
			on m.date = f.date
			and m.flightnumber = f.flightnumber
		order by delay;
quit;

/* coalesce function - overlays columns presenting 
results similar to merge data step*/
proc sql;
	select coalesce(three.x, four.x)
		as x, a, b
		from d.three full join
		d.four
		on three.x = four.x;
quit;
