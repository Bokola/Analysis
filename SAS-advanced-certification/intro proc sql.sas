libname d "C:\Users\basil\Google Drive (basil.okola@student.uhasselt.be)\MSc. Stats Hasselt\SAS\Advanced SAS\SQL\data";
* view column atrtributes;
* similar to proc contents;
proc sql;
	describe table d.customer;
quit;

* select columns;
proc sql;
	select firstname, lastname, state
		from d.customer(obs = 10);
quit;
* number, inobs, outobs options;
proc sql inobs = 10 number;
	select customerid, lastname, userid, dob
		from d.customer;
quit;

proc sql;
	select * 
		from d.customer
		where state in ("VT", "SC", "GA");
quit;
* special where operators: is null, is not null;
* is null operator works for both character and numeric values;
/* ActivityOpen s102a02.sas from the activitiesfolder and perform
the following tasks to find all customers with a nonmissing 
CreditScore value that is less than 500 */

proc sql;
	create table a as
		select * 
			from d.customer
			where creditscore is not null  and 
			creditscore LT 500;
quit;
proc sql inobs=50;
	select *
	from a
quit;
* between;
proc sql;
	create table a as
		select * 
			from d.customer
			where creditscore between 700 and 799;
quit;
* wildcards
* % for any number of characters ;
proc sql outobs=10;
	select * 
		from d.customer
		where firstname like "Z%";
quit;
* _ for a single character;
proc sql outobs=10;
	select * 
		from d.customer
		where firstname like "Z_";
quit;
* you sort output with order by;
proc sql;
	select firstname, lastname,  creditscore
		from d.customer
		where creditscore > 830
		order by 3 desc, 2;
quit;
* enhancing reports;
proc sql outobs = 10;
	title "Customers from Hawai";
	select Firstname, Lastname, State,
		userid "Email Address", /* ASCII standard */
		Income label = "Estimated Income"
		format = dollar10.2, /* sas flavour */
		DOB format = date9.
		from d.customer
		where state = "HI" and income is not null
		order by income desc;

quit;
title;
* creating new column without as alias results in blank name;
proc sql outobs=15;
	select state, yrdif(dob, '01jan2019'd)
		from d.customer;
quit;
* calculated key word for created columns;
proc sql;
	select state, yrdif(dob, '01jan2019'd) as Age
		from d.customer
		where calculated age >=70;
quit;
* Assigning values to a new column conditionally;
* case when;
proc sql;
	describe table d.customer;
quit;
proc sql;
	select Firstname, Lastname, State, creditscore,
		case
			when creditscore >= 750 then "Excellent"
			when creditscore >= 700 then "Good"
			when creditscore >= 650 then "Fair"
			when creditscore >= 550 then "Poor"
			when creditscore >= 0 then "Bad"
			else "Uknown"
		end as Category
		from d.customer (obs = 15);
quit;
* CASE-Operand Form;
* a test of inequality is implied;
proc sql outobs=10;
	select Firstname, Lastname, State, Creditscore,
		case Married
			when "M" then "Married"
			when "D" then "Divorced"
			when "S" then "Single"
			when "W" then "widowed"
			else "Unknown"
		end as Category
		from d.customer;
quit;
* using dynamic titles with %qsysfunc();
proc sql;
title "Created on %sysfunc(left(%qsysfunc(date(),
worddate18.)))";
	select substr(Firstname, 1,1) as Initial
	from d.customer (obs =3);
quit;
* using call symput in data step to create a date macro;
data _null_;
today =
trim(left(put(date(),worddate18.)));
call symput('dtnull',today);
run;
proc sql;
	title "created on &dtnull";
	select *
		from d.customer (obs = 10);
quit;
title;
/* Summaries to data */
* distinct column values;
proc sql;
	select distinct state
		from d.customer;
quit;
* max, avg, min;
* when you use a summary function with a single argument,
nonmissing values are totaled down a column;

proc sql;
	select 
		max(popestimate1) as Maxest format = comma16.,
		min(popestimate1) as Minest format = comma16.,
		avg(popestimate1) as Avgest format = comma16.
		from d.statepopulation;
quit;
* when you use a summary function with multiple arguments, nonmissing
values are totaled across row;
proc sql;
	select name, popestimate1, popestimate2, popestimate3,
		max(popestimate1, popestimate2, popestimate3) as
		MaxEst format=comma16.
		from d.statepopulation;
quit;

* using the count function;
proc sql;
	select count(*) as Totalcustomers format = comma12.
		from d.customer;
quit;
* grouping and filter with having;
proc sql;
	select state, count(*) as Totalcustomers format=comma7.
		from d.customer
		where BankID is not null
		group by state
		having Totalcustomers > 6000
		order by Totalcustomers desc;
quit;
* extracting data from datetime value;
* datepart and timepart;
proc sql;
	select month(datepart(datetime)) as month,
		median(Amount) as MedianSpent format = dollar16.
		from d.transaction
		group by month
		order by MedianSpent desc;
quit; 
/* creating tables */
proc sql;
	create table Customercount as 
		select state, yrdif(DOB, '01JAN2020'd, 'age') <25 
		/* age is the basis*/
			as Under25
			from d.customer;
quit;

* copy the structure of an existing table;
proc sql;
	create table highcredit 
	like d.customer(keep = Firstname Lastname Userid CreditScore);
quit;
* dfining columns;
proc sql;
	create table employee
	(Firstname char(20),
	Lastname char(20),
	DOB date format=mmddyy10.,
	Empid num format = z6.);
quit;
/* inserting rows into tables */
* a) with a query;
proc sql;
	insert into highcredit
	(Firstname, Lastname, Userid, creditscore)
	select Firstname, Lastname, Userid, creditscore
		from d.customer
		where creditscore > 700;
quit;
* b) values clause;
proc sql;
	insert into employee
	(Firstname, Lastname, DOB, Empid)
	values("Diego", "Lopez", "01SEP1980"d, 1280)
	values("Omar", "Fayed", "21MAR1989"d, 1310);
quit;

* c) set clause;
proc sql;
	insert into employee
	set Firstname = "Diego",
		Lastname = "Lopez",
		DOB = "01SEP1980"d,
		Empid = 1280;
quit;

* drop table with drop table-name;
proc sql;
	drop table employee;
quit;
/* using dictioanry tables */
* info about data - metadata;
* valid in proc sql only;
* info about each sas session, updated automatically;
* dictionary.tables, dictionary.columns, dictionary.libnames;



********************************;
*EXPLORE DICTIONARY.LIBNAMES   *;
********************************;
proc sql;
describe table dictionary.libnames;
select *
	from dictionary.libnames;
quit;


********************************;
*EXPLORE DICTIONARY.TABLES     *;
********************************;

proc sql;
describe table dictionary.tables;
select *
	from dictionary.tables
	where Libname = "D";
quit;


proc print data=sashelp.vtable(obs=100);
	where Libname = "D";
run;


********************************;
*EXPLORE DICTIONARY.COLUMNS    *;
********************************;

proc sql;
describe table dictionary.columns;
select *
	from dictionary.columns
	where Libname = "D";
quit;


proc sql;
describe table dictionary.columns;
select *
	from dictionary.columns
	where Libname = "D" and upcase(Name) = "BANKID";
quit;


proc print data=sashelp.vcolumn(obs=100);
	where Libname = "D";
run;


********************************;
*EXPLORE DICTIONARY.LIBNAMES   *;
********************************;

proc sql;
describe table dictionary.libnames;
select *
	from dictionary.libnames;
quit;


proc sql;
describe table dictionary.libnames;
select *
    from dictionary.libnames
    where Libname = "D";
quit;


proc print data=sashelp.vlibnam;
    where Libname = "D";
run;

/* 3 SQL JOINS */
