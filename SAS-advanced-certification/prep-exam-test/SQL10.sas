/* 
Write a SAS program that will:

Use SAS dictionary tables, run a PROC SQL query to 
create a macro variable named COL_LIST which contains
a comma separated list of the columns contained in 
the CERT.AIR10 data set. If needed, use the DESCRIBE
TABLE statement to display the column attributes of 
the dictionary.columns table. */
libname cert "C:\Users\basil\Analysis\SAS-advanced-certification\cert\input";
proc print data = cert.air10 (obs =5);
run;

* how to view summary of a table;
proc sql;
	describe table dictionary.indexes;
	/* The Sashelp view for this table is Sashelp.VINDEX) 
	first word in each line is the column name. You need 
	to use this name when you write a SAS statement that
	refers to the column/var
	following column name is the specification for type of 
	var

	*/
quit;

* vcolumns;
proc sql;
	describe table dictionary.columns;
quit;
* vmembers;
proc sql;
	describe table dictionary.members;
quit;

* how to subset  a dictionary table - answer the question;

proc sql;
	select name into: col_list separated by ','
	from dictionary.columns
	where memname = 'AIR10';
quit;

%put &col_list;
