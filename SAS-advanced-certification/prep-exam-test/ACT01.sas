libname cert "C:\Users\basil\Analysis\SAS-advanced-certification\cert\input";
/*
Use PROC FCMP to create a user-defined function called ReverseName to convert 
the variable name values from lastname, firstname to firstname 
lastname (Example: convert Wei, Zhang to Zhang Wei) Store the 
function in work.functions.dev.
Use the following formula for the conversion:
catx(" ", scan(name,2,","),scan(name,1,","))

Create a new data set work.ACT01.
Create this data set from the cert.names01 data set.
Use the ReverseName function to create a new variable newName from
the name variable.
work.ACT01 should contain both the name and newName variables.

When finished, sort the new data set by the newName variable in 
ascending order.
*/

* define the function;

proc fcmp outlib=work.functions.dev;
	function reversename(name $) $40;
	newname = catx(" ", scan(name,2, ","), scan(name, 1, ","));
	return(newname);
	endsub;
* call the function;
option cmplib = work.functions;
data act01;
	set cert.names01;
	newname = reversename(name);
	keep name newname;
run;

proc sort data = act01 out= act02;
	by newname;
run;

/* What is the value of the NewName variable for observation 28
in the output data set? Enter your answer in the space below: */
proc print data = act02 (firstobs = 28 obs=28) noobs;
run;
