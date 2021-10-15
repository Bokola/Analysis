/* 6 Ways to specify a list of variables in SAS &/

/* 1. use _NUMERIC_, _CHARACTER_, _ALL_ keywords */

/* compute descriptive statistics of all numeric variables */
proc means data = sashelp.Heart nolabels;
var _NUMERIC_; /* _NUMERIC_ is the default */
run;

/* display the frequencies of all levels of all character variables */
proc freq data = sashelp.Heart;
tables _CHARACTER_; /* _ALL_ is the default */
run;

data HeartNumeric;
set Sashelp.Heart(keep=_NUMERIC_            /* all numeric variables */
                       Sex Smoking_Status); /* two character variables */
run;

/* 2. Use a hyphen to specify numerical suffixes */
data A;
retain Y x1 x3 Z x6 x5 x2 W x4 R 0;
run; /* create 10 variables and one observation.
Initialize to 0 */
proc reg data = A plots = none;
	model Y = x1- x6; /* The syntax x1-x6 will select the six variables
	x1, x2, x3, x4, x5, and x6 regardless of their physical order in the data */
run;

/* 3. Use double-hyphen to specify consecutive variables */
data A;
retain Y 0   x3 2   C1 'A'   C2 'BC'
          Z 3   W  4   C4 'D'   C5 'EF'; /* Initialize eight variables */
run;

data B;
	set A(keep = x3 -- c4);
run;

/* The syntax Y-numeric-Z specifies all numeric variables between Y and Z
in the data set. The syntax Y-character-Z specifies all character variables
between Y and Z */
proc contents data=Sashelp.Heart order=varnum ;
run;
 
proc logistic data=Sashelp.Heart;
   model status = AgeCHDdiag-numeric-Smoking;
   ods select ParameterEstimates;
run;

/*4. Use the colon operator to specify a prefix */

data A;
retain Sales17 Y Sales16 Z SalesRegion Sales_new
Sales1 R 0; /* 1 obs. Initialize to 0 */
run;

data B;
	set A(drop=Sales:);
run;

/* 5. Arrays and the OF operator */
/* You can use variable lists to assign an array in a SAS DATA step.
For example, the following program creates a numerical array named X
and a character array named C. The program finds the maximum value
in each row and puts that value into the variable named rowMaxNUm.
The program also creates a variable named Str that contains the
concatenation of the character values for each row: */

data arrays;
	set sashelp.class;
	array x {*} _NUMERIC_; /* x[1] is 1st var, x[2] 2nd var, etc */
	array c {*} _CHARACTER_; /*c[1] is 1st var, c[2] is 2nd var, etc */
	/* use the OF operator to pass values in array to functions */
	rowmaxnum = max(of x[*]); /* find max value in this array (row) */
	length str $30;
	call catx(' ', str, of c[*]); /* concatenate the strings in this array (row) */
	keep rowmaxnum str;
run;

proc print data = arrays(obs = 4);
run;

/*  You can use the OF operator directly in functions without creating an array
 The program drops any observation that has a missing value for any variable */

data completecases;
	set sashelp.Heart;
	if cmiss(of _ALL_) = 0; /* output only complete cases for all vars */
run;

/* 6. Use macro variable to specify a list */

/* the following call to PROC MEANS creates an output data set
(called MissingValues) that contains columns named Variable and NMiss. */
proc means data=Sashelp.Heart nolabels NMISS stackodsoutput;
   var _NUMERIC_;
   ods output Summary = MissingValues;
run;
proc print; run;

/* Suppose you want to keep or drop those variables that have one or more
missing values. The following PROC SQL call creates a macro variable
(called MissingVarList) that contains a space-separated list of all
variables that have at least one missing value */

proc sql noprint;
	select Variable into :MissingVarlist separated by ' '
	from MissingValues
	where NMiss > 0;
quit;
%put &=MissingVarlist;

/* You can now use the macro variable in a KEEP, DROP, VAR,
or MODEL statement, such as KEEP=&MissingVarList; */
