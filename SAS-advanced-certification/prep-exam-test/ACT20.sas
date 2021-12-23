/*
Write a SAS program that will:

Use a DATA step to create data set work.ACT20 using cert.salary20 as 
input. Create an array named INCS to group the INCOME1-INCOME5 
variables together. Create a temporary array named NEW and define 
it with the initial values of 80000, 81000, 82000 83000 and 84000.
Use a loop to compare the elements in INCS to the corresponding 
element in the NEW array. If the value of INCS is less than NEW, 
assign INCS the value of NEW.
*/
libname cert "C:\Users\basil\Analysis\SAS-advanced-certification\cert\input";
data act20;
	set cert.salary20;
	array incs [5] income1-income5;
	array new [5]  _temporary_ (80000 81000 82000 83000 84000);
	do i = 1 to 5;
		if incs[i] < new[i] then do;
		incs[i] = new[i];
		end;
	end;
run;

/* What is the mean value of the income1 variable for the entire
output data set? Round your answer to the nearest integer and 
enter your numeric answer in the space below: */

proc means data = act20 mean maxdec = 0;
	var income1;
run;

%let idcode=Prod567; 
%put codenum=%substr(&idcode,%length(&idcode)-2);
%put x=%substr("ABCD", 2, 1);
