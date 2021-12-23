/* Run the line of code below for your own SAS software - 
change the file path as necessary */
libname cert "C:\Users\basil\Analysis\SAS-advanced-certification\cert\input";

%include "C:\Users\basil\Analysis\SAS-advanced-certification\cert\input\macro18.sas";
options mprint;
options symbolgen;
options nomprint;
%let one = Asia;
%let two = Europe;

	data one two;
		set sashelp.cars(obs = 1000);
		if upcase(origin) = upcase("&one") then do;
			output one;
		end;

		else if upcase(origin) = upcase("&two") then do;
			output two;
		end;
	run;

%macro cars(one, two);
	data &one &two;
		set sashelp.cars(obs = 1000);
		if upcase(origin) = upcase("&one") then do;
			output &one;
		end;

		else if upcase(origin) = upcase("&two") then do;
			output &two;
		end;
	run;
	%mend cars;

%cars(one=Asia, two=Europe);
%cars18();
