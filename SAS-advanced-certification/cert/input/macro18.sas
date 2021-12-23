%macro cars18(one, two);
/* macro for scenario mac18*/
data &one &two;
  set sashelp.cars(obs=1000);
  %if %upcase(origin)=%upcase("&one") %then %do;
     Code=1;
     output &one;
	 run;
  %end;

  %else %if %upcase(origin)=%upcase("&two") %then %do;
     Code=2;
     output &two;
	 run;
  %end;
  
%mend cars18;

*%put %upcase("&one");
