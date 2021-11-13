%let path=C:\Users\basil\Analysis\SAS-advanced-certification\data;
%macro setdelim;
   %global delim;
   %if %index(&path,%str(/)) %then %let delim=%str(/);
   %else %let delim=%str(\);
%mend;
%setdelim
libname certadv "&path";

