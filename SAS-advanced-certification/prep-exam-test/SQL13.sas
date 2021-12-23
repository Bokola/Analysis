/* 
Perform an SQL query to create an output table work.BigFish13 
that contains only these columns:
Species
AvgWeight. Calculate this variable as the aggregate average 
(mean) of Weight for each Species of fish.
Filter the results to include only species with an AvgWeight
that is greater than the median Weight of all fish in the cert.
fish13 table.
Your program must produce the table work.BigFish13 in a single
step without creating any additional intermediate tables.
The log must not show a remerge of summary statistics.
*/

proc print data = cert.fish13 (obs =5) noobs;
run;
proc sql;
	create table bigfish13 as
	select species, avg(weight) as avgweight
	from cert.fish13
	group by species
	having avg(weight) > (select median(weight)
	from cert.fish13);
quit;

proc print data = bigfish13;
sum avgweight;
run;
