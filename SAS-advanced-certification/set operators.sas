proc sql;
	describe table sq.salesemail, sq.salesphone;
quit;
title "Customers who have responded to both Email and Phone Sales";
proc sql;
	select CustomerID 
		from sq.salesemail
	intersect
	select CustomerID 
		from sq.salesphone;
quit;
title;
title "List of customers who have not respondend to sales email";
proc sql;
	select CustomerID 
		from sq.saleslist
	except
	select CustomerID
		from sq.salesemail;
quit;
title;
* using subquery to produce identical except set results;
proc sql;
	select customerID
		from sq.saleslist
		where customerID not in (select customerID 
			from sq.salesemail);
quit;
* union;
proc sql;
	select CustomerID
		from sq.salesemail
	union
	select CustomerID 
		from sq.salesphone;
quit;
proc sql;
	select * 
		from sq.salesemail
	union
	select * 
		from sq.salesphone;
quit;
/* The CORR keyword aligns the columns that have the same name in both tables
and removes any columns not found in both tables. */
proc sql;
	select * 
		from sq.salesemail
	union corr
	select * 
		from sq.salesphone;
quit;
proc sql;
	select count(*) as TotalNum
		from(select customerID from sq.salesemail
			union
			select customerID from sq.salesphone);
quit;
/* the ALL operator does not remove the duplicate rows */
proc sql;
	select customerID
		from sq.salesemail
	union all 
	select customerID
		from sq.salesphone;
quit;
/* combining set operators */
/* customers who have not respondend to any sales attempts */
proc sql;
	select customerID
		from sq.saleslist
	except
	(select customerID 
		from sq.salesemail
	union
	select customerID
		from sq.salesphone);
quit;
* outer union;
proc sql;
	select *
		from sq.salesemail
	outer union
	select *
		from sq.salesphone;
quit;
/* Add the RENAME= option after the salesemail table and rename the 
column EmailResp toResp. */
proc sql;
	select *
		from sq.salesemail(rename = (EmailResp = Resp))
	outer union corr
	select *
		from sq.salesphone (rename = (PhoneResp = Resp));
quit;

/* alternative */
proc sql;
	select CustomerID, EmailResp as Resp
		from sq.salesemail
	outer union corr
	select CustomerID, SalesRep, PhoneResp as Resp
		from sq.salesphone;
quit;
/* data step alternative */
data response2;
	length Resp $12;
	set sq.salesemail(rename=(EmailResp = Resp))
		sq.salesphone(rename=(phoneResp = Resp));
run;
