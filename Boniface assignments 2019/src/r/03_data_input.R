#	 
#	<copyright file="Diabetes and hypertension summaries.R" company="bokola">
#	 
#		Copyright (c) 2019 All rights reserved
#	 
#		This source is owned and managed by KEMRI Wellcome Trust.
#		Permission must be sought from the above mentioned entity for use, distribution or modification
#		If you are not a KEMRI employee, any violation of the above can constitute malice and criminal intent
#		All other rights reserved.
#	 
#	</copyright>
#	 
#	<author>Basil Okola</author>
#	<email>okolabasilowiti@gmail.com</email>
#	<date>2019-08-07</date>
#	<summary>
#	 
#		Point of entry and exit
#	 
#	</summaries and plots>
#	 

# Clear memory



# Display message:

cat("----------------------------------------\n\n")
cat("Data input... \n")

  
monthly_db_htn = read.csv(file.path(data_dir, 'monthly_db_htn.csv'), stringsAsFactors = F) %>%
  janitor::clean_names() #%>%
  #subset(., select = -c(x, x_1, x_2, x_3, x_4, x_5))

daily_db_htn_register = read.csv(file.path(data_dir, 'Daily Register.csv'), stringsAsFactors = F) %>%
  janitor::clean_names()

cat('Data input complete ...! \n\n')


          