# i. INTRODUCTION ---------------------------------------------------------

# The objective of this paper is to predict the tax scenario for employees in the payroll before the starting of a new fiscal year or the tax
# scenario for new hirings during the current year in order to reduce differences between the real amount of taxes and the cumulative total of
# taxes payed at the end of the year.

# ii. LOAD LIBRARIES ----------------------------------------------------------

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(stringr)
library(lubridate)

# I. Data Preparation --------------------------------------------------------
# ...I.I. Data Wrangling --------------------------------------------------------
# ......I.I. Compensations and Deductions --------------------------------------------------------

# Import dataset with the concepts payed in the payroll for each employee:
concepts_payroll_a_2017 <- read.csv("data/2017_concepts_payroll_a.csv")
concepts_payroll_a_2018 <- read.csv("data/2018_concepts_payroll_a.csv")
concepts_payroll_a_2020 <- read.csv("data/2020_concepts_payroll_a.csv")

str(concepts_payroll_a_2017)
str(concepts_payroll_a_2018)
str(concepts_payroll_a_2020) # Note the extra column "numplaza" in this database (id_job_position).

# Reload with all columns class character:
concepts_payroll_a_2017 <- read.csv("data/2017_concepts_payroll_a.csv",
                                    colClasses = replicate(12,"character"))
concepts_payroll_a_2018 <- read.csv("data/2018_concepts_payroll_a.csv",
                                    colClasses = replicate(12,"character"))
concepts_payroll_a_2020 <- read.csv("data/2020_concepts_payroll_a.csv",
                                    colClasses = replicate(12,"character"))

str(concepts_payroll_a_2017)
str(concepts_payroll_a_2018)
str(concepts_payroll_a_2020)

# Change some names:
names(concepts_payroll_a_2017)[1] <- "id_emp"
names(concepts_payroll_a_2017)[8] <- "year"
names(concepts_payroll_a_2018)[1] <- "id_emp"
names(concepts_payroll_a_2018)[8] <- "year"
names(concepts_payroll_a_2020)[1] <- "id_emp"
names(concepts_payroll_a_2020)[9] <- "year" #2020 has 1 additional column

str(concepts_payroll_a_2017)
str(concepts_payroll_a_2018)
str(concepts_payroll_a_2020)

# Append sets:
concepts_payroll_a <- rbind(concepts_payroll_a_2017,concepts_payroll_a_2018)
concepts_payroll_a <- concepts_payroll_a %>% mutate(numplaza="0") # Add additional column to match 2020's structure.
concepts_payroll_a <- rbind(concepts_payroll_a,concepts_payroll_a_2020)

str(concepts_payroll_a)

# Change some classes:
concepts_payroll_a <- concepts_payroll_a %>%
  mutate_at(c("id_emp","mes","year"),~as.integer(.))
concepts_payroll_a <- concepts_payroll_a %>%
  mutate_at("Importe",~as.numeric(.))

str(concepts_payroll_a)

# NA introduced by coercion: (why? id_emp and year are NAs so these must be real empty lines in the csv)
concepts_payroll_a %>%
  filter(is.na(id_emp)) %>%
  group_by(id_emp,year) %>% summarize(n())

# Explore the concepts:
# P = Percepciones, which means "Compensations".
# D = Deducciones, which means "Deductions".
head(concepts_payroll_a)

# ......I.II. Payroll Summaries --------------------------------------------------------

# Summary payroll contains the result of all compensations and all deductions by employee by month.
# The relation between summary payroll and concepts is: numEmp and year (because we will append multiple years).
# Import the summary payroll:
summary_payroll_a_2017 <- read.csv("data/2017_summary_payroll_a.csv")
summary_payroll_a_2018 <- read.csv("data/2018_summary_payroll_a.csv")
summary_payroll_a_2020 <- read.csv("data/2020_summary_payroll_a.csv")

str(summary_payroll_a_2017)
str(summary_payroll_a_2018)
str(summary_payroll_a_2020) #Note the extra column "numplaza"

# Convert "fechaIngreso" to date:
# In the datasets there is a column "uma" that stands for "Unidad de Medida y Actualización". This is an economic reference in mexican pesos which
# determines the amount of payment of obligations and assumptions established in federal laws.
# This feature is not on the original data but was added for modeling purposes that we will explain later.
summary_payroll_a_2017$fechaIngreso <- dmy(summary_payroll_a_2017$fechaIngreso)
summary_payroll_a_2018$fechaIngreso <- dmy(summary_payroll_a_2018$fechaIngreso)
summary_payroll_a_2020$fechaIngreso <- dmy(summary_payroll_a_2020$fechaIngreso)

str(summary_payroll_a_2017)
str(summary_payroll_a_2018)
str(summary_payroll_a_2020) # Note the extra column "numplaza"

# Append sets:
summary_payroll_a <- rbind(summary_payroll_a_2017,summary_payroll_a_2018)
summary_payroll_a <- summary_payroll_a %>% mutate(numplaza="0") # Add additional column to match 2020's structure.
summary_payroll_a <- rbind(summary_payroll_a,summary_payroll_a_2020)

str(summary_payroll_a)

# Change some names:
names(summary_payroll_a)[1] <- "id_emp"
names(summary_payroll_a)[3] <- "year"

str(summary_payroll_a)

# The length of service is an important feature in which many compensations depend on. So we will add a column that tells us how many years has an
# employee at the beginning of each fiscal year (2017, 2018 and 2020). 
# Date of start by year:
ini_year_2017 <- ymd("2017-01-01")
ini_year_2018 <- ymd("2018-01-01")
ini_year_2020 <- ymd("2020-01-01")

# Add length of service:
summary_payroll_a <- summary_payroll_a %>%
  mutate(years_service = (if_else(year==2017, ini_year_2017,
                                  if_else(year==2018, ini_year_2018,
                                          ini_year_2020))-fechaIngreso)/365) %>%
  mutate_at("years_service",~as.numeric(.)) %>%
  mutate(years_service = floor(years_service))

str(summary_payroll_a)

# ......I.III. Employers and Employees --------------------------------------------------------

# Import employers (employers are the same in the 3 years):
employers <- read.csv("data/employers.csv")
str(employers)

# Import employees (this is because we processed the RFC of the employees since RFC in "summary_payroll" presented some inconsistencies):
employees_2017 <- read.csv("data/2017_employees_a.csv")
employees_2018 <- read.csv("data/2018_employees_a.csv")
employees_2020 <- read.csv("data/2020_employees_a.csv")

str(employees_2017)
str(employees_2018)
str(employees_2020)

# Append sets:
employees <- rbind(employees_2017,employees_2018)
employees <- rbind(employees,employees_2020)

str(employees)

# ......I.IV. Tax Scenarios --------------------------------------------------------

# Import tax scenario:
tax_scenarios_2017 <- read.csv("data/2017_tax_scenarios.csv")
tax_scenarios_2018 <- read.csv("data/2018_tax_scenarios.csv")
tax_scenarios_2020 <- read.csv("data/2020_tax_scenarios.csv")

# ID_Ref is a key for each combination of Employee and Employer (RFC Employee + RFC Employer). This is the reference used to calculate taxes.
str(tax_scenarios_2017)
str(tax_scenarios_2018)
str(tax_scenarios_2020)

# Append sets:
tax_scenarios <- rbind(tax_scenarios_2017,tax_scenarios_2018)
tax_scenarios <- rbind(tax_scenarios,tax_scenarios_2020)

str(tax_scenarios)

# Eliminate first column:
tax_scenarios <- tax_scenarios %>% select(-X.)
str(tax_scenarios)


# ...I.II. Create Final Set --------------------------------------------------------

# So far we have 5 main datasets:
# 1. Payroll (summary).
# 2. Concepts.
# 3. Employers.
# 4. Employees.
# 5. Tax Scenarios (output).

# Now we will merge the data to create a useful dataset:

# Join payroll + employers:
summary_payroll_a <- left_join(summary_payroll_a,employers %>% select(idNomina,rfc_emisor),by="idNomina")

# Join payroll + employees:
summary_payroll_a <- left_join(summary_payroll_a,employees %>% select(numEmpleado,RFC_Empleado,year),by=c("id_emp"="numEmpleado","year"))

# Create ID to identify each employee per employer (ID_Ref = RFC employee + RFC employer):
# (ID_Ref: main key to calculate tax scenario since this gives us 1 single person per employer even if that person is registered as multiple
# employees, which is a mistake found in tha database).
# ID_Ref will be the key id to join the tax scenario (outcome).
summary_payroll_a <- summary_payroll_a %>% mutate(ID_Ref = str_c(RFC_Empleado,rfc_emisor,sep="|"))

# Join payroll + tax scenarios:
# Organismo_DET will be useful since there are employees that has records in both payroll databases (1 and 2). This field would be refer as
# id_database.
summary_payroll_a <- left_join(summary_payroll_a,tax_scenarios %>% select(ID_Ref,Escenario,Year,Organismo_DET),by=c("ID_Ref","year"="Year"))
str(summary_payroll_a)

# Join concepts + payroll:
features <- summary_payroll_a %>% select(id_emp,mes,year,numplaza,uma,years_service,puesto,ID_Ref,Escenario,Organismo_DET)

# There are multiple records for the same employee in the same month in the same year with same job position:
features %>% group_by(id_emp,mes,year,numplaza) %>% summarize(n=n()) %>% arrange(-n)

# So, we need to discriminate those employees (special cases):
discard_observations <- features %>%
  group_by(id_emp,mes,year,numplaza) %>%
  summarize(n=n()) %>%
  filter(n!=1)

features <- anti_join(features,discard_observations,by="id_emp")

# Now we have unique combinations of employee + month + year + job position, so we can join concepts_payroll + features (of summary_payroll):
concepts_payroll_a <- left_join(concepts_payroll_a,features,by=c("id_emp","mes","year","numplaza"))

str(concepts_payroll_a)

# For our final set we substract only the columns of interest:
# (Filter only tipind="P" since tax scenario depends exclusively on conpensations)
# (Filter only Organismo_DET = "ADMINISTRATIVO" (id_database = 1) since there are exceptions of employees with records in other payroll which are
# out of the scope of this test)
# (Filter only job_title which does not begin with "J" or "N"... this step will be perform later)
# (Discard tax_scenario = 0 which is an exception out of the scope of this test)
final_set <- concepts_payroll_a %>%
  filter(tipind == "P",Organismo_DET =="ADMINISTRATIVO",Escenario!=0) %>%
  select(-id_emp,-clave,-claveSatAnterior,-descripcionSatAnterior,-tipoSatAnterior,-Organismo,-numplaza,-Organismo_DET)

str(final_set)

# Rename some columns:
names(final_set)[2] <- "idind"
names(final_set)[3] <- "total"
names(final_set)[4] <- "month"
names(final_set)[6] <- "payroll_name"
names(final_set)[9] <- "job_title"
names(final_set)[11] <- "tax_scenario"

str(final_set)

# Squish job_position:
final_set <- final_set %>%
  mutate(job_title = str_squish(job_title))

# Now we will replace ID_Ref with an index to keep employee information private:
id_ref <- final_set %>%
  distinct(ID_Ref) %>%
  rowid_to_column() %>%
  rename(id_employee = rowid)

final_set <- left_join(final_set,id_ref,by="ID_Ref")

final_set <- final_set %>% select(-ID_Ref)

str(final_set)

# We will do the same with payroll_name:
id_payroll <- final_set %>%
  distinct(payroll_name) %>%
  rowid_to_column() %>%
  rename(id_payroll = rowid)

final_set <- left_join(final_set,id_payroll,by="payroll_name")

final_set <- final_set %>% select(-payroll_name)

str(final_set)

# We will do the same with job_title:
# Before eliminating job_title, we will filter out employees which have at least one record with job_title that begin with "J" or "N":
# (Exceptions for Non-employees "N" and retired "J")
discard_observations <- final_set %>%
  mutate(job_title = str_squish(job_title)) %>%
  mutate(first_letter = str_sub(job_title,1,1)) %>%
  filter(first_letter=="N" | first_letter=="N" | first_letter=="J" | first_letter=="j") %>%
  distinct(id_employee)

final_set <- anti_join(final_set,discard_observations,by="id_employee")

job_title <- final_set %>%
  distinct(job_title) %>%
  rowid_to_column() %>%
  rename(id_job_title = rowid)

final_set <- left_join(final_set,job_title,by="job_title")

final_set <- final_set %>% select(-job_title)

str(final_set)

# We will substitute "tipind" and "idind" with "id_concept", since id_concept = tipind + idind:
final_set <- final_set %>%
  mutate(id_concept = str_c(tipind,str_squish(idind),sep="")) %>%
  select(-tipind,-idind)

# We replace id_concept by an index to keep this information private:
id_compensation <- final_set %>%
  distinct(id_concept) %>%
  rowid_to_column() %>%
  rename(id_compensation = rowid)

final_set <- left_join(final_set,id_compensation,by="id_concept")

final_set <- final_set %>% select(-id_concept)

# Our final set is:
str(final_set)

# Export to csv data sets of modified variables:
write.csv(id_compensation,"exports/id_compensations.csv")
write.csv(job_title,"exports/job_title.csv")
write.csv(id_payroll,"exports/id_payroll.csv")

# Save dataset:
saveRDS(final_set,"rda/final_set.rda")
