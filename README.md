# Database Normal Forms and SQL Query Performance

## Goal
In this study, the effects of database normalization (first three normal forms - 1NF, 2NF, and 3NF) on the SQL query performance were put to the test.
SQL performance was assesed following:
* query (successfully) completion within a 30-minute timeput
* query duration (in seconds), for the completed queries 

## Research Design

### The database
Three (sub) schemas of the TPC-H benchmark database were created and populated (using DBGen utility provided by the TPC-H) for:
* Three database servers MS-SQL Server, MySQL, and PostgreSQL
* Two scale factors (0.1 GB and 1GB)
* Three normal forms (!NF, 2NF, and 3NF) of the TPC-H database

### The SQL query set
For each scale factor:
* An initial 1,000 SQL query set was created (randomly) for the original (3NF) TPC-H schema
* The query set was converted for the 2NF and 1NF (mainly by removing the unnecesary joins)
* All the queris were tweaked for execution in all three DBMSs (there are some differences among the three SQL dialects)
Directory data containts the queries executed in PostgreSQL for 1NF, 2NF and 3NF and scale factors of 0.1GB and 1GB.

### Data collection
Query execution results (query completion and query duratiion) were collected using JMeter,
For each query a 30-minute timeout was set (and controlled through JMeter).

### Variables
* dbserver (MS-SQL Server, MySQL, PostgreSQL)
* scale_factor (for this study, the TPC-H database was populated with data of 0.1GB and 1GB)
* normal_form (1NF, 2NF, or 3NF)
* various parametera describing to query complexity (e.g., the number of joins, number of predicated in WHERE, etc.)
* query_completion (whether the query execution was completed during the 30 minute timeout)
* duration_sec (query execution time (in seconds) for each completed query)

### Method
* exploratory data analysis (see scripts 1a and 1b in directory scripts)
* statistical tests for analysing the association between query completion and the normal form (see scripts 2a and 2b in directory scripts)
* machine Learning models (using random forest and extreme gradient boosting algoritms) for:
  - prediction of the odds of query completion (classification)
  - prediction of the  query duration (scoring)
  - estimating the importance of predictor normal_form in the outcome variability (among all other predictors)
  - examination of feature effects on the outcome (using techniques of interpretable ML)


## Results
Results raise serious concerns about the conventional consensus on the performance gains incurred by the reduced number of table joins. 
Even for small-sized databases, the penalties due to the extra volume caused by redundancy associated with lower normal forms
seem larger than the performance gains due to the reduced number of joins. 
