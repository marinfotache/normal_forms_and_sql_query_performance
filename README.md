# Database Normal Forms and SQL Query Performance

## Goal
In this study, the effects of database normalization (first three normal forms - 1NF, 2NF, and 3NF) on the SQL query performance were put to the test.

SQL performance was assesed following:
* query (successfully) completion within a 30-minute timeout
* query duration (in seconds), for the completed queries 


## Research Design

### The database
Three (sub)schemas of the TPC-H benchmark database were created and populated (using the DBGen utility provided by the TPC-H) for:
* three database servers MS-SQL Server, MySQL, and PostgreSQL
* two scale factors (0.1 GB and 1GB)
* three normal forms (1NF, 2NF, and 3NF) of the TPC-H database

### The SQL query set
For each scale factor:
* an initial 1,000 SQL query set was created (randomly) for the original (3NF) TPC-H schema
* this initial query set was adapted for execution in 2NF and 1NF (mainly by removing the unnecessary joins)
* all the queris were tweaked for execution in all three DBMSs (there are some differences among the three SQL dialects)
  
Directory __queries__ containts the queries executed in PostgreSQL for 1NF, 2NF and 3NF and scale factors of 0.1GB and 1GB.


### Data collection
Query execution results (query completion and query duratiion) were collected using JMeter,

For each query, a 30-minute timeout was set (and controlled through JMeter).

Directory __data__ containts the files with the query parameters and the query execution results.


### Variables
* _dbserver_ (MS-SQL Server, MySQL, PostgreSQL)
* _scale_factor_ (for this study, the TPC-H database was populated with data of 0.1GB and 1GB)
* _normal_form_ (1NF, 2NF, or 3NF)
* various parameters describing to query complexity (e.g., the number of joins, number of predicated in WHERE, etc.)
* _query_completion_ (whether the query execution was completed during the 30 minute timeout)
* _duration_sec) (query execution time in seconds for each completed query)

### Method
* exploratory data analysis (see scripts __1a..._ and __1b...__ in directory __scripts__)
* statistical tests for analysing the association between query completion and the normal form (see scripts __2a...__ and __2b...__ in directory __scripts__)
* machine Learning models based on random forest and extreme gradient boosting algoritms (see scripts __3a...__ and __3b...__ in directory __scripts__) for:
  - prediction of odds of _query_completion_ (classification)
  - prediction of query _duration_sec_ (scoring)
  - estimating the importance (among all predictors) of the predictor _normal_form_ in the outcome variability (_query_completion_ and _duration_sec__
  - examination of feature effects on the outcome (using techniques of interpretable ML)


## Results
Results raise serious concerns about the conventional consensus on the performance gains incurred by the reduced number of table joins. 
Even for small-sized databases, the penalties due to the extra volume caused by redundancy associated with lower normal forms
seem larger than the performance gains due to the reduced number of joins. 
