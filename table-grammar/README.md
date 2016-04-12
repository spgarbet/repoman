A Grammar of Tables
===================

Goal
---------------------
To split the syntax of creating a rich summary table into a abstract component, and a very specific reference implementation with functions that contain the assumptions of summaryM inside Hmisc.


General Outline
-----------------
A formula, a data frame (spreadsheet), and a transform function input into the framework will output an abstract table, that can be rendered into text, LaTeX, or HTML5. 

Formulas will be in the 'Cols ~ Rows' syntax. 



Table 9 Example
-------------------
_Statistical Tables and Plots using S and LaTeX_ by FE Harrell, has an example table Table 9, that will be used as the first example.

### Proposal 1

data = getHdata(pbc)

formula = "drug ~ bili + albumin + stage + protime + sex + age + spiders"

functionDefaults = list(factor=pearson, numeric = kruskal, logical=pearson)

_Note: typing in R is impure, and a means to determine type will be needed._

A function applied to a variable in the data may result in the construction of multiple columns. In the Hmisc assumptions, both pearson and kruskal would return
an object {row name, N, [drug results], Test Statistic}

A function name appearing on the right hand side (row), would override the app,lied function. E.g., 
formula = "drug ~ someother(bili)+albumin"

All functions applied in a table generation must return the same columns. However, no constraint is given on what those columns must be.

Note that column variables *must* be categorical in nature. In the case of R, this means a factor, logical or character variable.

In this proposal, the functions "pearson" and "kruskal" would generate the necessary summary columns split across the specified drugs, and contain the specific assumptions that are made in the Hmisc package.


### Proposal 2

