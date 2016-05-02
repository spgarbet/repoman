source("tableGrammar/parser.R")

library(Hmisc)

transformDefaults = list()#factor=pearson, numeric = kruskal, logical=pearson)

transformToTable <- function(ast, data, transforms)
{
  tbl <- structure(list(ast = ast), class="table")
  
  tbl
}

summaryTG <- function(formula, data, transforms=transformDefaults)
{
  ast <- Parser$new()$run(formula)
  
  transformToTable(ast, data, transforms)
}

summary.table <- function(object)
{
  "YO!"
}

getHdata(pbc)
#table <- summaryTG(drug ~ bili + albumin + stage + protime + sex + age + spiders, pbc)
table <- summaryTG(drug ~ bili, pbc)

summary(table)
#index(table)
#html5(table)
#latex(table)



