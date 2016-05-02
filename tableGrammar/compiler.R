source("tableGrammar/parser.R")

library(Hmisc)

transformDefaults = list()#factor=pearson, numeric = kruskal, logical=pearson)


transformToTable <- function(ast, data, transforms)
{
  el <- ast$elements()
  
  cells <- matrix(rep(NA, length(el[[1]])*length(el[[2]])),
                  nrow=length(el[[1]]),
                  dimnames=el)
  
  structure(list(ast = ast, cells=cells), class="table")
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
table <- summaryTG(drug ~ bili + albumin + stage + protime + sex + age + spiders, pbc)
#table <- summaryTG(drug ~ bili, pbc)
table$cells
summary(table)
#index(table)
#html5(table)
#latex(table)



