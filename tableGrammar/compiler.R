source("tableGrammar/parser.R")

library(Hmisc)

transformDefaults = list()#factor=pearson, numeric = kruskal, logical=pearson)

labels <- function(elements, data)
{
  rows <- sapply(elements[[2]],
    FUN=function(x) 
    {
print(x)
      l <- x
      try({
        l2 <- label(data[x])
        if(nchar(l2)>0) {l<-l2}
      })
      l
    }
  )
  
  cols <- sapply(elements[[1]],
    FUN=function(x)
    {
      l <- x
      try({
        l2 <- label(data[x])
        if(nchar(l2)>0) {l<-l2}
      })
      l
    }
  )
  
  list(c("",rows), cols)
}

transformToTable <- function(ast, data, transforms)
{
  lbl <- labels(ast$elements(), data)

  height <- length(lbl[[1]])
  width  <- length(lbl[[2]])
  
  cells <- matrix(rep(NA, height*width),
                  nrow=height,
                  dimnames=lbl)
  
  structure(list(ast = ast, cells=cells), class="table")
}

summaryTG <- function(formula, data, transforms=transformDefaults)
{
  ast <- Parser$new()$run(formula)
  
  transformToTable(ast, data, transforms)
}

summary.table <- function(object)
{
  print(object$cells)
}

getHdata(pbc)
table <- summaryTG(drug ~ bili + albumin + stage + protime + sex + age + spiders, pbc)
#table <- summaryTG(drug ~ bili, pbc)
table$cells
summary(table)
#index(table)
#html5(table)
#latex(table)



