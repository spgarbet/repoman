source("tableGrammar/parser.R")
source("tableGrammar/S3-Cell.R")


library(Hmisc)

transformDefaults = list()#numeric = kruskal)#, factor=pearson, logical=pearson)

labels <- function(elements, data)
{
  rows <- sapply(elements[[2]],
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
  elements <- ast$elements()
  
  lbl <- labels(elements, data)

  height <- length(lbl[[1]]) -1
  width  <- length(lbl[[2]])
  
  tbl <- table(height, width)

  sapply(1:width, FUN=function(col_idx) {
    column <- elements[[1]][col_idx]
    categories <- levels(data[,column])
    
    sapply(1:height, FUN=function(row_idx) {
      # ASSUMPTION, column is categorical, i.e. factor -- need to relax this assumption

      row    <- elements[[2]][row_idx]
      
      inner_tbl <- table(1, length(categories) + 3) # name + n + no. categories + test statistic


      inner_tbl[[1]][[1]] <- label_cell(lbl[[1]][row_idx+1]) # Need to split out units...
      
      inner_tbl[[1]][[2]] <- label_cell(as.character(sum(!is.na(data[,row]))))
      
      sapply(1:length(categories), FUN=function(category) {
        inner_tbl[[1]][[category+2]] <- quantile_cell(quantile(data[pbc[,column] == categories[category], row], na.rm=TRUE))
      })
      
      tbl[[row_idx]][[col_idx]] <- inner_tbl
    })
  })
  
  tbl
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
#table <- summaryTG(drug ~ bili + albumin + stage + protime + sex + age + spiders, pbc)
#table <- summaryTG(drug ~ bili, pbc)
table <- summaryTG(drug ~ bili + albumin + protime + age, pbc)

table

#summary(table)
#index(table)
#html5(table)
#latex(table)



