source("tableGrammar/parser.R")
source("tableGrammar/S3-Cell.R")

library(Hmisc)

## Default Summary Functions

summarize_kruskal_horz <- function(data, row, column)
{
  categories <- levels(data[,column])
  
  # TODO: Table needs a "col / row label"
  tbl <- tg_table(1, length(categories) + 2, TRUE) # n + #categories + test statistic
  
  # N value
  N <- sum(!is.na(data[,row]))
  tbl[[1]][[1]] <- tg_label(as.character(N))
  
  # The quantiles by category
  sapply(1:length(categories), FUN=function(category) {
    tbl[[1]][[category+1]] <- tg_quantile(quantile(data[pbc[,column] == categories[category], row], na.rm=TRUE))
  })
  
  # Kruskal-Wallis via F-distribution
  test <- spearman2(data[,column], data[,row], na.action=na.retain)
  
  tbl[[1]][[length(categories)+2]] <- tg_fstat(test$F, test$df1, test$df2, test$P)

  tbl  
}

summarize_kruskal_vert <- function(data, row, column)
{
  tg_table(1, 1, TRUE)
}

summarize_pearson <- function(data, row, column)
{
  tg_table(1, 1, TRUE)
}

summarize_spearman <- function(data, row, column)
{
  tg_table(1, 1, TRUE)
}

# Top  level list is row "class"
# Next level list is column "class"
transformDefaults = list(
  numeric = list(
              numeric = summarize_spearman,
              factor  = summarize_kruskal_horz,
              logical = summarize_kruskal_horz
            ),
  factor  = list(
              numeric = summarize_kruskal_vert,
              factor  = summarize_pearson,
              logical = summarize_pearson
            ),
  logical = list(
              numeric = summarize_kruskal_vert,
              factor  = summarize_pearson,
              logical = summarize_pearson
            )
)


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

data_type <- function(x)
{
  if(     is.factor(x))  "factor"
  else if(is.logical(x)) "logical"
  else if(is.numeric(x)) "numeric"
  else                   stop(paste("Unsupported class/type - ",class(x), typeof(x)))
}

tg_create_table <- function(ast, data, transforms)
{
  elements <- ast$elements()

  width  <- length(elements[[1]]) + 1
  height <- length(elements[[2]]) + 1
  tbl    <- tg_table(height, width)

  sapply(2:width, FUN=function(col_idx) {
    column <- elements[[1]][col_idx-1]
    
    sapply(2:height, FUN=function(row_idx) {
      row <- elements[[2]][row_idx-1]
      
      rowtype <- data_type(data[,row])
      coltype <- data_type(data[,column])

      transform <- transforms[[rowtype]][[coltype]]

      # The +1 leaves room for labels
      tbl[[row_idx]][[col_idx]] <<- transform(data, row, column)
    })
  })
  
  tbl
}

tg_summary <- function(formula, data, transforms=transformDefaults)
{
  ast <- Parser$new()$run(formula)
  
  tg_create_table(ast, data, transforms)
}

summary.table <- function(object)
{
  print(object$cells)
}

getHdata(pbc)
table <- summaryTG(drug ~ bili + albumin + stage + protime + sex + age + spiders, pbc)
#table <- summaryTG(drug ~ bili, pbc)
test_table <- tg_summary(drug ~ bili + albumin + protime + age, pbc)

test_table

#summary(table)
#index(table)
#html5(table)
#latex(table)

