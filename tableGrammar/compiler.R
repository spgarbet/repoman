source("tableGrammar/parser.R")
source("tableGrammar/S3-Cell.R")

library(Hmisc)

## Default Summary Functions

#### Data required:
#row_idx
#col_idx
#lbl

# FIXME: This function needs to be able to "flip"
summarize_kruskal_horz <- function(data, row, column)
{
  categories <- levels(data[,column])
  
  tbl <- tg_table(1, length(categories) + 2, TRUE) # n + no. categories + test statistic
  
  tbl[[1]][[1]] <- tg_label(as.character(sum(!is.na(data[,row]))))
  
  sapply(1:length(categories), FUN=function(category) {
    tbl[[1]][[category+1]] <- tg_quantile(quantile(data[pbc[,column] == categories[category], row], na.rm=TRUE))
  })
  
  # TODO: Need test statistic here.

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

tg_create_table <- function(ast, data, transforms)
{
  elements <- ast$elements()
  
  lbl <- labels(elements, data)

  height <- length(lbl[[1]]) - 1
  width  <- length(lbl[[2]])
  
  tbl <- tg_table(height, width)

  sapply(1:width, FUN=function(col_idx) {
    column <- elements[[1]][col_idx]
    
    sapply(1:height, FUN=function(row_idx) {
      row <- elements[[2]][row_idx]

      #FIXME!!!!
      inner_tbl[[1]][[1]] <- tg_label(lbl[[1]][row_idx+1]) # FIXME: Split out units
            
      transform <- transforms[[class(row)]][[class(column)]]

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

library(Hmisc)
getHdata(pbc)
#table <- summaryTG(drug ~ bili + albumin + stage + protime + sex + age + spiders, pbc)
#table <- summaryTG(drug ~ bili, pbc)
#test_table <- tg_summary(drug ~ bili + albumin + protime + age, pbc)

#test_table

#summary(table)
#index(table)
#html5(table)
#latex(table)

