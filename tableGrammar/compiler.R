source("tableGrammar/parser.R")
source("tableGrammar/S3-Cell.R")

library(Hmisc)
getHdata(pbc)

### function to round values to N significant digits
# input:   vec       vector of numeric
#          n         integer is the required sigfig  
# output:  outvec    vector of numeric rounded to N sigfig
sigfig <- function(vec, n=3)
{ 
  formatC(signif(vec,digits=n), digits=n,format="fg", flag="#") 
}   

## Default Summary Functions

summarize_kruskal_horz <- function(data, row, column)
{
  categories <- levels(data[,column])
  if (is.null(categories)) {unique(data[,row])}
  
  # TODO: Table needs a "col / row label" Maybe on expansion?
  # 1 X (n + no. categories + test statistic)
  tbl <- tg_table(1, length(categories) + 2, TRUE)
  
  # N value
  N <- sum(!is.na(data[,row]))
  tbl[[1]][[1]] <- tg_label(as.character(N))
  
  # The quantiles by category
  sapply(1:length(categories), FUN=function(category) {
    tbl[[1]][[category+1]] <<- tg_quantile(quantile(data[data[,column] == categories[category], row], na.rm=TRUE))
  })
  
  # Kruskal-Wallis via F-distribution
  test <- spearman2(data[,column], data[,row], na.action=na.retain)
  
  tbl[[1]][[length(categories)+2]] <- tg_fstat(test['F'], test['df1'], test['df2'], test['P'])

  tbl  
}

summarize_kruskal_vert <- function(data, row, column)
{
  categories <- levels(data[,row])
  if (is.null(categories)) {unique(data[,row])}
  
  # TODO: Table needs a "col / row label" Maybe on expansion?
  # N value
  #N <- sum(!is.na(data[,row]))
  #tbl[[1]][[1]] <- tg_label(as.character(N))
  
  tbl <- tg_table(length(categories), 3, TRUE) # no. categories X 3
  
  # The quantiles by category
  sapply(1:length(categories), FUN=function(category) {
    x <- data[data[,row] == categories[category], column]
    tbl[[category]][[1]] <<- tg_label(as.character(length(x)))
    tbl[[category]][[2]] <<- tg_quantile(quantile(x, na.rm=TRUE))
  })
  
  # Kruskal-Wallis via F-distribution
  test <- spearman2(data[,row], data[,column], na.action=na.retain)
  
  tbl[[1]][[3]] <- tg_fstat(test['F'], test['df1'], test['df2'], test['P'])
  
  tbl  
}

summarize_chisq <- function(data, row, column)
{
  row_categories <- rev(levels(data[,row])) # FIXME: Why are levels reversed to match summaryM?
  if (is.null(row_categories)) {unique(data[,row])}
  
  col_categories <- levels(data[,column])
  if (is.null(col_categories)) {unique(data[,column])}
  
  n <- length(row_categories)
  m <- length(col_categories)
  
  # TODO: Table needs a "col / row label" Maybe on expansion?
  
  # N X (M+2)
  tbl <- tg_table(n, m+2, TRUE)
  
  N <- length(data[!is.na(data[,row]) & !is.na(data[,column]),row])
  tbl[[1]][[1]] <- tg_label(as.character(N))
  
  # The fractions by category intersection
  sapply(1:length(col_categories), FUN=function(col_category) {
    c_x <- data[data[,column] == col_categories[col_category], column]
    c_x <- c_x[!is.na(c_x)]
    denominator <- length(c_x)
    sapply(1:length(row_categories), FUN=function(row_category) {
      c_xy <- data[data[,column] == col_categories[col_category] &
                   data[,row]    == row_categories[row_category], column]
      c_xy <- c_xy[!is.na(c_xy)]
      numerator <- length(c_xy)
      tbl[[row_category]][[col_category+1]] <<- tg_fraction(numerator, denominator)
    })
    
  })
  
  test <- chisq.test(table(data[,row],data[,column]), correct=FALSE)
  
  tbl[[1]][[m+2]] <- tg_chi2(test$statistic, test$parameter, test$p.value)
  
  if(length(tbl) == 2)
  {
    tbl[[2]] <- NULL
  }
  
  tbl
}

summarize_spearman <- function(data, row, column)
{
  tbl <- tg_table(1, 3, TRUE)
  
  # MAYBE, this should use pvrank if it can
  test <- cor.test(data[,row], data[,column], alternate="two.sided", method="spearman", na.action=na.omit, exact=FALSE)
  
  n <- length(data[!is.na(data[,row]) & !is.na(data[,column])  ,row])
  
  tbl[[1]][[1]] <- tg_label(as.character(n) )
  
  tbl[[1]][[2]] <- tg_estimate(test$estimate)
  
  # Reversed engineered from cor.test for spearman  
  r <- test$estimate
  statistic <- r/sqrt((1 - r^2)/(n - 2))
  
  tbl[[1]][[3]] <- tg_studentt(statistic, n-2, test$p.value)
  
}

summarize_ordinal_lr <- function(data, row, column)
{
  tg_table(1, 1, TRUE)
}

# Used to determine "type" of data for transform
data_type <- function(x)
{
  if(     is.ordered(x)) "ordered"
  else if(is.factor(x) ) "factor"
  else if(is.logical(x)) "logical"
  else if(is.numeric(x)) "numeric"
  else                   stop(paste("Unsupported class/type - ",class(x), typeof(x)))
}

# Top  level list is row "class"
# Next level list is column "class"
# TODO: ordered needs to be made sensible
transformDefaults = list(
  numeric = list(
              numeric = summarize_spearman,
              factor  = summarize_kruskal_horz,
              logical = summarize_kruskal_horz,
              ordered = summarize_ordinal_lr
            ),
  factor  = list(
              numeric = summarize_kruskal_vert,
              factor  = summarize_chisq,
              logical = summarize_chisq,
              ordered = summarize_ordinal_lr
            ),
  logical = list(
              numeric = summarize_kruskal_vert,
              factor  = summarize_chisq,
              logical = summarize_chisq,
              ordered = summarize_ordinal_lr
            ),
  ordered = list(
              numeric = summarize_kruskal_vert,
              factor  = summarize_chisq,
              logical = summarize_chisq,
              ordered = summarize_ordinal_lr
            )
)


#labels <- function(elements, data)
#{
#  rows <- sapply(elements[[2]],
#    FUN=function(x) 
#    {
#      l <- x
#      try({
#        l2 <- label(data[x])
#        if(nchar(l2)>0) {l<-l2}
#      })
#      l
#    }
#  )
#  
#  cols <- sapply(elements[[1]],
#    FUN=function(x)
#    {
#      l <- x
#      try({
#        l2 <- label(data[x])
#        if(nchar(l2)>0) {l<-l2}
#      })
#      l
#    }
#  )
#  
#  list(c("",rows), cols)
#}


tg_flatten <- function(table)
{
  rows    <- 0
  cols    <- 0
  
  sapply(1:length(table), FUN=function(row) {
    element <- table[[row]][[1]]
    if(inherits(element, "tg_table") && attr(element, "embedded"))
      rows <<- rows + length(element)
    else
      rows <<- rows + 1
  })
  sapply(1:length(table[[1]]), FUN=function(col){
    element <- table[[1]][[col]]
    if(inherits(element, "tg_table") && attr(element, "embedded"))
      cols <<- cols + length(element[[1]])
    else
      cols <<- cols + 1
  })
  
  label_rows <- 1 #FIXME: Should be possible to have more than 1
  label_cols <- 1
  new_tbl <- tg_table(rows+label_rows, cols+label_cols) 
  
  output_row <- label_rows + 1
  sapply(1:length(table), FUN=function(row) {
    output_col <- label_cols + 1
    sapply(1:length(table[[row]]), FUN=function(col) {
      element <- table[[row]][[col]]
      
      #cat(row, col, class(element), '\n')
      #cat("   => ", output_row, output_col, '\n')
      
      if(inherits(element, "tg_table") && attr(element, "embedded"))
      {
        ## Need another double sapply here.
        sapply(element, FUN=function(inner_row)
        {
          sapply(inner_row, FUN=function(inner_element)
          {
            new_tbl[[output_row]][[output_col]] <<- inner_element
            output_col <<- output_col + 1
          })
          output_col <<- output_col - length(inner_row)
          output_row <<- output_row + 1
        })
        output_row <<- output_row - length(element)
      }
      else
      {
        new_tbl[[output_row]][[output_col]] <<- element
      }
      output_col <<- output_col + length(element[[1]])
      
    })
    output_row <<- output_row + length(table[[row]][[1]])
  })
  
  new_tbl
}

tg_create_table <- function(ast, data, transforms, data_type_fun)
{
  elements <- ast$elements()

  width  <- length(elements[[1]])
  height <- length(elements[[2]])
  tbl    <- tg_table(height, width)

  sapply(1:width, FUN=function(col_idx) {
    column <- elements[[1]][col_idx]
    
    sapply(1:height, FUN=function(row_idx) {
      row <- elements[[2]][row_idx]
      
      rowtype <- data_type_fun(data[,row])
      coltype <- data_type_fun(data[,column])

      transform <- transforms[[rowtype]][[coltype]]

      tbl[[row_idx]][[col_idx]] <<- transform(data, row, column)
    })
  })
  
 # tg_flatten(tbl)
  tbl
}

tg_summary <- function(formula, data, transforms=transformDefaults, data_type_fun=data_type)
{
  ast <- Parser$new()$run(formula)
  
  tg_create_table(ast, data, transforms, data_type_fun)
}


summary.tg_cell <- function(object) ""

summary.tg_label <- function(object) 
{
  if(is.na(object$units))
    object$label
  else
    paste(object$label, " (", object$units, ")", sep="")
}

summary.tg_quantile <- function(object)
{
  paste(sigfig(object$q25), " *", sigfig(object$q50), "* ", sigfig(object$q75), sep="")
}

summary.tg_table <- function(object)
{
  rows <- length(object)
  cols <- length(object[[1]])
  
  text <- matrix(data=rep("", rows*cols), nrow=rows, ncol=cols)
  
  sapply(1:rows, FUN=function(row) {
    sapply(1:cols, FUN=function(col) {
      text[row,col] <<- summary(object[[row]][[col]])
    })
  })
  
  text
}

summary.tg_estimate <- function(object)
{
  if(is.na(object$low))
    as.character(object$value)
  else
    paste(object$value, " (",object$low,",",object$high,")")
}

summary.tg_fstat <- function(object)
{
  paste("F_{",object$n1,",",object$n2,"}=",round(object$f,2)," P=",round(object$p,3),sep="")
}

summary.tg_fraction <- function(object)
{
  paste(round(100*object$numerator/object$denominator,0),"%  ",
        object$numerator,"/",object$denominator,
        sep="")
}

summary.tg_chi2 <- function(object)
{
  paste("X^2_",object$df,"=",round(object$chi2,2)," P=",round(object$p,3),sep="")
}


#data(pbc)
pbc$stage <- factor(pbc$stage, levels=1:4, ordered=TRUE)
test_table <- tg_summary(drug ~ bili + albumin + stage + protime + sex + age + spiders, pbc)
#table <- summaryTG(drug ~ bili, pbc)
#test_table <- tg_summary(drug ~ bili + albumin + protime + age, pbc)

flat <- tg_flatten(test_table)

#summary(table)
#index(table)
#html5(table)
#latex(table)


