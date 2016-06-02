
getHdata(pbc)

tg_cell <- function(contents=NA)
{
  if(inherits(contents,"list"))
  {
    structure(contents, class="tg_cell")
  }
  else if(is.na(contents))
  {
    structure(list(), class="tg_cell")
  } 
  else
  {
    print(traceback())
    stop("Improper table cell construction")
  }
}

tg_table <- function(rows, cols, embedded=FALSE)
{
  nt <- sapply(1:rows, FUN=function(x) list(sapply(1:cols, FUN=function(x) tg_cell())))
  
  structure(tg_cell(nt), class=c("tg_table", "tg_cell") ) 
}

tg_label <- function(text, units=NA)
{
  structure(tg_cell(list(label=as.character(text), units=as.character(units))), class = c("tg_label", "tg_cell"))
}

tg_quantile <- function(quantiles)
{
  structure(tg_cell(list(q25=quantiles[2], q50=quantiles[3], q75=quantiles[4])), class=c("tg_quantile", "tg_cell"))  
}

tg_fstat <- function(f, n1, n2, p)
{
  structure(tg_cell(list(f=f, n1=n1, n2=n2, p=p)), class=c("tg_fstat","tg_cell"))
}

tg_fraction <- function(numerator, denominator)
{
  structure(tg_cell(list(numerator=numerator, denominator=denominator)), class=c("tg_fraction","tg_cell"))
}

tg_chi2 <- function(chi2, n, p)
{
  structure(tg_cell(list(chi2=chi2, n=n, p=p)), class=c("tg_chi2", "tg_cell"))
}

# Now test with serum bili row
serum_bili <- tg_table(1, 6)
serum_bili[[1]][[1]] <- tg_label("Serum Bilirubin", "mg/dl")
serum_bili[[1]][[2]] <- tg_label("418")
serum_bili[[1]][[3]] <- tg_quantile(quantile(pbc$bili[pbc$drug == "D-penicillamine"]))
serum_bili[[1]][[4]] <- tg_quantile(quantile(pbc$bili[pbc$drug == "placebo"]))
serum_bili[[1]][[5]] <- tg_quantile(quantile(pbc$bili[pbc$drug == "not randomized"]))
serum_bili[[1]][[6]] <- tg_fstat(0.03, 2, 415, 0.972)



