
getHdata(pbc)

new_cell <- function(contents=NA)
{
  if(inherits(contents,"list"))
  {
    structure(contents, class="cell")
  }
  else if(is.na(contents))
  {
    structure(list(), class="cell")
  } 
  else
  {
    stop("Improper table cell construction")
  }
}

new_table <- function(rows, cols)
{
  nt <- sapply(1:rows, FUN=function(x) list(sapply(1:cols, FUN=function(x) new_cell())))
  
  structure(nt, class="table")
}

new_label <- function(text, units=NA)
{
  structure(new_cell(list(label=as.character(text), units=as.character(units))), class = "cell_label")
}

new_quantile <- function(quantiles)
{
  structure(new_cell(list(q25=quantiles[2], q50=quantiles[3], q75=quantiles[4])), class="cell_quantile")
}

new_fstat <- function(f, n1, n2, p)
{
  structure(new_cell(list(f=f, n1=n1, n2=n2, p=p)), class="cell_fstat")
}

new_fraction <- function(numerator, denominator)
{
  structure(new_cell(list(numerator=numerator, denominator=denominator)), class="cell_fraction")
}

new_chi2 <- function(chi2, n, p)
{
  structure(new_cell(list(chi2=chi2, n=n, p=p)), class="cell_chi2")
}

# Now test with serum bili row
serum_bili <- new_table(1, 6)
serum_bili[[1]][[1]] <- new_label("Serum Bilirubin", "mg/dl")
serum_bili[[1]][[2]] <- new_label("418")
serum_bili[[1]][[3]] <- new_quantile(quantile(pbc$bili[pbc$drug == "D-penicillamine"]))
serum_bili[[1]][[4]] <- new_quantile(quantile(pbc$bili[pbc$drug == "placebo"]))
serum_bili[[1]][[5]] <- new_quantile(quantile(pbc$bili[pbc$drug == "not randomized"]))
serum_bili[[1]][[6]] <- new_fstat(0.03, 2, 415, 0.972)
