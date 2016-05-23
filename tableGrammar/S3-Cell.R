
getHdata(pbc)

cell <- function(contents=NA)
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

table <- function(rows, cols)
{
  nt <- sapply(1:rows, FUN=function(x) list(sapply(1:cols, FUN=function(x) cell())))
  
  structure(cell(nt), class="table")
}

label_cell <- function(text, units=NA)
{
  structure(cell(list(label=as.character(text), units=as.character(units))), class = "cell_label")
}

quantile_cell <- function(quantiles)
{
  structure(cell(list(q25=quantiles[2], q50=quantiles[3], q75=quantiles[4])), class="cell_quantile")
}

fstat_cell <- function(f, n1, n2, p)
{
  structure(cell(list(f=f, n1=n1, n2=n2, p=p)), class="cell_fstat")
}

fraction_cell <- function(numerator, denominator)
{
  structure(cell(list(numerator=numerator, denominator=denominator)), class="cell_fraction")
}

chi2_cell <- function(chi2, n, p)
{
  structure(cell(list(chi2=chi2, n=n, p=p)), class="cell_chi2")
}

# Now test with serum bili row
serum_bili <- table(1, 6)
serum_bili[[1]][[1]] <- label_cell("Serum Bilirubin", "mg/dl")
serum_bili[[1]][[2]] <- label_cell("418")
serum_bili[[1]][[3]] <- quantile_cell(quantile(pbc$bili[pbc$drug == "D-penicillamine"]))
serum_bili[[1]][[4]] <- quantile_cell(quantile(pbc$bili[pbc$drug == "placebo"]))
serum_bili[[1]][[5]] <- quantile_cell(quantile(pbc$bili[pbc$drug == "not randomized"]))
serum_bili[[1]][[6]] <- fstat_cell(0.03, 2, 415, 0.972)

master <- table(2,1)
master[[1]][[1]] <- serum_bili
