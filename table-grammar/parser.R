library(stringr)
library(R6)

rm("pr")
rm("Parser")
rm("ASTNode")
rm("ASTBranch")
rm("Token")

Token <- R6Class("Token",
  public = list(
    id         = "character",
    name       = "character",
    initialize = function(id, name="")
    {
      self$id   <- id
      self$name <- name
#cat("Token[",id,",",name,"]\n")
    })
)

Parser <- R6Class("Parser", 
  public  = list(
    input = "character",
    pos   = "numeric",
    len   = "numeric",
    initialize = function() {},
    expect = function(id)
    {
      t <- self$nextToken()
      if(t$id != id)
      {
        stop(paste("Expecting",id,"before '",substr(self$input,self$pos,self$len),"'"))
      }

      t
    },
    peek = function()
    {
#cat("peeking at...")
       nt       <- self$nextToken()
       self$pos <- self$pos - nchar(nt$name)
       return(nt)
    },
    r_expression = function()
    {
      match <- str_match(substr(self$input, self$pos, self$len), "^[^\\(\\)]*")
      self$pos <- self$pos + nchar(match[1,1])
      c <- substr(self$input, self$pos, self$pos)
      if (c == "(" )
      {
        self$pos <- self$pos + 1
        self$r_expression()
        self$expect(")")
        return(NA)
      }

      return(NA)
    },
    expression = function()
    {
      nt <- self$nextToken()
      if(nt$id == "LPAREN")
      {
        self$tableFormula()
        self$expect("RPAREN")
        return(NA)
      }
      if(nt$id != "NAME") # An expression starts with either a name or a '('
      {
        stop(paste("Unrecognized token",nt$name,"before",substr(self$input,self$pos,self$len)))
      }
      if(nt$name == "I") # R-expression
      {
        self$expect("LPAREN")
        self$r_expression()
        self$expect("RPAREN") 
        return(NA)
      }
      pk <- self$peek() # What follows the name determines next grammar element
      if(pk$id == "TIMES")
      {
        self$expect("TIMES")
        self$expression()
        return(NA)
      }
      if(pk$id == "LPAREN")
      {
        self$expect("LPAREN")
        self$expression()
        self$expect("RPAREN")
        return(NA)
      }
      # Else it's just a name
      return(NA)
    },
    formula = function()
    {
      self$expression()
      t <- self$peek()
      if(t$id == "PLUS")
      {
        self$expect("PLUS")
        self$expression()
      }
    },
    columnSpecification = function() {
      self$formula()
    },
    rowSpecification    = function() {
      self$formula()
    },
    tableFormula = function()
    {
      self$columnSpecification()      
      self$expect("TILDE") 
      self$rowSpecification()
    },
    run       = function(x)
    {
      self$input <- str_replace_all(x, "[[:space:]]", "")
      self$pos   <- 1    
      self$len   <- nchar(self$input)
   
      self$tableFormula()
      self$expect("EOF")
    },
    nextToken = function()
    {
#cat("**position = ",self$pos)
        # The end?
        if (self$pos == (self$len+1)) {return(Token$new("EOF"))}
        # The parser kept asking for tokens when it shouldn't have
        if (self$pos > self$len)    { stop("Internal Error. No remaining input") }

        x <- substr(self$input, self$pos, self$pos)
        self$pos <- self$pos + 1

        # Look for reserved characters
        if (x == '*')  {return(Token$new("TIMES",  "*") )}
        if (x == '+')  {return(Token$new("PLUS",   "+") )}
        if (x == '(')  {return(Token$new("LPAREN", "(") )}
        if (x == ')')  {return(Token$new("RPAREN", ")") )}
        if (x == '~')  {return(Token$new("TILDE",  "~") )}
 
        # Scan for Name
        #   A syntactically valid name consists of letters, numbers and the dot
        #   or underline characters and starts with a letter or the dot not
        #   followed by a number. 
        match <- str_match(substr(self$input,self$pos-1,self$len),
                           "^([a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z0-9\\._]*")

        if(is.na(match[1,1]))
        {
          stop(paste("Unparseable input starting at",substr(self$input,self$pos-1,self$pos+10)))
        }

        self$pos <- self$pos + nchar(match[1,1]) - 1

        return(Token$new("NAME", match[1,1]))
    }
  )
)

pr <- Parser$new()
pr$run("y ~ x")

pr$run("col1 + col2 ~ drug*age+spiders")

pr$run("I(10^23*rough) ~ spiders + (youth ~ age)")
