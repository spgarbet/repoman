library(stringr)
library(R6)

rm("pr")
rm("Parser")
rm("ASTNode")
rm("ASTBranch")
rm("Token")

ASTNode <- R6Class("ASTNode",
  public = list(
    symbol = "character",
    value  = "character",
    initialize = function(symbol, value) 
    {
      self$symbol <- symbol
      self$value  <- value
    }
  )
)

ASTBranch <- R6Class("ASTBranch",
  inherit = ASTNode,
  public = list(
    left  = "ASTNode",
    right = "ASTNode",
    initialize = function(symbol, left, right, value="")
    {
      self$symbol <- symbol
      self$left   <- left
      self$right  <- right
      self$value  <- value
    }
  )
)

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
      starting <- self$pos
      self$pos <- self$pos + nchar(match[1,1])
      # Didn't call tokenizer for peek, due to different grammar of R expressions
      c <- substr(self$input, self$pos, self$pos)
      if (c == "(" )
      {
        self$pos <- self$pos + 1 # Eat that character
        rexpr <- self$r_expression()
        self$expect("RPAREN")
        rexpr <- self$r_expression() # Continue the r_expr
        return(ASTNode$new("r_expr", substr(self$input, starting, self$pos-1)))
      }

      return(ASTNode$new("r_expr", substr(self$input, starting, self$pos-1)))
    },
    expression = function()
    {
      nt <- self$nextToken()
      if(nt$id == "LPAREN")
      {
        tf <- self$tableFormula()
        self$expect("RPAREN")
        return(tf)
      }
      if(nt$id != "NAME") # An expression starts with either a name or a '('
      {
        stop(paste("Unrecognized token",nt$name,"before",substr(self$input,self$pos,self$len)))
      }
      if(nt$name == "I") # R-expression
      {
        self$expect("LPAREN")
        r_expr <- self$r_expression()
        self$expect("RPAREN") 
        return(r_expr)
      }
      pk <- self$peek() # What follows the name determines next grammar element
      if(pk$id == "TIMES")
      {
        self$expect("TIMES")
        expr <- self$expression()
        return(ASTBranch$new("permute", ASTNode$new("name", nt$name), expr))
      }
      if(pk$id == "LPAREN")
      {
        self$expect("LPAREN")
        expr <- self$expression()
        self$expect("RPAREN")
        return(ASTBranch$new("transform", expr, NA, nt$name))
      }
      # Else it's just a name
      return(ASTNode$new("name",nt$name))
    },
    formula = function()
    {
      l_expr  <- self$expression()
      r_expr  <- NA
      t <- self$peek()
      if(t$id == "PLUS")
      {
        self$expect("PLUS")
        r_expr <- self$formula()
      }

      return(ASTBranch$new("plus", l_expr, r_expr))      
    },
    columnSpecification = function() {
      self$formula()
    },
    rowSpecification    = function() {
      self$formula()
    },
    tableFormula = function()
    {
      cs <- self$columnSpecification()      
      self$expect("TILDE") 
      rs <- self$rowSpecification()
      
      return(ASTBranch$new("table", cs, rs))
    },
    run       = function(x)
    {
      self$input <- str_replace_all(x, "[[:space:]]", "")
      self$pos   <- 1    
      self$len   <- nchar(self$input)
   
      tf <- self$tableFormula()
      self$expect("EOF")
      return(tf)
    },
    nextToken = function()
    {
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

ast <- pr$run("col1 + col2 ~ drug*age+spiders")

ast2 <- pr$run("I(10^23*(rough+2)+3) ~ spiders + (youth ~ age)+more")
