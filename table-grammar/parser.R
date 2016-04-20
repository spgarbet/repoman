library(stringr)

Token <- setRefClass("Token",
  fields = list(id   = "character",
                name = "character")
)

rm("pr")
rm("Parser")

Parser <- setRefClass("Parser", 
  fields  = list(input = "character",
                 pos   = "numeric",
                 len   = "numeric"),
  methods = list(
    token = function(id)
    {
      t <- nextToken()
      if(t$id != id)
      {
        stop(paste("Expecting",id,"before '",substr(input,pos,len),"'"))
      }

      t
    },
    peek = function()
    {
       nt   <-  nextToken()
       pos  <<- pos - length(nt$name)
       return(nt)
    },
    r_expression = function()
    {
      match <- str_match(substr(input, pos, len), "^[^\\(\\)]*")
      pos <<- pos + nchar(match[1,1])
      c <- substr(input, pos, pos)
      if (c == "(" )
      {
        pos <<- pos + 1
        r_expression()
        token(")")
        return(NA)
      }

      return(NA)
    },
    expression = function()
    {
      nt <- nextToken()
      if(nt$id == "LPAREN")
      {
        tableFormula()
        token("RPAREN")
        return(NA)
      }
      if(nt$id != "NAME") # An expression starts with either a name or a '('
      {
        stop(paste("Unrecognized token",nt$name,"before",substr(input,pos,len)))
      }
      if(nt$name == "I") # R-expression
      {
        token("LPAREN")
        r_expression()
        token("RPAREN") 
        return(NA)
      }
      pk <- peek() # What follows the name determines next grammar element
      if(pk$id == "TIMES")
      {
        token("TIMES")
        expression()
        return(NA)
      }
      if(pk$id == "LPAREN")
      {
        token("LPAREN")
        expression()
        token("RPAREN")
        return(NA)
      }
      # Else it's just a name
      return(NA)
    },
    formula = function()
    {
      expression()
      t <- peek()
      if(t$id == "PLUS")
      {
        token("PLUS")
        expression()
      }
    },
    columnSpecification = function() {
      formula()
    },
    rowSpecification    = function() {
      formula()
    },
    tableFormula = function()
    {
      columnSpecification()      
      token("TILDE") 
      rowSpecification()
    },
    run       = function(x)
    {
      input <<- str_replace_all(x, "[[:space:]]", "")
      pos   <<- 1    
      len   <<- nchar(input)
   
      tableFormula()
      token("EOF")
    },
    nextToken = function()
    {
        # The end?
        if (pos == len+1) {return(Token$new(id="EOF"))}
        # The parser kept asking for tokens when it shouldn't have
        if (pos > len)    { stop("Internal Error. No remaining input") }

        x <- substr(input, pos, pos)
        pos <<- pos + 1

        # Look for reserved characters
        if (x == '*')  {return(Token$new(id="TIMES", name="*") )}
        if (x == '+')  {return(Token$new(id="PLUS",  name="+") )}
        if (x == '(')  {return(Token$new(id="LPAREN",name="(") )}
        if (x == ')')  {return(Token$new(id="RPAREN",name=")") )}
        if (x == '~')  {return(Token$new(id="TILDE", name="~") )}
 
        # Scan for Name
        #   A syntactically valid name consists of letters, numbers and the dot
        #   or underline characters and starts with a letter or the dot not
        #   followed by a number. 
        match <- str_match(substr(input,(pos-1),len),
                           "^([a-zA-Z]|\\.[a-zA-Z_])[a-zA-Z0-9\\._]*")

        if(is.na(match[1,1]))
        {
          stop(paste("Unparseable input starting at",substr(input,(pos-1),(pos+10))))
        }

        pos <<- pos + nchar(match[1,1]) - 1

        return(Token$new(id="NAME", name=match[1,1]))
    }
  )
)

pr <- Parser$new()
pr$run("y ~ x")

pr$run("col1 + col2 ~ drug*age+spiders")

pr$run("I(10^23*rough) ~ spiders + (youth ~ age)")
