A Grammar of Tables
===================

Goal
---------------------
To split the syntax of creating a rich summary table into a abstract component, and a very specific reference implementation with functions that contain the assumptions of summaryM inside Hmisc.

This will allow generalized computation of tables, and provide a means to generate a rich index for using in Reproducible Manuscripts.


General Outline
-----------------
A formula, a data frame (spreadsheet), and a transform function input into the framework will output an abstract table, that can be rendered into text, LaTeX, or HTML5. 

Formulas will be in the 'Cols ~ Rows' syntax. 

Requirements
-----------------
1. It must render to LaTeX, Text, HTML5, RMarkdown, Index table.
2. It must allow for user override of any derivation function.
3. It must allow for user override of any rendering function.
4. It must be easily extensible. I.e., any user overrides should require a minimum of fuss / syntax for the end user.
5. Index table must be user specified name based, and not numeric numbers.
6. Index table must be repeatible, and contain search information.
7. It should reproduce by default, Hmisc summaryM behaviors.

Table 9 Example
-------------------
_Statistical Tables and Plots using S and LaTeX_ by FE Harrell, has an example table Table 9, that will be used as the first example.

### Interface Proposal
data = getHdata(pbc)

formula = "drug ~ bili + albumin + stage + protime + sex + age + spiders"

functionDefaults = list(factor=pearson, numeric = kruskal, logical=pearson)

_Note: typing in R is impure, and a means to determine type will be needed._

A function applied to a variable in the data may result in the construction of multiple columns. In the Hmisc assumptions, both pearson and kruskal would return
an object {row name, N, [drug results], Test Statistic}

A function name appearing on the right hand side (row), would override the app,lied function. E.g., 
formula = "drug ~ someother(bili)+albumin"

All functions applied in a table generation must return the same columns. However, no constraint is given on what those columns must be.

Note that column variables *must* be categorical in nature. In the case of R, this means a factor, logical or character variable.

In this proposal, the functions "pearson" and "kruskal" would generate the necessary summary columns split across the specified drugs, and contain the specific assumptions that are made in the Hmisc package.

### Design as Collection of Rows

The memory representation will be a table is a collection of rows that have alignment information.

A Haskell type representation of this idea is tested with the following code:

        {-# OPTIONS_GHC -XDatatypeContexts #-}

        --------------------------------------------------------------
        -- Haskell formal type declaration of table relations
        --
        -- The type class definition of functions that must be supported to be
        -- of class "Renderable"
        -- This applies to both a Table or an individual Cell of a table
        class Show a => Renderable a where
          latex     :: a -> String
          rmarkdown :: a -> String
          html5     :: a -> String
          label     :: a -> String -- This is for indexing
          show      :: a -> String -- Haskell version of R summary
        
        -- A stop is simply a string id
        type Stop = String
        
        -- Alignment is a collection of ordered stops
        data Alignment = Alignment {stops :: [Stop]}
        
        -- A cell is renderable content, and has a right and left stop
        data Renderable a => Cell a = Cell { leftalign  :: Stop,
                                             content    :: a,
                                             rightalign :: Stop}
        
        -- A Row is a collection Renderables with left and right stop
        -- http://stackoverflow.com/questions/18934882/haskell-line-of-code-not-compiling-illegal-datatype-context
        data Row a = Row {cells :: [(Cell a)]}
        
        -- A Table is a collection of descending rows with Alignment information
        data Table a = Table {rows :: [Row a], alignment ::  Alignment}

This demonstrates the feasibility of constructing objects which correspond to these types, and serves as a definition. 

### Design as a Table of Tables

An alternate approach would be to have a Table be a grid of Tables or a Renderable Cell. Boundaries are defined in a recursive manner into a table. Formal definition is in table-recursive.hs at present, but this hasn't compiled. The current pursuit is table as a collection of rows.

Further thought on this topic yields this as a better solution, but more complicated. This is like going from 1-dimensional to 2-dimenstional representation. The row based approach will be taken first, to work out issues before moving onto a 2-d representation of the table grammar.

### Backus-Naur Form (BNF) for formula syntax

       <table-formula>        ::= <column-specification> "~" <row-specification>
       <column-specification> ::= <formula>
       <row-specification>    ::= <formula>
       <formula>              ::= <expression> "+" <formula> | <expression>
       <expression>           ::= <data-name> "*" <expression>          | 
                                  <data-name>                           |
                                  "(" <table-formula> ")"               | 
                                  <transform-name> "(" <expression> ")" |
                                  <r-expression>

The "+" operator denotes major spilts in row / column designations.

The "*" operator denotes factorial combinations of expressions.

Data-name is the name of a column (_or_row) in the given data set.

The recursive descent into another table formula would be a possible 2-D representation mentioned in design. Not for development in Phase I. 

The r-expression is the last attempt at resolving a formula reference. The contents will be evaluated in the current R context, and the results will be used as the data column (_or row_).

### Design Choices

* Reference Class approach will be attempted first.



