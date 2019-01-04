source("http://www.bioconductor.org/biocLite.R")
biocLite("BiocUpgrade")
if(!require(BiocManager)) {
  install.packages("BiocManager"); require(BiocManager)
}
BiocManager::install(c("ALL" ,"GenomicRanges"))
## packages
library(ALL)
library(GenomicRanges)

## A look at S3 object
### An output of a linear regression model function `lm` in base `R`
df <- data.frame(y = rnorm(10), x = rnorm(10))
lm.object <- lm(y ~ x, data = df)
lm.object
names(lm.object)
class(lm.object)
### In standard R, an S3 object ia a `list` with a `class` attribute on it. The problem with S3 objects is that we can assign any class to any list.
xx <- list(a = letters[4:6], b = rnorm(3))
xx
class(xx) # a list
### Now we assign xx a new class
class(xx) <- "lm"
xx # calling xx does not return an error but prints `NULL`

### S4 objects have a formal def & validity checking, which guarantees the validity of the object
# Loading S4 object
library(ALL)
data(ALL)
ALL; class(ALL); isS4(ALL) # checks if object is S4

## S4 Methods
### Think of S4 methods as simple functions. A method is a function that looks at its arguments and decide what to do. One way to mimic a nethod is by a function definition as below:
mimicMethod <- function(x) {
  if (is(x, "matrix"))
    method1(x)
  if (is(x, "data.frame"))
    method2(x)
  if (is(x, "IRanges"))
    method3(x)
}
# An example is `as.data.frame`
as.data.frame
showMethods("as.data.frame")
getMethod("as.data.frame", "DataFrame")
# Method Definition:
#   
#   function (x, row.names = NULL, optional = FALSE, ...) 
#   {
#     .local <- function (x, row.names = NULL, optional = FALSE, 
#                         stringsAsFactors = FALSE, ...) 
#     {
#       stopifnot(identical(stringsAsFactors, FALSE))
#       if (length(list(...))) 
#         warning("Arguments in '...' ignored")
#       if (is.null(row.names)) {
#         row.names <- rownames(x)
#         if (!is.null(row.names)) 
#           row.names <- make.unique(row.names)
#         else if (ncol(x) == 0L) 
#           row.names <- seq_len(nrow(x))
#       }
#       old_option <- getOption("stringsAsFactors")
#       options(stringsAsFactors = FALSE)
#       on.exit(options(stringsAsFactors = old_option))
#       x_colnames <- colnames(x)
#       df_list <- lapply(setNames(seq_along(x), x_colnames), 
#                         function(j) {
#                           col <- x[[j]]
#                           if (is.data.frame(col)) 
#                             return(col)
#                           if (is(col, "DataFrame")) 
#                             return(as.data.frame(col, optional = optional))
#                           if (is(col, "List") && pcompareRecursively(col)) 
#                             col <- as.list(col)
#                           if (is.list(col)) 
#                             col <- I(col)
#                           df <- as.data.frame(col, optional = optional)
#                           if (is.null(colnames(col)) && ncol(df) == 1L) 
#                             colnames(df) <- x_colnames[[j]]
#                           df
#                         })
#       do.call(data.frame, c(df_list, list(row.names = row.names, 
#                                           check.names = !optional, stringsAsFactors = FALSE)))
#     }
#     .local(x, row.names, optional, ...)
#   }
