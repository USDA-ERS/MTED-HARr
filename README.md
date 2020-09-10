# MTED-HARr
A package to read and write GEMPACK-style header-array files into R. 

This package does not use any GEMPACK routines; instead, it works with the binary HAR files directly using basic R functions 

# To install you can try the following: 
```R
install.packages('devtools')
devtools::install_git('https://github.com/USDA-ERS/MTED-HARr.git')
require(HARr)
```
# To read a HAR file you can try the following: 
```R
data = read_har('basedata.har')
```

# To write a HAR file you can try the following: 
```R
data = list(
  REAL = array(
    c(1, 2, 3, 4, 5, 6),
    dim = c(2, 3),
    dimnames = list(
      dim1name = c('a', 'b'),
      dim2name = c('f', 'g', 'h')
    )
  ),
  INTG = array(c(1L, 2L, 3L, 4L, 5L, 6L),
               dim = c(2, 3)),
  STRG =
    c("STRING1", "STRING2", "STRING3")
  
)
write_har(data, 'test.har')
```
# To read an SL4 solution file you can try the following: 
```R
data = read_SL4('solutoin.sl4')
```
