x<-c(TRUE,TRUE,FALSE,FALSE)
y<-c(TRUE,FALSE,TRUE,FALSE)
expect_that(not(and(x,y)),nand(x,y))
