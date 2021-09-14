#' @title stand (stem), distance between 
#' @return stand (stem), distance between 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @param p03 stand (stem), diameter (breast height)
mdl_L_108 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 0.489
		c03 = -1.855
		c01 = 11.213
		10000 / sqrt(exp(c01+log(p02) * c02+log(p03) * c03))
	}

	xmdl.2<-function(p02, p03){
		c02 = 0.446
		c03 = -1.354
		c01 = 10.059
		10000 / sqrt(exp(c01+log(p02) * c02+log(p03) * c03))
	}

	xmdl.3<-function(p02, p03){
		c02 = 0.085
		c03 = -2.332
		c01 = 13.645
		10000 / sqrt(exp(c01+log(p02) * c02+log(p03) * c03))
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
	))))
}
