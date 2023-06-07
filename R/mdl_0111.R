#' @title stand (stem), age (mature)
#' @return stand (stem), age (mature)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), age 
#' @param p03 stand (stem), number of trees 
#' @param p04 stand (stem), diameter (breast height)
#' @param p05 stand (stem), height (100 years old)
#' @export
#' @rdname mdl_A_111
mdl_A_111 <- function(p01, p02, p03, p04, p05){

	xmdl.1<-function(p02, p03, p04, p05){
		c02 = 0.142
		c03 = -0.034
		c01 = 1.436
		p02 / sqrt(p03 * (c01+c02 * p04+c03 * p05)^2 / 10000)
	}

	xmdl.2<-function(p02, p03, p04, p05){
		c02 = 0.129
		c03 = -0.012
		c01 = 0.918
		p02 / sqrt(p03 * (c01+c02 * p04+c03 * p05)^2 / 10000)
	}

	xmdl.3<-function(p02, p03, p04, p05){
		c02 = 0.147
		c03 = 0.0
		c01 = 0.714
		p02 / sqrt(p03 * (c01+c02 * p04+c03 * p05)^2 / 10000)
	}

	with(data.frame( p01, p02, p03, p04, p05 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04, p05), 
		ifelse(p01 %in% c('TA','LV','LM','HB','KS'), xmdl.3(p02, p03, p04, p05), 
		NA
	))))
}
