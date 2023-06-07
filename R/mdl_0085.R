#' @title stand (stem), height (100 years old)
#' @return stand (stem), height (100 years old)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), age (breast height)
#' @param p03 stand (stem), height (dominant)
#' @export
#' @rdname mdl_H_85
mdl_H_85 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 1.3
		c03 = 1.17
		c04 = 0.83
		c01 = 2.6
		(c01-c02 * log10(p02)-c03 * p03) / (c04-log10(p02))
	}

	xmdl.2<-function(p02, p03){
		c02 = 1.3
		c03 = 1.14
		c04 = 0.86
		c01 = 2.6
		(c01-c02 * log10(p02)-c03 * p03) / (c04-log10(p02))
	}

	xmdl.3<-function(p02, p03){
		c02 = 1.3
		c03 = 0.899
		c04 = 0.8
		c01 = 2.209
		(c01-c02 * log10(p02)-c03 * p03) / (c04-log10(p02))
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
	))))
}
