#' @title stand (stem), volume 
#' @return stand (stem), volume 
#' @param p01 stand (stem), species ()
#' @param p02 stand (stem), age ()
#' @param p03 stand (stem), volume ()
#' @param p04 stand (stem), height (dominant)
#' @export
#' @rdname mdl_V_63
mdl_V_63 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = 1.05986
		c03 = 5.69
		c04 = 0.9
		c05 = 0.071
		c06 = 1.5
		c07 = -1
		c01 = 2.95091
		exp(c01+c02 * ((c03+c07 * log(p02)))^c04-c05 * (log(p03 * p04))^c06)
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = -1.99553
		c03 = 0.0
		c04 = 0.65
		c05 = 0.00216
		c06 = 3.0
		c07 = 1
		c01 = 9.25266
		exp(c01+c02 * ((c03+c07 * log(p02)))^c04-c05 * (log(p03 * p04))^c06)
	}

	xmdl.3<-function(p02, p03, p04){
		c02 = -2.50830
		c03 = 0.0
		c04 = 0.6
		c05 = 0.07567
		c06 = 1.5
		c07 = 1
		c01 = 10.65520
		exp(c01+c02 * ((c03+c07 * log(p02)))^c04-c05 * (log(p03 * p04))^c06)
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04), 
		NA
	))))
}
