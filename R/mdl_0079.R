#' @title stand (stem), height 
#' @return stand (stem), height 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), age (breast height)
#' @param p03 stand (stem), height (100 years old)
#' @export
#' @rdname mdl_H_79
mdl_H_79 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 1
		c03 = 1.0002E-4
		c04 = 9.5953e-6
		c05 = 1.3755
		c06 = 1
		c07 = 0.066074
		c08 = 4.4189E5
		c09 = -2.9134
		c01 = 1.0075
		ipf1 = c01*p03^c02
		ipf2 = c03+c04*p03^c05
		ipf3 = c07+c08*p03^c09
		13+ipf1*(1-exp(-ipf2*p02))^(c06/(1-ipf3))
	}

	xmdl.2<-function(p02, p03){
		c02 = 0.99808
		c03 = 0.042624
		c04 = -7.1145
		c05 = -1.0068
		c06 = 0.98822
		c07 = 0.15933
		c08 = 3.7E6
		c09 = -3.156
		c01 = 1.0017
		ipf1 = c01*p03^c02
		ipf2 = c03+c04*p03^c05
		ipf3 = c07+c08*p03^c09
		13+ipf1*(1-exp(-ipf2*p02))^(c06/(1-ipf3))
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	)))
}
