#' @title species level (stem), height ()
#' @return species level (stem), height ()
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), age 
#' @param p03 species level (stem), height 
#' @param p04 species level (stem), age (random point)
#' @export
#' @rdname mdl_H_7
mdl_H_7 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = -487.90
		c01 = 25069
		ipf1 = 2*(1+c01/p02^2)
		ipf2 = ipf1*p04
		ipf3 = sqrt(c02^2 / p02^4+2 * ipf1 / p03^2)-c02 / p02^2
		ipf2 / (ipf3 * sqrt(c02 * ipf1 / ipf3+p04^2+c01))
	}

	xmdl.3<-function(p02, p03, p04){
		c02 = -131.34
		c01 = 7074
		ipf1 = 2*(1+c01/p02^2)
		ipf2 = ipf1*p04
		ipf3 = sqrt(c02^2 / p02^4+2 * ipf1 / p03^2)-c02 / p02^2
		ipf2 / (ipf3 * sqrt(c02 * ipf1 / ipf3+p04^2+c01))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('TO','NU','SD','TS','LH','KP','JA','VA','SA','TA','KU','MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('TL','PN','PP','RE','LV','LM','HB','KS'), xmdl.3(p02, p03, p04), 
		NA
	)))
}
