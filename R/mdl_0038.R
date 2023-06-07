#' @title single tree (stem), height 
#' @return single tree (stem), height 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 species level (stem), diameter (breast height)
#' @param p04 species level (stem), height 
#' @export
#' @rdname mdl_H_38
mdl_H_38 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = 9.9241
		c01 = 1.1
		1.3+(p04-1.3) * ((p03+c01) * p02 / (p03 * (p02+c01)))^c02
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = 10.858
		c01 = 1.3
		1.3+(p04-1.3) * ((p03+c01) * p02 / (p03 * (p02+c01)))^c02
	}

	xmdl.3<-function(p02, p03, p04){
		c02 = 1.4625
		c01 = 8
		1.3+(p04-1.3) * ((p03+c01) * p02 / (p03 * (p02+c01)))^c02
	}

	xmdl.4<-function(p02, p03, p04){
		c02 = 2.4979
		c01 = 4.3
		1.3+(p04-1.3) * ((p03+c01) * p02 / (p03 * (p02+c01)))^c02
	}

	xmdl.7<-function(p02, p03, p04){
		c02 = 8.2934
		c01 = 1.6
		1.3+(p04-1.3) * ((p03+c01) * p02 / (p03 * (p02+c01)))^c02
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03, p04), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04), 
		NA
	))))))
}
