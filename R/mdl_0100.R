#' @title stand (stem), formheight 
#' @return stand (stem), formheight 
#' @param p01 stand (stem), species 
#' @param p02 single tree (stem), formheight 
#' @param p03 single tree (stem), height 
#' @param p04 single tree (stem), diameter (breast height)
#' @export
#' @rdname mdl_HF_100
mdl_HF_100 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c03 = 1.074
		c04 = -0.0081
		c05 = 0.0167
		c06 = 0.3
		c01 = 1.109
		c02 = 0.3
		c01 * (p02 / p03-c02)^c03 * (p04)^c04 * (p03)^c05+c06
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = 0.0
		c03 = 1.095
		c04 = 0.2115
		c05 = -0.03379
		c06 = 0.0
		c01 = 1.106
		c01 * (p02 / p03-c02)^c03 * (p04)^c04 * (p03)^c05+c06
	}

	xmdl.3<-function(p02, p03, p04){
		c02 = 0.3
		c03 = 1.032
		c04 = -0.02116
		c05 = -0.02034
		c06 = 0.3
		c01 = 1.194
		c01 * (p02 / p03-c02)^c03 * (p04)^c04 * (p03)^c05+c06
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04), 
		NA
	))))
}
