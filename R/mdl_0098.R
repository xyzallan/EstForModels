#' @title stand (stem), formheight 
#' @return stand (stem), formheight 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @param p03 stand (stem), basal area (breast height)
#' @export
#' @rdname mdl_HF_98
mdl_HF_98 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 0.05595
		c03 = 3.18
		c04 = 1.75
		c05 = 0.0
		c01 = 0.48183
		(c01+c02 * ((c03-log(p02))^c04) * (log(p03)^c05)) * p02
	}

	xmdl.2<-function(p02, p03){
		c02 = 0.04719
		c03 = 3.10
		c04 = 2.3
		c05 = 0.0
		c01 = 0.48116
		(c01+c02 * ((c03-log(p02))^c04) * (log(p03)^c05)) * p02
	}

	xmdl.3<-function(p02, p03){
		c02 = -0.05748
		c03 = 1
		c04 = 0.0
		c05 = 0.8
		c01 = 0.60686
		(c01+c02 * ((c03-log(p02))^c04) * (log(p03)^c05)) * p02
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
	))))
}
