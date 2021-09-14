#' @title stand (stem), basal area (breast height)
#' @return stand (stem), basal area (breast height)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), basal area (breast height)
#' @param p03 stand (stem), number of trees 
#' @param p04 stand (stem), number of trees 
#' @param p05 stand (stem), height 
#' @param p06 stand (stem), height 
mdl_G_178 <- function(p01, p02, p03, p04, p05, p06){

	xmdl.2<-function(p02, p03, p04, p05, p06){
		c03 = 4.292
		c01 = 0.142
		c02 = 0.601
		p02 * p04^(1-c01 * p06^c02) * p03^(c01 * p05^c02-1) * (p06 / p05)^c03
	}

	with(data.frame( p01, p02, p03, p04, p05, p06 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04, p05, p06), 
		NA
	))
}
