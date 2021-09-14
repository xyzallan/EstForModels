#' @title Stem G
#' @return Stem G
#' @param p01 Stem Spec
#' @param p02 Stem G
#' @param p03 Stem N
#' @param p04 Stem N
#' @param p05 Stem H
#' @param p06 Stem H
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
