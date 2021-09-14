#' @title Stem V
#' @return Stem V
#' @param p01 Stem Spec
#' @param p02 Stem V
#' @param p03 Stem A
#' @param p04 Stem A
mdl_V_184 <- function(p01, p02, p03, p04){
	xmdl.15<-function(p02, p03, p04){
		c02 = 2.38
		c01 = 157000
		ipf1 = c01*10^(-c02)
		ipf2 = sqrt((p02-ipf1)^2+4*c01*p02*p03^(-c02))
		(p02+ipf1+ipf2) / (2+4 * c01 * (p04^(-c02)) / (p02-ipf1+ipf2))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('LH'), xmdl.15(p02, p03, p04), 
		NA
		))
}
