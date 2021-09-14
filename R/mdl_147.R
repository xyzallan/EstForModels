#' @title Stem V
#' @return Stem V
#' @param p01 Stem Spec
#' @param p02 Stem G
#' @param p03 Stem A
#' @param p04 Stem Bon
mdl_V_147 <- function(p01, p02, p03, p04){
	xmdl.1<-function(p02, p03, p04){
		c10 = 0.00183810
		c02 = -1.130
		c03 = 0.3105
		c04 = 0.280
		c05 = -4.735
		c06 = 0.4656
		c07 = 0.78780049
		c08 = 0.00216531
		c09 = -0.18329824
		c01 = 9.7180
		exp(log(c01+c02 * p04)+c03 * exp(c04 * p04) * log(p03 / 10)+(p03 / 10) / (c05+c06 * p04)+((c10 * p03+c09) * log(p02 / 10)+c08 * p03+c07) * log(p02 / 10))
	}
	xmdl.2<-function(p02, p03, p04){
		c10 = 0.00042843
		c02 = -2.4186
		c03 = 0.1574
		c04 = 0.574
		c05 = -6.6088
		c06 = 1.0324
		c07 = 0.67395179
		c08 = 0.00218171
		c09 = -0.08601535
		c01 = 12.412
		exp(log(c01+c02 * p04)+c03 * exp(c04 * p04) * log(p03 / 10)+(p03 / 10) / (c05+c06 * p04)+((c10 * p03+c09) * log(p02 / 10)+c08 * p03+c07) * log(p02 / 10))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		NA
		)))
}
