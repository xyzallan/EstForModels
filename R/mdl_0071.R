#' @title stand (stem), basal area (breast height)
#' @return stand (stem), basal area (breast height)
#' @param p01 stand (stem), species 
#' @param p02 species level (stem), basal area (breast height)
#' @param p03 stand (stem), basal area (breast height)
#' @param p04 stand (stem), number of trees 
#' @param p05 stand (stem), age 
#' @param p06 stand (stem), height (100 years old)
#' @param p07 stand (area), latitude 
#' @param p08 stand (area), altitude 
#' @param p09 stand (stem), thinning exists 
#' @export
#' @rdname mdl_G_71
mdl_G_71 <- function(p01, p02, p03, p04, p05, p06, p07, p08, p09){

	xmdl.1<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = -.00592599
		c14 = -1.75160
		c13 = 2.7732949
		c12 = 0.0637421
		c11 = 0.0
		c10 = -0.0111567
		c02 = 0.938947
		c03 = 0.0005632
		c04 = 0.148914
		c05 = 0.0041959
		c06 = -1.15586
		c07 = 0.0138465
		c08 = 0.0
		c09 = 0.0
		c01 = -0.0302305
		ipf1 = max(0.0635, p03 -p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}

	xmdl.2<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = 0.0
		c14 = 0.0
		c13 = 1.95845937
		c12 = -.03184642
		c11 = 0.0
		c10 = -0.0269193
		c02 = 0.838105
		c03 = 0.00051981
		c04 = 0.141232
		c05 = 0.0
		c06 = -0.722723
		c07 = -0.0237689
		c08 = 0.0
		c09 = 0.00127135
		c01 = -0.0227763
		ipf1 = max(0.0635, p03 -p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}

	xmdl.3<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = -.00134245
		c14 = 0.0
		c13 = 1.50569210
		c12 = 0.0
		c11 = 0.0
		c10 = 0.0
		c02 = 0.332109
		c03 = -.00045799
		c04 = 0.474159
		c05 = 0.0092238
		c06 = -1.50315
		c07 = -0.0116043
		c08 = 0.0
		c09 = 0.0
		c01 = 0.144427
		ipf1 = max(0.0635, p03 -p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}

	xmdl.7<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = 0.0
		c14 = 0.0
		c13 = 0.7714665
		c12 = 0.0
		c11 = 0.0
		c10 = 0.0
		c02 = 0.89659857
		c03 = 0.0
		c04 = 0.19935433
		c05 = 0.0
		c06 = -0.8426653
		c07 = -0.0146423
		c08 = 0.0
		c09 = 0.0
		c01 = 0.0
		ipf1 = max(0.0635, p03 -p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}

	xmdl.19<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = 0.0
		c14 = 0.0
		c13 = -1.6235950
		c12 = 0.125048
		c11 = -0.0003619
		c10 = 0.03548655
		c02 = 0.8571526
		c03 = -0.0000542
		c04 = 0.1526839
		c05 = -0.0080309
		c06 = -0.5702304
		c07 = 0.0
		c08 = -0.1005175
		c09 = 0.0
		c01 = 0.0
		ipf1 = max(0.0635, p03 -p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}

	with(data.frame( p01, p02, p03, p04, p05, p06, p07, p08, p09 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05, p06, p07, p08, p09), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04, p05, p06, p07, p08, p09), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04, p05, p06, p07, p08, p09), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04, p05, p06, p07, p08, p09), 
		ifelse(p01 %in% c('PN','PP','RE','KP','JA','VA','SA','LV','LM','HB','TL'), xmdl.19(p02, p03, p04, p05, p06, p07, p08, p09), 
		NA
	))))))
}
