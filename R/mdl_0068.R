#' @title stand (stem), basal area (breast height)
#' @return stand (stem), basal area (breast height)
#' @param p01 stand (stem), species ()
#' @param p02 species level (stem), basal area (breast height)
#' @param p03 stand (stem), basal area (breast height)
#' @param p04 stand (stem), number of trees ()
#' @param p05 stand (stem), age ()
#' @param p06 stand (stem), height (100 years old)
#' @param p07 stand (area), latitude ()
#' @param p08 stand (area), altitude ()
#' @param p09 stand (stem), thinning exists ()
#' @export
#' @rdname mdl_G_68
mdl_G_68 <- function(p01, p02, p03, p04, p05, p06, p07, p08, p09){

	xmdl.1<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = -.00592599
		c14 = -1.75160
		c13 = 1.6408349
		c12 = 0.0637421
		c11 = 0.0
		c10 = -0.0111568
		c05 = 0.00211976
		c06 = -0.821163
		c07 = -0.0094139
		c08 = 0.0
		c09 = 0.0
		c01 = -0.0123212
		c02 = 0.8648510
		c03 = -.49777E-4
		c04 = 0.2000066
		ipf1 = max(0.0635,p03-p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}

	xmdl.2<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = 0.0
		c14 = 0.0
		c13 = 1.6985793
		c12 = -0.0318464
		c11 = 0.0
		c10 = -0.0269193
		c02 = 0.7947380
		c03 = -0.0000923
		c04 = 0.279717
		c05 = 0.0
		c06 = -0.790588
		c07 = -0.0187801
		c08 = 0.0
		c09 = 0.00127135
		c01 = -0.0167127
		ipf1 = max(0.0635,p03-p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}

	xmdl.3<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = -.00134245
		c14 = 0.0
		c13 = 0.1785211
		c12 = 0.0
		c11 = 0.0
		c10 = 0.0
		c02 = 0.68815
		c03 = -0.0001581
		c04 = 0.304149
		c05 = 0.0041118
		c06 = -0.8645010
		c07 = -0.0005333
		c08 = 0.0
		c09 = 0.0
		c01 = 0.0129783
		ipf1 = max(0.0635,p03-p02)
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
		c07 = -0.146423
		c08 = 0.0
		c09 = 0.0
		c01 = 0.0
		ipf1 = max(0.0635,p03-p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}

	xmdl.19<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = 0.0
		c14 = 0.0
		c13 = -1.6590950
		c12 = 0.0
		c11 = -.00036199
		c10 = 0.0354866
		c02 = 0.794405
		c03 = -0.0000247
		c04 = 0.202344
		c05 = -.00250423
		c06 = -0.669629
		c07 = 0.0
		c08 = -0.101205
		c09 = 0.0
		c01 = 0.0
		ipf1 = max(0.0635,p03-p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}

	with(data.frame( p01, p02, p03, p04, p05, p06, p07, p08, p09 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05, p06, p07, p08, p09), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04, p05, p06, p07, p08, p09), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04, p05, p06, p07, p08, p09), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04, p05, p06, p07, p08, p09), 
		ifelse(p01 %in% c('TL','PN','PP','RE','KP','JA','VA','SA','LV','LM','HB'), xmdl.19(p02, p03, p04, p05, p06, p07, p08, p09), 
		NA
	))))))
}
