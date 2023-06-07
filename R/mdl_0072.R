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
#' @rdname mdl_G_72
mdl_G_72 <- function(p01, p02, p03, p04, p05, p06, p07, p08, p09){

	xmdl.1<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = -.00592599
		c14 = -1.75160
		c13 = 3.445105
		c12 = 0.0637421
		c11 = 0.0
		c10 = -0.0111568
		c03 = -.00016903
		c04 = 0.0621225
		c05 = 0.00305833
		c06 = -1.18279
		c07 = -.00043906
		c08 = 0.0
		c09 = 0.0
		c01 = -.00216126
		c02 = 0.938131
		ipf1 = max(0.0635, p03 -p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}

	xmdl.2<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = 0.0
		c14 = 0.0
		c13 = 2.3685294
		c12 = -0.0318464
		c11 = 0.0
		c10 = -0.0269193
		c02 = 0.835811
		c03 = -.99543E-4
		c04 = 0.258612
		c05 = 0.0
		c06 = -0.931549
		c07 = -0.0246752
		c08 = 0.0
		c09 = 0.00127135
		c01 = -0.0167448
		ipf1 = max(0.0635, p03 -p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}

	xmdl.3<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = -.00134245
		c14 = 0.0
		c13 = 1.505692
		c12 = 0.0
		c11 = 0.0
		c10 = 0.0
		c02 = 0.962877
		c03 = .103737E-3
		c04 = 0.18679
		c05 = -.00127109
		c06 = -1.02854
		c07 = -.00849201
		c08 = 0.0
		c09 = 0.0
		c01 = -.0235447
		ipf1 = max(0.0635, p03 -p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}

	xmdl.7<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = 0.0
		c14 = 0.0
		c13 = -0.447867
		c12 = 0.0
		c11 = 0.0
		c10 = 0.0
		c02 = 0.896599
		c03 = 0.0
		c04 = 0.199354
		c05 = 0.0
		c06 = -0.842665
		c07 = -.0146432
		c08 = 0.0
		c09 = 0.0
		c01 = 0.0
		ipf1 = max(0.0635, p03 -p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}

	xmdl.19<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = 0.0
		c14 = 0.0
		c13 = -1.704245
		c12 = 0.125048
		c11 = -.00036199
		c10 = 0.0354866
		c03 =  -0.000027
		c05 = -0.0025043
		c06 = -0.669629
		c07 = 0.0
		c08 = -0.101205
		c09 = 0.0
		c01 = 0.0
		c02 = 0.794405
		c04 = 0.202344
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
