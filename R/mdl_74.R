#' @title Stem G
#' @return Stem G
#' @param p01 Stem Spec
#' @param p02 Stem G
#' @param p03 Stem G
#' @param p04 Stem N
#' @param p05 Stem A
#' @param p06 Stem H
#' @param p07 Area Lat
#' @param p08 Area Alt
#' @param p09 Stem Thin
mdl_G_74 <- function(p01, p02, p03, p04, p05, p06, p07, p08, p09){
	xmdl.1<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = -.00592599
		c14 = -1.75160
		c13 = 1.616805
		c12 = .0637421
		c11 = 0.0
		c10 = -.0111568
		c02 = 1.00931
		c03 = -.65376E-5
		c04 = .0851371
		c05 = -.00307386
		c06 = -0.635182
		c07 = -0.011097
		c08 = 0.0
		c09 = 0.0
		c01 = -0.0207497
		ipf1 = max(0.0635, p03 -p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}
	xmdl.2<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = 0.0
		c14 = 0.0
		c13 = 1.6513594
		c12 = -.0318464
		c11 = 0.0
		c10 = -.0269193
		c02 = 0.8552
		c03 = -.00017655
		c04 = 0.269091
		c05 = 0.0
		c06 = -0.765104
		c07 = -.0180257
		c08 = 0.0
		c09 = .00127135
		c01 = -.0218319
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
		c02 = .730671
		c03 = .000025637
		c04 = .256131
		c05 = .0126785
		c06 = -1.24005
		c07 = -.00341768
		c08 = 0.0
		c09 = 0.0
		c01 = .00255898
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
		c02 = .851362
		c03 = 0.0
		c04 = .128100
		c05 = 0.0
		c06 = -.667346
		c07 = -.0199705
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
		c12 = .125048
		c11 = -.00036199
		c10 = .0354866
		c02 = .771398
		c03 = .427071E-4
		c04 = .167037
		c05 = -.00190695
		c06 = -.587696
		c07 = 0.0
		c08 = -.113489
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
