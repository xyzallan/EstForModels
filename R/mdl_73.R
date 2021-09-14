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
mdl_G_73 <- function(p01, p02, p03, p04, p05, p06, p07, p08, p09){
	xmdl.1<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = -.00592599
		c14 = -1.75160
		c13 = 2.540705
		c12 = 0.0637421
		c11 = 0.0
		c10 = -.0111568
		c09 = 0.0
		c08 = 0.0
		c07 = -0.0133437
		c06 = -0.945376
		c05 = 0.00163026
		c04 = 0.173044
		c03 = -.00017355
		c02 = 0.76771
		c01 = -0.0063294
		ipf1 = max(0.0635, p03 -p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}
	xmdl.2<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = 0.0
		c14 = 0.0
		c13 = 1.5334694
		c12 = -0.0318464
		c11 = 0.0
		c10 = -0.0269193
		c09 = 0.00127135
		c08 = 0.0
		c07 = -0.0151436
		c06 = -0.716504
		c05 = 0.0
		c04 = 0.248563
		c03 = -.75516E-4
		c02 = 0.83689
		c01 = -0.020397
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
		c09 = 0.0
		c08 = 0.0
		c07 = -.0197822
		c06 = -1.16385
		c05 = 0.01067
		c04 = 0.241055
		c03 = -.00030415
		c02 = 0.818427
		c01 = -.00043879
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
		c09 = 0.0
		c08 = 0.0
		c07 = -0.022299
		c06 = -0.727278
		c05 = 0.0
		c04 = 0.144495
		c03 = 0.0
		c02 = 0.84742
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
		c09 = 0.0
		c08 = -.0823802
		c07 = 0.0
		c06 = -.733575
		c05 = -.00078715
		c04 = 0.239626
		c03 = -.00012511
		c02 = 0.782374
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
