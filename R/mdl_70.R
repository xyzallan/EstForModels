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
mdl_G_70 <- function(p01, p02, p03, p04, p05, p06, p07, p08, p09){
	xmdl.1<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = -.00592599
		c14 = -1.75160
		c13 = 1.3244849
		c12 = 0.0637421
		c11 = 0.0
		c10 = -0.0111567
		c02 = 0.809028
		c03 = -0.0002074
		c04 = 0.199179
		c05 = 0.0002596
		c06 = -0.663161
		c07 = -0.0142082
		c08 = 0.0
		c09 = 0.0
		c01 = -0.0073851
		ipf1 = max(0.0635,p03-p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}
	xmdl.2<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = 0.0
		c14 = 0.0
		c13 = 1.7376393
		c12 = -0.0318464
		c11 = 0.0
		c10 = -0.0269193
		c07 = -0.013584
		c08 = 0.0
		c09 = 0.00127135
		c02 = 0.90273
		c03 = -0.0000706
		c04 = 0.198283
		c05 = 0.0
		c01 = -0.0243263
		c06 = -0.71323
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
		c03 = -0.000179
		c04 = 0.316913
		c05 = 0.0026212
		c07 = -0.0146037
		c08 = 0.0
		c09 = 0.0
		c01 = -0.0204315
		c02 = 0.792798
		c06 = -0.791796
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
		c02 = 0.851362
		c03 = 0.0
		c04 = 0.1281
		c05 = 0.0
		c06 = -0.667346
		c07 = -0.0199705
		c08 = 0.0
		c09 = 0.0
		c01 = 0.0
		ipf1 = max(0.0635,p03-p02)
		exp(p02 * c01+log(p02) * c02+p04 * c03+log(p04) * c04+p05 * c05+log(p05) * c06+ipf1 * c07+log(ipf1) * c08+p06 * c09+p07 * c10+p08 * c11+p09 * c12+sqrt(p02 / p04 * 4 / 3.1415926) * c14+c13)
	}
	xmdl.19<-function(p02, p03, p04, p05, p06, p07, p08, p09){
		c15 = 0.0
		c14 = 0.0
		c13 = -1.6235950
		c12 = 0.0
		c11 = -.00036199
		c10 = 0.0354866
		c02 = 0.771398
		c03 = .427071E-4
		c04 = 0.167037
		c05 = -.00190695
		c06 = -0.587696
		c07 = 0.0
		c08 = -0.113489
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
