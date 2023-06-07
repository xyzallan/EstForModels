#' @title single tree (stem), height 
#' @return single tree (stem), height 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 species level (stem), diameter (breast height)
#' @param p04 species level (stem), height 
#' @export
#' @rdname mdl_H_143
mdl_H_143 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = 0.142404
		c03 = -0.0015008
		c04 = 1.30841
		c05 = -0.079939
		c06 =  0.0009236
		c01 = -3.29487
		ipf1 = p02/p03
		p04 * (1-((c01+(c02+c03 * p02) * p02)+(c04+(c05+c06 * p02) * p02) * (1 / 1.5+1 / (ipf1+0.5))) * (1 / 1.5-1 / (ipf1+0.5)))
	}

	xmdl.2<-function(p02, p03, p04){
		c02 =  0.259258
		c03 = -0.0035748
		c04 =  2.03747
		c05 = -0.142911
		c06 =  0.0020762
		c01 = -4.89742
		ipf1 = p02/p03
		p04 * (1-((c01+(c02+c03 * p02) * p02)+(c04+(c05+c06 * p02) * p02) * (1 / 1.5+1 / (ipf1+0.5))) * (1 / 1.5-1 / (ipf1+0.5)))
	}

	xmdl.3<-function(p02, p03, p04){
		c02 = 0.159893
		c03 = -0.0021644
		c04 =  1.01665
		c05 = -0.083867
		c06 =  0.0012905
		c01 = -3.02931
		ipf1 = p02/p03
		p04 * (1-((c01+(c02+c03 * p02) * p02)+(c04+(c05+c06 * p02) * p02) * (1 / 1.5+1 / (ipf1+0.5))) * (1 / 1.5-1 / (ipf1+0.5)))
	}

	xmdl.4<-function(p02, p03, p04){
		c02 = 0.055625
		c03 = -0.0002023
		c04 = 0.37793
		c05 = -0.023913
		c06 = 0.0000509
		c01 = -1.71826
		ipf1 = p02/p03
		p04 * (1-((c01+(c02+c03 * p02) * p02)+(c04+(c05+c06 * p02) * p02) * (1 / 1.5+1 / (ipf1+0.5))) * (1 / 1.5-1 / (ipf1+0.5)))
	}

	xmdl.5<-function(p02, p03, p04){
		c02 = 0.021870
		c03 =  0.0005271
		c04 =  0.12847
		c05 = -0.015836
		c06 = -0.0001081
		c01 = -1.28120
		ipf1 = p02/p03
		p04 * (1-((c01+(c02+c03 * p02) * p02)+(c04+(c05+c06 * p02) * p02) * (1 / 1.5+1 / (ipf1+0.5))) * (1 / 1.5-1 / (ipf1+0.5)))
	}

	xmdl.6<-function(p02, p03, p04){
		c03 =  0.0010588
		c04 =  0.58451
		c05 = -0.026614
		c06 = -0.0007057
		c01 = -2.11866
		c02 =  0.066088
		ipf1 = p02/p03
		p04 * (1-((c01+(c02+c03 * p02) * p02)+(c04+(c05+c06 * p02) * p02) * (1 / 1.5+1 / (ipf1+0.5))) * (1 / 1.5-1 / (ipf1+0.5)))
	}

	xmdl.7<-function(p02, p03, p04){
		c02 =  0.084581
		c03 = -0.0009408
		c04 =  0.32366
		c05 = -0.031105
		c06 =  0.0003111
		c01 = -1.80468
		ipf1 = p02/p03
		p04 * (1-((c01+(c02+c03 * p02) * p02)+(c04+(c05+c06 * p02) * p02) * (1 / 1.5+1 / (ipf1+0.5))) * (1 / 1.5-1 / (ipf1+0.5)))
	}

	xmdl.8<-function(p02, p03, p04){
		c02 = -0.009868
		c03 = 0.0007665
		c04 = -0.10432
		c05 = 0.013194
		c06 = -0.0005216
		c01 = -0.83195
		ipf1 = p02/p03
		p04 * (1-((c01+(c02+c03 * p02) * p02)+(c04+(c05+c06 * p02) * p02) * (1 / 1.5+1 / (ipf1+0.5))) * (1 / 1.5-1 / (ipf1+0.5)))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03, p04), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03, p04), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03, p04), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02, p03, p04), 
		NA
	)))))))))
}
