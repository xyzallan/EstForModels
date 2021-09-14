#' @title Stem Bon
#' @return Stem Bon
#' @param p01 Stem Spec
#' @param p02 Stem H
#' @param p03 Stem A
mdl_Bon_89 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 = -20.468
		c03 = 9.201
		c04 = -0.8317
		c05 = -0.571
		c06 = 2.114
		c07 = -1.17
		c08 = 0.1219
		c01 = 11.81
		(p02-(c01+c02 * log(p03)+c03 * log(p03)^2+c04 * log(p03)^3)) / (c05+c06 * log(p03)+c07 * log(p03)^2+c08 * log(p03)^3)
	}
	xmdl.2<-function(p02, p03){
		c02 = -67.663
		c03 = 20.228
		c04 = -1.6059
		c05 = -9.108
		c06 = 8.82
		c07 = -2.761
		c08 = 0.2392
		c01 = 70.29
		(p02-(c01+c02 * log(p03)+c03 * log(p03)^2+c04 * log(p03)^3)) / (c05+c06 * log(p03)+c07 * log(p03)^2+c08 * log(p03)^3)
	}
	xmdl.3<-function(p02, p03){
		c02 = -5.019
		c03 = 2.555
		c04 = 0.0
		c05 = -1.709
		c06 = 1.202
		c07 = -0.368
		c08 = 0.0
		c01 = 4.74
		(p02-(c01+c02 * log(p03)+c03 * log(p03)^2+c04 * log(p03)^3)) / (c05+c06 * log(p03)+c07 * log(p03)^2+c08 * log(p03)^3)
	}
	xmdl.4<-function(p02, p03){
		c05 = -2.249
		c06 = 1.493
		c07 = -0.416
		c08 = 0.0
		c02 = -6.392
		c03 = 2.786
		c01 = 6.76
		c04 = 0.0
		(p02-(c01+c02 * log(p03)+c03 * log(p03)^2+c04 * log(p03)^3)) / (c05+c06 * log(p03)+c07 * log(p03)^2+c08 * log(p03)^3)
	}
	xmdl.5<-function(p02, p03){
		c02 = -14.277
		c04 = -0.9235
		c05 = -0.355
		c06 = -0.212
		c07 = -0.207
		c08 = 0.0269
		c01 = 8.77
		c03 = 8.355
		(p02-(c01+c02 * log(p03)+c03 * log(p03)^2+c04 * log(p03)^3)) / (c05+c06 * log(p03)+c07 * log(p03)^2+c08 * log(p03)^3)
	}
	xmdl.6<-function(p02, p03){
		c02 = -18.290
		c03 = 10.125
		c04 = -1.2395
		c05 = 0.370
		c06 = -0.799
		c07 = -0.074
		c08 = 0.0294
		c01 = 14.48
		(p02-(c01+c02 * log(p03)+c03 * log(p03)^2+c04 * log(p03)^3)) / (c05+c06 * log(p03)+c07 * log(p03)^2+c08 * log(p03)^3)
	}
	xmdl.7<-function(p02, p03){
		c02 = -52.979
		c03 = 18.293
		c04 = -1.6336
		c05 = -14.194
		c06 = 14.742
		c07 = -5.066
		c08 = 0.5065
		c01 = 50.41
		(p02-(c01+c02 * log(p03)+c03 * log(p03)^2+c04 * log(p03)^3)) / (c05+c06 * log(p03)+c07 * log(p03)^2+c08 * log(p03)^3)
	}
	xmdl.8<-function(p02, p03){
		c02 = -28.644
		c03 = 13.44
		c04 = -1.3654
		c05 = 19.657
		c06 = -14.477
		c07 = 2.822
		c08 = -0.1861
		c01 = 15.62
		(p02-(c01+c02 * log(p03)+c03 * log(p03)^2+c04 * log(p03)^3)) / (c05+c06 * log(p03)+c07 * log(p03)^2+c08 * log(p03)^3)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02, p03), 
		NA
		)))))))))
}
