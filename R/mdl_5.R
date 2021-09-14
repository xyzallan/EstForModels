#' @title Stem D
#' @return Stem D
#' @param p01 Stem Spec
#' @param p02 Stem D
mdl_D_5 <- function(p01, p02){
	xmdl.1<-function(p02){
		c02 = 0.87
		c01 = -1.89
		c01+c02 * p02
	}
	xmdl.2<-function(p02){
		c02 = 0.79
		c01 =  0.00
		c01+c02 * p02
	}
	xmdl.3<-function(p02){
		c02 = 0.81
		c01 = -1.28
		c01+c02 * p02
	}
	xmdl.4<-function(p02){
		c02 = 0.88
		c01 = -0.33
		c01+c02 * p02
	}
	xmdl.5<-function(p02){
		c02 = 0.84
		c01 = -0.86
		c01+c02 * p02
	}
	xmdl.6<-function(p02){
		c02 = 0.90
		c01 = -1.27
		c01+c02 * p02
	}
	xmdl.8<-function(p02){
		c02 = 0.85
		c01 = -1.94
		c01+c02 * p02
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('SD','LH','MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('TO','NU','TS','KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('PN','PP','HB'), xmdl.4(p02), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02), 
		ifelse(p01 %in% c('TL','RE','LV'), xmdl.6(p02), 
		ifelse(p01 %in% c('KP','JA','VA','SA','TA'), xmdl.8(p02), 
		NA
		))))))))
}
