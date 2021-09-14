detach("package:RPostgreSQL", unload = TRUE)
rm(list=ls())
require(RPostgreSQL)
require(stringi)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="formis", user="formis", host="formis.emu.ee")

getCodif <- function(tbl, clmn)
{
  tmp <- dbGetQuery(con, paste("select * from ", tbl))
  tul = list()
  for(i in 1:nrow(tmp))
  {
    tul[[tmp[i, "kood"]]] <- tolower(tmp[i, clmn])
  }
  tul
}


cod.level <- getCodif("dma_levels","en")
cod.obj <- getCodif("dma_objects","en")
cod.var <- getCodif("dma_variables","en")
cod.point <- getCodif("dma_points","en")

dbMain <- dbGetQuery(con, "select * from dm_formmain where status = 'K' order by id")
dbArgs <- dbGetQuery(con, "select * from dm_argsinfo")
dbSpec <- dbGetQuery(con, "select * from species")
dbConst <- dbGetQuery(con, "select * from dm_constans")
dbCase <- dbGetQuery(con, "select * from dm_casetbl")
pars <- split(dbArgs, dbArgs$id_form)
cons <- split(dbConst, dbConst$id_form)
cases <- split(dbCase, dbCase$id_form)



i = 171
#sink()
for(i in 1:nrow(dbMain))
{
  mdlID <- as.character(dbMain[i,"id"])
  fileName = paste0(getwd(), "/R/mdl_", mdlID, ".R")
  sink(fileName)
  if(mdlID %in% names(pars))
  {
    xpars <- pars[[mdlID]]
    xpars <- xpars[order(xpars$arg),]
    cat("#' @title ", cod.level[[xpars[1, "level"]]], " (", cod.obj[[xpars[1, "obj"]]], "), ", cod.var[[xpars[1, "id_var"]]], " ", ifelse(!is.na(xpars[1, "point"]), paste0("(",cod.point[[xpars[1, "point"]]],")"), ""), "\n", sep="")
    cat("#' @return ", cod.level[[xpars[1, "level"]]], " (", cod.obj[[xpars[1, "obj"]]], "), ", cod.var[[xpars[1, "id_var"]]], " ", ifelse(!is.na(xpars[1, "point"]), paste0("(",cod.point[[xpars[1, "point"]]],")"), ""), "\n", sep="")
    for(j in 2:nrow(xpars))
    {
      cat("#' @param ", tolower(xpars[j, "arg"]), " ", cod.level[[xpars[j, "level"]]], " (", cod.obj[[xpars[j, "obj"]]], "), ", cod.var[[xpars[j, "id_var"]]], " ", ifelse(!is.na(xpars[j, "point"]), paste0("(",cod.point[[xpars[j, "point"]]],")"), ""), "\n", sep="")
    }
  }
  cat("mdl_", xpars[1, "id_var"], "_", mdlID, " <- function(", tolower(paste(xpars[-1, "arg"], collapse = ", ")), "){", "\n", sep="")
  if(mdlID %in% names(cons))
  {
    xcons <- cons[[mdlID]]
    plcons <- split(xcons, xcons$id_spec)
    for(xpl in names(plcons))
    {
      cat("\n\txmdl.", xpl, "<-function(", tolower(paste(xpars[3:nrow(xpars), "arg"], collapse = ", ")), "){", "\n", sep="")
      for(j in 1:nrow(plcons[[xpl]]))
      {
        cat("\t\t", tolower(plcons[[xpl]][j,"cname"]), " = ", ifelse(!is.na(plcons[[xpl]][j,"constant"]), plcons[[xpl]][j,"constant"], "0.0") , "\n", sep="")
      }
      
      if(!is.na(dbMain[i, "ipf1"]))
      {
        cat("\t\tipf1 = ", tolower(dbMain[i, "ipf1"]), "\n", sep="")
      }
      if(!is.na(dbMain[i, "ipf2"]))
      {
        cat("\t\tipf2 = ", tolower(dbMain[i, "ipf2"]), "\n", sep="")
      }
      if(!is.na(dbMain[i, "ipf3"]))
      {
        cat("\t\tipf3 = ", tolower(dbMain[i, "ipf3"]), "\n", sep="")
      }
      cat("\t\t", tolower(dbMain[i, "formula"]), "\n", sep="")
      
      cat("\t}", "\n", sep="")
    }
  }

  cat("\n\twith(data.frame(", tolower(paste(xpars[-1, "arg"], collapse = ", ")), "),\n")
  xcase = cases[[mdlID]]
  xcase$pl <- dbSpec[match(xcase$id_cospecies, dbSpec$id_spec), "est_srt"]
  plcase <- split(xcase, xcase$id_fsp)
  for(xpl in names(plcase))
  {
    cat("\t\tifelse(p01 %in% c('", paste(plcase[[xpl]][,"pl"], collapse = "','"), "'), xmdl.",xpl,"(",tolower(paste(xpars[3:nrow(xpars), "arg"], collapse = ", ")), "), \n", sep="")
  }
  cat("\t\tNA\n")
  cat("\t",rep(")", length(names(plcase))), ")\n", sep="")
  cat("}","\n", sep="")
  sink()
  #source(fileName)
}



roxygen2::roxygenize()


