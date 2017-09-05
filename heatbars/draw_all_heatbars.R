rm(list=ls())

files <- dir("heatbars")

i <- 0

for (f in files) {
  if (grepl("draw_all",f)==F & grepl("heatbar_functions",f)==F) {
    i <- i + 1
    print(f)
    save.image()
    t <- try(
      source(paste0("heatbars/",f))
    )
    if(!is(t,"try-error")) {
      newdata <- data
      newdata$PY <- as.numeric(newdata$PY)
      newdata$technology <- u_sheetName
      load(".RData")
      if (i==1) {
        all_data <- newdata
      } else {
        all_data <- bind_rows(all_data, newdata)
      }    
      rm(newdata)
    } else {
      load(".RData")
    }
  }
}


