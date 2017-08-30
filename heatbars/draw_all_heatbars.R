files <- dir("heatbars")

for (f in files) {
  if (grepl("draw_all",f)==F & grepl("heatbar_functions",f)==F) {
    print(f)
    try(
      source(paste0("heatbars/",f))
    )
  }
}
