get_data <- function(ss,offset){
  if (dim(ss)[1] <= 2) stop("No data in selected spreadsheet")
  
  #==== PROCESS DATA ==========
  # Get 3 header lines
  header1 <- names(ss)
  header2 <- paste(ss[1,])
  header3 <- paste(ss[2,])
  
  # Make a new data frame with the actual data
  data <- ss[3:dim(ss)[1], ]
  
  # loop through and combine the column names
  lcol1 <- ''
  lcol2 <- ''
  lcol3 <- ''
  separator <- 'Â§'
  for (col in seq(1,dim(ss)[2])) {
    h1 <- if (grepl('^X[0-9]+$',header1[col])) lcol1 else header1[col]
    lcol1 <- h1
    h2 <- if (grepl('^NA$',header2[col])) lcol2 else header2[col]
    lcol2 <- h2
    h3 <- if (grepl('^NA$',header3[col])) lcol3 else header3[col]
    lcol3 <- h3
    var <- paste(h1,h2,h3,sep=separator)
    names(data)[col] <- var
  } 
  

  # Switch the data to long format
  data <- data %>% 
    gather("variable", "value",
            #8:(dim(ss)[2]-offset)
           names(data)[8:(dim(ss)[2]-offset)]
            ) %>% 
    separate(variable, into=c("category", "subcategory", "variable"), sep="Â§") %>%
    separate(
      variable,into=c("variable","measurement"), 
      sep="\\.",fill="right",extra="drop"
    )
  
  
  names(data) <- gsub(separator,'',names(data))
  names(data) <- gsub('Dimensions','',names(data))
  
  return(data)
}

countranges <- function(df,data,headers, measure) {
  
  countrange <- function(x, resource, measure) {
    if (measure=="range") {
      dataf <- filter(
        suppressWarnings(mutate(data,value=as.numeric(value))),
        measurement %in% c("min","max"),
        variable==resource,
        !is.na(value)
      ) %>% spread(
        measurement, value
      ) %>%
      filter(
        min <=x,
        max >=x
      )
    } else {
      dataf <- filter(
        suppressWarnings(mutate(data,value=as.numeric(value))),
        measurement==measure,
        variable==resource,
        !is.na(value),
        value>=x
      )      
    }

    dataf$TI <- if ("TI" %in% names(dataf)) dataf$TI else dataf$UT
    if (length(dataf$TI) > length(unique(dataf$TI))) {
        print("warning, some titles seem to be duplicated, do you
            need to filter by a dimension?") 
      if (measure=="range") {
    
      } else {
        dataf <- dataf %>% 
          group_by(TI) %>% 
          filter(
            value == max(value)
          ) %>% ungroup()  
      }

    } 
    return(as.numeric(count(dataf)))
    }
  
  
  
  # For each resource, count the number of values under each threshold
  for (r in headers) {
    df[[r]] <- as.numeric(lapply(df$v,countrange, r, measure))
  }
  
  data_r_sum <- filter(
      suppressWarnings(mutate(data,value=as.numeric(value))),
      measurement %in% c("min","max"),
      !is.na(value)
    ) %>% spread(
      measurement, value
    ) %>%
    filter(
      !is.na(min),
      !is.na(max)
    ) %>% 
    group_by(variable) %>%
    summarise(
      maxvalue = n()
    )
    
  
  
  # Gather the resources
  res <- df %>%
    gather(resource,value,-v) %>% #gather resources into a resource column
    group_by(resource) %>% 
    left_join(data_r_sum, by = c("resource" = "variable")) %>%
    mutate(pcnt=value/maxvalue*100) # calculate the value as a pcnt of the total
  
  return(res)
}

heatbar <- function(df,f,step=1) {
  flab <- if (f=="pcnt") "% of Studies" else "Number of Studies" 
  #df <- df[df[[f]]>0,]
  df <- df %>%
    group_by(resource) %>%
    mutate(resourcelab=paste0(resource,'\n[',max(maxvalue),' studies]'),
           dfmax = max(df$v[df$value>0]) 
           ) %>%
    filter(v <  max(df$v[df$value>0]))
  ggplot() +
    theme_bw() +
    geom_bar(
      data=df,
      aes_string(x="resourcelab", y=step, fill=f),
      stat="identity",
      width=0.6,
      color=NA
    ) +
    geom_bar(
      data=filter(df,v==min(df$v)),
      aes(x=resourcelab,y=dfmax),
      stat = "identity",
      fill=NA,
      color="grey22",
      width=0.6
    ) +
    scale_fill_gradientn(
      colours=c(NA,"#fffcef","#fcf0ba","#d30000"),
      values = scales::rescale(c(0,min(df[[f]]),max(df[[f]])/2,max(df[[f]]))),
      name=flab
    ) +
    guides(fill = guide_colourbar(reverse = TRUE))
}

heatbar_years <- function(data, df, f, grp=NA) {
  dataf <- filter(
    suppressWarnings(mutate(data,value=as.numeric(value))),
    measurement %in% c("min","max"),
    variable==costs[1],
    !is.na(value)
  ) %>% spread(
    measurement, value
  ) %>%
    group_by(PY) %>%
    mutate(
      gtot = n(),
      pn = row_number()
    ) %>%
    ungroup() %>%
    mutate(
      PY = as.numeric(PY),
      jitter= (1/gtot)*(pn-1),
      PYJ = PY + (1/gtot)*(pn-1),
      country= substr(`Data categorisationsystem boundaries`,1,15),
      region = countrycode(`Data categorisationsystem boundaries`,"country.name","ar5")
    )
  
  dataf$region[dataf$`Data categorisationsystem boundaries`=="Global"] <- "Global"
  dataf$region[grepl("Korea", dataf$`Data categorisationsystem boundaries`)] <- "ASIA"
  dataf$region[grepl("Latin America", dataf$`Data categorisationsystem boundaries`)] <- "LAM"
  dataf$region[grepl("Ecuador", dataf$`Data categorisationsystem boundaries`)] <- "LAM"
  
  w = 20
  y = 1980
  #df <- res2050[res2050[[f]]>0,]
  flab <- if (f=="pcnt") "% of Studies" else "Number of Studies" 
  
  df <- df %>%
    group_by(resource) %>%
    mutate(resourcelab=paste0(resource,'\n[',max(maxvalue),' studies]'),
           dfmax = max(df$v[df$value>0]) 
    ) %>%
    filter(v <  max(df$v[df$value>0]))
  
  if (is.na(grp)) {
    ylines <- geom_linerange(
      data=dataf,
      aes_string(x="PYJ",
                 ymin="min",
                 ymax="max"),
      size=1.5
    )
  } else{
    ylines <- geom_linerange(
      data=dataf,
      aes_string(x="PYJ",
                 ymin="min",
                 ymax="max",
                 colour=grp),
      size=1.5
    )
  } 
  
  p <- ggplot() +
    theme_bw() +
    geom_bar(
      data=df,
      aes_string(x=y,y=1,fill=f),
      stat="identity",
      width=w,
      color=NA
    ) +
    geom_bar(
      data=filter(df,v==min(df$v)),
      aes(x=y,y=dfmax),
      stat = "identity",
      fill=NA,
      color="grey22",
      width=w
    ) +
    scale_fill_gradientn(
      colours=c(NA,"#fffcef","#fcf0ba","#d30000"),
      values = scales::rescale(c(0,min(df[[f]]),max(df[[f]])/2,max(df[[f]]))),
      name=flab
    ) +
    scale_x_continuous(
      breaks = c(1990,2000,2010)
    ) +
    guides(fill = guide_colourbar(reverse = TRUE)) +
    ylines + labs(
      x = "Year",y="Costs in $US(2011)/tCO2"
    )
  
  print(p)
  
  return(dataf)
}
