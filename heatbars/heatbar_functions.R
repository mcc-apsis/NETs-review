get_data <- function(ss,offset=2){
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
  
  gvars <- c(8:(dim(ss)[2]-offset))

  a <- gvars[1]
  b <- tail(gvars,1)
  
  gnames <- names(data)[8:(dim(ss)[2]-offset)]
  except <- "Potentials in tCO2/yrÂ§Â§conversion factor to common unit"
  gnames <- gnames[gnames!=except]
  
  a <- gnames[1]
  b <- tail(gnames,1)
  
  # Switch the data to long format
  data <- data %>% 
    gather_("variable", "value",
           paste(gnames)
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

countranges <- function(df, data, headers, measure) {
  
  if (measure !="range") {
    onames <- names(data)
    dataf <- suppressWarnings(mutate(data,value=as.numeric(value))) %>%
      group_by(variable) %>%
      filter(!is.na(measurement)) %>%
      spread(measurement, value )
    dataf$max[is.na(dataf$max)] <- dataf$estimate[is.na(dataf$max)]
    newnames <- names(dataf)[!(names(dataf) %in% onames)]
    newnames <- newnames[nchar(newnames)>0]
    data_cleaned <- dataf %>%
      gather_("measurement","value",newnames)    
  }
  

  
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
        data_cleaned,
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
    
    return(nrow(dataf))
    }
  
  
  
  # For each resource, count the number of values under each threshold
  for (r in headers) {
    df[[r]] <- as.numeric(lapply(df$v,countrange, r, measure))
  }
  
  if (measure == "range") {
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
  } else {
    data_r_sum <- filter(
      data_cleaned,
      measurement == measure,
      !is.na(value)
    ) %>% 
      group_by(variable) %>%
      summarise(
        maxvalue = n()
      )    
  }

  # Gather the resources
  res <- df %>%
    gather(resource,value,-v) %>% #gather resources into a resource column
    group_by(resource) %>% 
    left_join(data_r_sum, by = c("resource" = "variable")) %>%
    mutate(pcnt=value/maxvalue*100) # calculate the value as a pcnt of the total
  
  return(res)
}

calc_cscale <- function(df, f, flab, fixed =T) {
  if (fixed==TRUE) {
    cscale <- scale_fill_gradientn(
      colours=c("#fffcef","#ffeda0","#feb24c","#f03b20","#f03b20"),
      #values = scales::rescale(c(0,33,66,100)),
      values = scales::rescale(c(0,25,50,75,100)),
      limits = c(0,100),
      name=flab
    )
  } else {
    cscale <- scale_fill_gradientn(
      colours=c(NA,"#fffcef","#fcf0ba","#d30000"),
      values = scales::rescale(c(0,min(df[[f]]),max(df[[f]])/2,max(df[[f]]))),
      name=flab
    )
  }
}

heatbar <- function(df,f,step=1, fixed=T, text=F) {
  flab <- if (f=="pcnt") "% of Studies" else "Number of Studies" 
  #df <- df[df[[f]]>0,]
  df <- df %>%
    group_by(resource) %>%
    mutate(resourcelab=paste0(resource,'\n[',max(maxvalue),' studies]'),
           dfmax = max(df$v[df$value>0]),
           mostagreement = max(value)
           ) %>%
    filter(v <  max(df$v[df$value>0]))

  dfagreement <- df %>%
    ungroup() %>%
    group_by(resourcelab) %>%
    filter(
      value==mostagreement
    ) %>%
    summarise(
      med = median(v),
      min = min(v),
      max = max(v),
      n = median(value),
      pcnt = median(pcnt)
    ) %>%
    mutate(
      text = paste0(min,"-",max,"\n",round(pcnt),"%")
    )
    
  cscale <- calc_cscale(df, f, flab, fixed)
  
  p <- ggplot() +
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
    cscale +
    guides(fill = guide_colourbar(reverse = TRUE))
  
  if (text) {
    p <- p +
      geom_crossbar(
        data=dfagreement,
        aes(x=resourcelab, ymin=med, ymax=med, y=med, width=scales::rescale(n,to=c(0.2,0.9))),
        stat="identity"
      ) +
      geom_text(
        data=dfagreement,
        aes(resourcelab, med, label=text),
        #angle=90,
        #hjust=0,
        vjust=0,
        nudge_y=20
      )
  }
    
  return(p)
}


heatbar_years <- function(data, df, f, grp=NA, fixed=TRUE) {
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
  
  
  cscale <- calc_cscale(df, f, flab, fixed)
  
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
    cscale +
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



library(ggplot2)  ## devtools::install_github("hadley/ggplot2)
library(grid)     ## rasterGrob
library(EBImage)  ## readImage
library(ggthemes) ## theme_minimal

## ##########
## INDEPENDENT CODE TO BE SOURCED:
## ##########
# user-level interface to the element grob
my_axis = function(img) {
  structure(
    list(img=img),
    class = c("element_custom","element_blank", "element") # inheritance test workaround
  )
}
# returns a gTree with two children: the text label, and a rasterGrob below
element_grob.element_custom <- function(element, x,...)  {
  stopifnot(length(x) == length(element$img))
  tag <- names(element$img)
  # add vertical padding to leave space
  g1 <- textGrob(paste0(tag, "\n\n\n\n\n"), x=x, vjust=1.2)
  g2 <- mapply(rasterGrob, x=x, image=element$img[tag], 
               MoreArgs=list(vjust=0.1, interpolate=FALSE,
                             height=unit(3,"lines")),
               SIMPLIFY=FALSE)
  
  gTree(children=do.call(gList, c(list(g1), g2)), cl="custom_axis")
}
# gTrees don't know their size and ggplot would squash it, so give it room
grobHeight.custom_axis = heightDetails.custom_axis = function(x, ...)
  unit(6, "lines")
## ##########
## END
## ##########


evcf <- function(x) {
  if (grepl("[[:alpha:]]",x)) {
    return(1)
  }
  if (is.na(x)) {
    return(1)
  }
  t <- paste0("1*",x)
  t <- gsub("**","*",t,fixed=T)
  t <- gsub(",",".",t, fixed=T)
  f <- eval(parse(text = t))
  return(f)
}


