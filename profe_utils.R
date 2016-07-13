
`%ni%` <- Negate(`%in%`)


get_dies_entre_dates_str <- function(data_ini="2016/6/1", data_fi="2016/6/27"){
  seq <- seq(as.Date(data_ini), as.Date(data_fi), "days")
  
}

calc_get_dies_str <- function(data_ini="2016/6/1", data_fi="2016/6/27", dies_filtrar=NA){
  # seq <- seq(as.Date("2016/6/1"), as.Date("2016/6/27"), "days")
  seq <- get_dies_entre_dates_str(data_ini, data_fi)
  if (class(dies_filtrar) == "Date") { # !is.na(dies_filtrar)
    seq <- seq[seq %ni%  dies_filtrar]
  }  
  seq
}
calc_count_dies_setmana_str <-  function(data_ini="2016/6/1", data_fi="2016/6/27", dies_filtrar=NA){
  seq <- calc_get_dies_str(data_ini, data_fi, dies_filtrar)
  dies <- unlist(lapply(seq, weekdays))
  table(dies)
}

days_as_weekdays <- function(dies){
  unlist(lapply(dies, weekdays))
}
# -----------------------------------------
# Casament 17 Març fins 7 Abril!?
dies_jesuites_festa_1516 <- c(as.Date("2015/09/24"), as.Date("2015/12/7"),as.Date("2015/12/8"),
                              get_dies_entre_dates_str("2015/12/23", "2016/01/8"),
                             as.Date("2016/02/15"),as.Date("2016/02/15"),
                             get_dies_entre_dates_str("2016/03/17", "2016/04/07"),
                             get_dies_entre_dates_str("2016/05/13", "2016/05/16")
                           )
# Calc
dies <- calc_get_dies_str(data_ini = "2015/09/15",data_fi = "2016/04/28", dies_filtrar = dies_jesuites_festa_1516)
table(days_as_weekdays(dies))

dies[dies >= as.Date("2016/3/27")]

# ASIX2
d <- dies[dies >= as.Date("2015/9/18") & dies <= as.Date("2015/11/6")]
t_tmp <- table(days_as_weekdays(d)) # Monday 2, Fr 1
t_tmp["Monday"]*2+t_tmp["Friday"] # 22

d <- dies[dies >= as.Date("2015/11/9") & dies <= as.Date("2016/1/15")]
t_tmp <- table(days_as_weekdays(d)) # Monday 2, Fr 1
t_tmp["Monday"]*2+t_tmp["Friday"] # 21

d <- dies[dies >= as.Date("2016/1/18") & dies <= as.Date("2016/3/18")]
t_tmp <- table(days_as_weekdays(d)) # Monday 2, Fr 1
t_tmp["Monday"]*2+t_tmp["Friday"] # 24

d <- dies[dies >= as.Date("2016/3/21") & dies <= as.Date("2016/4/29")]
t_tmp <- table(days_as_weekdays(d)) # Monday 2, Fr 1
t_tmp["Monday"]*2+t_tmp["Friday"] # 9
# 22+21+24+9


# SMX2BM8
d <- dies[dies >= as.Date("2015/9/17") & dies <= as.Date("2015/10/3")]
t_tmp <- table(days_as_weekdays(d)) # 
t_tmp["Tuesday"]*2+t_tmp["Thursday"]*2+t_tmp["Friday"] # 11

d <- dies[dies >= as.Date("2015/10/9") & dies <= as.Date("2015/11/6")]
t_tmp <- table(days_as_weekdays(d)) # 
t_tmp["Tuesday"]*2+t_tmp["Thursday"]*2+t_tmp["Friday"] # 21

d <- dies[dies >= as.Date("2015/11/10") & dies <= as.Date("2016/1/19")]
t_tmp <- table(days_as_weekdays(d)) # 
t_tmp["Tuesday"]*2+t_tmp["Thursday"]*2+t_tmp["Friday"] # 37

d <- dies[dies >= as.Date("2016/1/21") & dies <= as.Date("2016/2/25")]
t_tmp <- table(days_as_weekdays(d)) # 
t_tmp["Tuesday"]*2+t_tmp["Thursday"]*2+t_tmp["Friday"] # 27

d <- dies[dies >= as.Date("2016/2/26") & dies <= as.Date("2016/4/29")]
t_tmp <- table(days_as_weekdays(d)) # 
t_tmp["Tuesday"]*2+t_tmp["Thursday"]*2+t_tmp["Friday"] # 28


# SMX2AM6
d <- dies[dies >= as.Date("2015/9/18") & dies <= as.Date("2015/12/4")]
t_tmp <- table(days_as_weekdays(d)) # 
t_tmp["Friday"]*2 # 24 

d <- dies[dies >= as.Date("2015/12/11") & dies <= as.Date("2016/4/29")]
t_tmp <- table(days_as_weekdays(d)) # 
t_tmp["Friday"]*2 # 28



dies[dies >= as.Date("2015/11/17") & dies <= as.Date("2015/11/27")]


# --------------------

## Exemple dies
seq <- seq(as.Date("2016/6/1"), as.Date("2016/6/27"), "days")
seq

format(seq[1], "%d %b %Y %d")
weekdays(seq[1])

seq[seq %ni% as.Date("2016-06-26")]

dies <- unlist(lapply(seq, weekdays))

table(dies)[c("Monday","Wednesday")]



##########################


