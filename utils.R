# COMMON FUNCTIONS

############################################################
# System
library("codetools")
# findGlobals(fun=TDT_extended) # from codetools !!
sys_check_globalsIn_function <- function(fun){
  findGlobals(fun) 
}

sys_lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

sys_explore_functions <- function(pattern_def){
  pos <- which(grepl(pattern=pattern_def, x=search()))
  if (pos > 0){
    print(paste("Found pattern in position/s : ", pos))
    ls(pos)  
  } else {
    print("Not found any package with this pattern")
  }  
}
free_memory <- function(){
  gc(reset=TRUE)
}
sys_free_memory <- function(){
  # To label
  free_memory()
}

sys_clear_all_plot_devices <- function(){
  ## Just clear all devices ... 
  for (dev_item in dev.list() ){
    dev.off(dev_item)  
  }  
}

sys_convert_table_into_dataframe <- function(tbl_tmp){
  # Otherwise is arranged differently (maybe invert to merge...)
  as.data.frame.matrix(tbl_tmp)
}

# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

########################################################

#####################################################


############################################################
## Entorn
## /volumsas/tmp
## http://stackoverflow.com/questions/17107206/r-how-to-change-temporary-directory
## write("TMP = '<your-desired-tempdir>'", file=file.path(Sys.getenv('R_USER'), '.Renviron'))
## write("TMP = '/volumsas/tmp'", file=file.path('.Renviron'))

extract_file_header <- function(file_name,sep="\t"){
  title.line <- readLines(file_name, n=1) 
  title.line <- unlist(strsplit(title.line, split="\t")  )  
  title.line
}

read_tsv_default <- function(file_name, header_flag=TRUE, base_path=NULL, sep_def="\t", stringsAsFactors_def=FALSE){
  if (!is.null(base_path)){
    file_name <- paste(base_path, "/", file_name ,sep="")
  }
  return(read.table(file_name,sep=sep_def,header=header_flag,blank.lines.skip=TRUE,flush=TRUE,allowEscapes=TRUE,fill=TRUE, stringsAsFactors=stringsAsFactors_def) )
}

save_that_df <- function(dataframe, base_path, name, row.names_flag=FALSE, col.names=TRUE) {  
  file_path = paste(base_path, "/",name, sep="")  
  write.table(x=dataframe, sep="\t", file=file_path, quote=FALSE, row.names=row.names_flag, col.names=col.names)  
}

list_append <- function(lst, obj) {
  # Not for reference : http://stackoverflow.com/questions/2436688/append-an-object-to-a-list-in-r-in-amortized-constant-time
  lst[[length(lst)+1]] <- obj
  return(lst)
}

is_like <- function(item, pattern) {
  grepl(x=item, pattern=pattern)
}

## SET OPERATION
`%ni%` <- Negate(`%in%`)

is.even <- function(x) x %% 2 == 0 
is.odd  <- function(x) x %% 2 != 0 

#####

# SORT
# dd[with(dd, order(-z, b)), ]
## dd[with(dd, order(L6_ID2953)), ]
#dd[with(dd, order(-L6_ID2953)), ]
#head(dd[with(dd, order(-L6_ID2953)), ])
#nrow(dd[with(dd, L6_ID2953 > 1), ])
# 670
helper_utils.check_columns_stopif <- function(cols_check, df, postfix_message=""){
  #cols_check <- c("chr_key_ovelap", "sample", "chr", "IniProbe", "EndProbe") # affected == 2 (CA) or 1(CO)
  if ( sum(colnames(df) %in% cols_check) != length(cols_check) ) {    
    cols_falti <- cols_check[which(cols_check %ni% colnames(df))]
    stop( paste("Some cols are not found! Check for ", paste(cols_falti,collapse=","), "\n See ",postfix_message ))
  }
}
sort_by_column <- function(df, col_name, des=FALSE){      
  ans <- NULL
  position_col <- which(grepl(pattern=col_name, x=colnames(df) ))
  if (length(position_col) != 1){
    stop("Error, col_name is erroneouse.")
  }  
  if ( any(class(df) %in% "data.table") ) {        
    ans <- setorderv(df, c(col_name))   # data.table includes data.frame .. #ans <- setcolorder(df, c(col_name))    
  } else {
    if (des){
      ans <- df[with(df,order(df[,position_col],decreasing=T )),]
    } else {
      ans <- df[with(df,order(df[,position_col]) ),]
    }
  }
  ans
}

#dd <- data.frame(b = factor(c("Hi", "Med", "Hi", "Low"), levels = c("Low", "Med", "Hi"), ordered = TRUE), x = c("A", "D", "A", "C"), y = c(8, 3, 9, 9), z = c(1, 1, 1, 2))
#dd[with(dd, order(-z, b)), ]

#####

change_NA_to <- function(dataframe, value=0){
  
  # Converts NA to (value) [also exist is.nan ! ]  
  mask <- apply(dataframe, 2, is.na)  
  dataframe[mask] <- value
  
  return(dataframe)
}

##############################

## Representation - for sure there 

verbalize_list <- function(l){
  ans <- NULL
  for (i in 1:length(l)){
    lb <- names(l)[i]
    val <- l[[i]]
    str <- paste(val,"(",lb,")",sep="")
    ans <- c(ans, str)
  }  
  ans <- paste(ans, collapse=",")
}

calculate_percentages_of_list <- function(l, prec=6){
  total <- sum(as.data.frame(l))
  ans <- NULL  
  for (i in 1:length(l)){
    lb <- names(l)[i]
    val <- l[[i]]
    str <- paste(round(val/total,prec),"(",lb,")",sep="")
    ans <- c(ans, str)
  }  
  ans <- paste(ans, collapse=",")
}


df_calc_position_sameOrder <- function(ids, df, col_id){
  ## Given a list, it return the position that correspond to the data frame
  obj_vec <- df[,col_id] # Necessary to get it quickly
  unlist(lapply(ids, FUN=function(id_val, obj){which(obj == id_val)}, obj=obj_vec)  )  
}




#############################

save_study_obj <- function( name, obj, current_path="/home/armand2/bioData/dbGap/summary_study/the_five_inversions/") {
  inv_study_path <- paste(current_path, name,".RData", sep="")
  save(obj, file=inv_study_path)
}


##### Benchmarking in R
# http://r.789695.n4.nabble.com/Which-system-time-component-to-use-td1572427.html
# https://code.google.com/p/rbenchmark/
# source('http://rbenchmark.googlecode.com/svn/trunk/benchmark.r')

# > system.time(Sys.sleep(10)) 
# user  system elapsed 
# 0.00    0.00   10.05 
# > pt <- proc.time(); Sys.sleep(60); proc.time() - pt 
# user  system elapsed 
# 0.00    0.00   60.01 

## NOTE : proc.time()["elapsed"] is in seconds! (so /60 in minutes)

#########################


# To check info ..
# system.time(read.table("tdata01.txt.gz", sep=","))

# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
#lsos()
# free_memory()

## TRANSFORM STRUTCTURES EFFICIENTLY
transform_list_to_data_frame <- function(){
  data.frame(matrix(unlist(anss_list), nrow=length(anss_list), byrow=T))
}

#########################################
## PLOT STATISTICS
###################

# http://monkeysuncle.stanford.edu/?p=485
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

error.line <- function(x,  upper, lower=upper, length=0.1,...){
  arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)
}


# SEE : http://monkeysuncle.stanford.edu/?p=485
plot_or_dataframe <- function(df, y_threshold_line=NULL, min_bottom=0, title="") {
  # The difference between plot_or_dataframe and plot_or_dataframe_labels
  # is that in this case the scale of x axis maybe it is wrong
  labels_def <- rownames(df)
  x <- 1:length(labels_def) # the number of the inversion! (NUMBER)  
  y <- df[,"OR"]
  # within(df, plot(OR~factor(rownames(df)),  type="p", ylim=ylim_def ))  # WORKS and it is nice .. 
  ylim_def <- c(min(df[,"UpperCI"],min_bottom), max(df[,"UpperCI"]))
  plot(x,y,ylim=ylim_def,xaxt="n", xlab="Inversions", ylab="OR (alpha 0.05)", main=title)  # xaxt is for surpress x labels
  axis(1, at=1:length(labels_def), labels=labels_def)
  # segments(1, 2, 1, 0.5)  
  error.line(x=as.numeric(x), upper=df[,"UpperCI"], lower=df[,"LowerCI"])  
  if (!is.null(y_threshold_line)){
    abline(h=y_threshold_line, col="red")    
  }
}

plot_or_dataframe_labels <- function(df, y_threshold_line=NULL, min_bottom=0) {
  # The difference between plot_or_dataframe and plot_or_dataframe_labels
  # is that in this case an string is required ...
  labels_def <- rownames(df)
  x <- 1:length(labels_def)
  #axis(1, at=1:12, labels=month.name)
  ylim_def <- c(min(df[,"UpperCI"],min_bottom), max(df[,"UpperCI"]))
  within(df, plot(OR~factor(rownames(df)),  type="p", ylim=ylim_def , xlab=""))  
  # Draw errors ... When we put factors, the names are re-arranged! trick! ...
  df_initial_order_factors <- data.frame(row.names=rownames(df), order=1:length(rownames(df)))
  row_factors <- levels(factor(rownames(df)))
  error.line(x=1:length(rownames(df)), upper=df[row_factors,"UpperCI"], lower=df[row_factors,"LowerCI"]) 
  # Draw threshold line if necessary
  if (!is.null(y_threshold_line)){
    abline(h=y_threshold_line, col="red")    
  }
}

####################

plot_inversions_neg_pvalue <- function(df, x_labels=NULL, title="", xlab="Inversions", ylab="-log(pvalue)", y_max_def=10) {
  x_items <- c(1:length(rownames(df)))
  # xaxt is to avoid print axes
  if (any(is.na(df[,"pvalue"]))) {
    pos <- which(is.na(df[,"pvalue"]))
    df[pos,"pvalue"] <- 0.9  # -log(0.9)=0.1    
  }  
  ylim_max <- max(with(df, max(-log(pvalue))),y_max_def)
  
  plot( formula=-log(pvalue)~x_items, data=df, xaxt="n", type="b", pch=21, col="black", 
        lty=3, xlab=xlab, ylab=ylab, main=title, ylim=c(0,ylim_max))
  # plot x axis names
  if (!is.null(x_labels)){
    axis(1, at=1:length(x_labels), labels=x_labels, las = 2)  
  }  
  abline(h=-log(0.05), col="red")    

}
# add x vs. 1/x 


####
# rJava
#Sys.setenv(JAVA_HOME="/usr/lib/jvm/java-6-sun-1.6.0.26/jre")
#Sys.getenv("JAVA_HOME")
#install.packages("rJava")
#library("rJava")
#install.packages(pkgs='venneuler',repos='http://www.rforge.net/')

############ PCA
# setwd("/volumsata/home/armand2/Dropbox/cursets/BDA/practiques/")

add_prefix_all_columns <- function(df, col_prefix) {
  # Rename columns
  colnames <- colnames(df)
  colnames(df) <- unlist(lapply(X=colnames, FUN=function(coln){paste(col_prefix,"_", coln, sep="")}))  
  df
}

############################

