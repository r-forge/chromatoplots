setMethod("explore", c("cpSample", protocolClass("removeBaseline")),
          function(object, protocol, raw, mz)
          {
            explore_prof_filter(object, raw=raw,mz=mz,subtract= TRUE)
          })

explore_prof_filter <- function(object, protocol, raw, mz, subtract = TRUE)
{
  ## FIXME: replace the code with faster qtpaint function
  time <- raw@scantime
  mzmin <- raw@mzrange[1]
  rawint <- raw@env$profile[mz-mzmin+1,]
  chrom <- data.frame(time = time, intensity = rawint)
  p <- qplot(time,rawint,data=chrom,geom="point")
  p$title=NULL
  corint <- object@env$profile[mz-mzmin+1,]
  if (subtract) {
    res <- corint
    fit <- rawint - corint
  } else {
    res <- rawint - corint
    fit <- corint
  }
  p_chrom_fit <- p + geom_line(aes(x = time, y = fit), 
                               colour = "gray50")
  p_chrom_res <- qplot(time, res, ylab = "residuals")
  stack_plots(paste("Baseline Subtraction  ","(MZ = ",mz,")  "),
              list(p_chrom_fit,p_chrom_res),
              weights=c(1,1))
}


## cplotViewBL by ggplot2 used for plotting single slice at certain mz
cplotViewBL <- function(raw,mz,ylim=NULL){
  time <- raw@scantime
  mzmin <- raw@mzrange[1]
  rawint <- raw@env$profile[mz-mzmin+1,]
  if(is.null(ylim)) ylim <- range(rawint)
  chrom <- data.frame(time = time, intensity = rawint)
  p <- qplot(time,rawint,data=chrom,geom="point")+opts(title=paste('Mz=',mz))+scale_y_continuous(limits=ylim)
  print(p)
}





