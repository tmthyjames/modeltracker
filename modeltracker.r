library(plyr)
library(dplyr)

track <- function(model, metrics=NULL, customMetrics=NULL, ...){
    
    kwargs <- list(...);
    
    environmentVars <- list(
        datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        username = Sys.info()[["user"]],
        className = class(model)[[1]]
    )
    
    form <- data.frame(formula=paste(deparse(formula(model)), collapse=''));
    
    for (env in names(environmentVars)){
        form[[env]] <-  environmentVars[[env]];
    }

    if (!is.null(metrics)){
        for (metric in metrics){
            form[[metric]]  <- summary(model)[[metric]];
        }
    }

    if (!is.null(customMetrics)){
        for (cmetric in customMetrics){
            form[[cmetric]] <- get(cmetric)(model);
        }
    }

    for (arg in names(kwargs)){
        form[[arg]] <- kwargs[[arg]];
    }
    
    DIRTY_METRICS <- FALSE;
    
    if (exists('METRICS_LIST')){
        
        DIRTY_METRICS <- FALSE %in% 
            unique(
                append(
                    names(kwargs), unlist(
                        append(
                            metrics, customMetrics
                        )
                    )
                ) == METRICS_LIST
            )
        
        if (DIRTY_METRICS){
            MMODELS <<- form;
        }
    }
    
    METRICS_LIST <<- append(
        names(kwargs), unlist(
            append(
                metrics, customMetrics
            )
        )
    )

    if (exists('MODELS')){
        COUNTER <<- COUNTER + 1;
    } else {
        MODELS <<- form;
        COUNTER <<- 1;
    }
    
    if (exists('METRICS_LIST') & DIRTY_METRICS){

        for (env in names(environmentVars)){
            MMODELS[[env]] <<-  environmentVars[[env]];
        }

        if (!is.null(metrics)){
            for (metric in metrics){
                MMODELS[[metric]]  <<- summary(model)[[metric]];
            }
        }

        if (!is.null(customMetrics)){
            for (cmetric in customMetrics){
                MMODELS[[cmetric]] <<- get(cmetric)(model);
            }
        }

        for (arg in names(kwargs)){
            MMODELS[[arg]] <<- kwargs[[arg]];
        }
    }
    
    if (COUNTER > 1){
        if (DIRTY_METRICS){
            combinedDf <- rbind.fill(MODELS, MMODELS);
            MODELS <<- combinedDf[with(combinedDf, order(datetime, decreasing=TRUE)),];
        } else {
            combinedDf <- rbind.fill(MODELS, form);
            MODELS <<- combinedDf[with(combinedDf, order(datetime, decreasing=TRUE)),];
        }
    }
    return(form);
}
