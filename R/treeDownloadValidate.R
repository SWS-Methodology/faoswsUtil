##' Download Commodity Tree and validate
##' 
##' This function downoad commodity tree form "suafbs" Domain and perform Flag Validation and Value 
##' Validation
##' 
##' @param areakeys A character vector containing the M49 country codes 
##' @param yearvals A character vector containing years to be downloaded
##'   
##' @return a commodity Tree
##'   
##' @export
##' 

treeDownloadValidate = function(areakeys= NULL,
                                yearvals= NULL){
    ## Data Quality Checks
    stopifnot(is(areakeys, "character"))
    stopifnot(is(yearvals, "character"))
    if(!exists("swsContext.datasets")){
        stop("No swsContext.datasets object defined.  Thus, you probably ",
             "won't be able to read from the SWS and so this function won't ",
             "work.")
    }
    treeelemKeys = c("5423", "5431")
    # 5423 = Extraction Rate [hg/t]
    # 5431 = Share of utilization [%]
    
    treeitemPKeys = faosws::GetCodeList(domain = "suafbs", dataset = "ess_fbs_commodity_tree", "measuredItemParentCPC")
    treeitemPKeys = treeitemPKeys[, code]
    
    treeitemCKeys = faosws::GetCodeList(domain = "suafbs", dataset = "ess_fbs_commodity_tree", "measuredItemChildCPC")
    treeitemCKeys = treeitemCKeys[, code]
    
    treekey = faosws::DatasetKey(domain = "suafbs", dataset = "ess_fbs_commodity_tree", dimensions = list(
        geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = areaKeys),
        measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = treeelemKeys),
        measuredItemParentCPC = Dimension(name = "measuredItemParentCPC", keys = treeitemPKeys),
        measuredItemChildCPC = Dimension(name = "measuredItemChildCPC", keys = treeitemCKeys),
        timePointYears = Dimension(name = "timePointYears", keys = yearVals)
    ))
    
    
    tree = faosws::GetData(treekey,omitna = FALSE)
    if("flag_obs_status_v2"%in%colnames(tree)){
        setnames(tree,"flag_obs_status_v2","flagObservationStatus")
    }
    
    
    if(nrow(tree[measuredElementSuaFbs=="5423"&is.na(Value)])>0){
        tree[measuredElementSuaFbs=="5423"&is.na(Value),flagObservationStatus:="T"]
        tree[measuredElementSuaFbs=="5423"&is.na(Value),flagMethod:="-"]
        tree[measuredElementSuaFbs=="5423"&is.na(Value),Value:=0]
    }
    if(nrow(tree[measuredElementSuaFbs=="5431"&is.na(Value)])>0){
        tree[measuredElementSuaFbs=="5431"&is.na(Value),flagObservationStatus:="E"]
        tree[measuredElementSuaFbs=="5431"&is.na(Value),flagMethod:="-"]
        tree[measuredElementSuaFbs=="5431"&is.na(Value),Value:=0]
    }    
    
    
    tree[measuredElementSuaFbs=="5423",measuredElementSuaFbs:="extractionRate"]
    tree[measuredElementSuaFbs=="5431",measuredElementSuaFbs:="share"]
    
    
    ##create column for check
    ##(this column will be deleted)
    tree[,checkFlags:=paste0("(",flagObservationStatus,",",flagMethod,")")]
    
    
    ##############################################################
    #################### CHECK FLAG VALIDITY  ####################
    ##############################################################
    
    ## check if the Flags are valid.
    ## The possible flags are (decided with Data Team): 
    ##
    ## Extraction Rates:
    ## (T,-) = validated up do 2013 - protected
    ## (E,t) = copied from 2014 onwards - not protected but that do not change during standardization process
    ## (E,f) = manually changed by the user - protected
    ##
    ## Shares: 
    ## (E,-) = coming from old methodology - NOT protected. These values willbe overwritten
    ## at any run of the module, except the values of oils, which are kept, unless manually changed
    ## (E,f) = manually changed by the user - protected
    ## (I,i) = calculated by the module - not protected
    ##
    ##
    
    validERflags=c("(T,-)","(E,t)","(E,f)")
    validSHflags=c("(E,-)","(E,f)","(I,i)")
    
    
    # define parameters for export
    if(any(!(tree[measuredElementSuaFbs=="extractionRate",unique(checkFlags)]%in%validERflags))|
       any(!(tree[measuredElementSuaFbs=="share",unique(checkFlags)]%in%validSHflags))){
        
        FILETYPE = ".csv"
        CONFIG <- faosws::GetDatasetConfig(swsContext.datasets[[1]]@domain, swsContext.datasets[[1]]@dataset)
        sessionid <- ifelse(length(swsContext.datasets[[1]]@sessionId), 
                            swsContext.datasets[[1]]@sessionId,
                            "core")
        
        basename <- sprintf("%s_%s",
                            "invalidFlags",
                            sessionid)
        basedir <- tempfile()
        dir.create(basedir)
        destfile <- file.path(basedir, paste0(basename, FILETYPE))
        
        if(any(!(tree[measuredElementSuaFbs=="extractionRate",unique(checkFlags)]%in%validERflags))){
            invalidER=tree[measuredElementSuaFbs=="extractionRate"&(!checkFlags%in%validERflags)]
            if(any(!(tree[measuredElementSuaFbs=="share",unique(checkFlags)]%in%validSHflags))){
                invalidSH=tree[measuredElementSuaFbs=="share"&(!checkFlags%in%validERflags)]
                invalidFlags=rbind(invalidER,invalidSH)
                invalidFlags[,checkFlags:=NULL]
                message = "Invalid Flags for Extraction Rates and Shares"
            }else{
                invalidFlags=invalidER
                invalidFlags[,checkFlags:=NULL]
                message = "Invalid Flags for Extraction Rates"
                
            }
        }else{
            if(any(!(tree[measuredElementSuaFbs=="share",unique(checkFlags)]%in%validSHflags))){
                invalidSH=tree[measuredElementSuaFbs=="share"&(!checkFlags%in%validSHflags)]
                invalidFlags=invalidSH
                invalidFlags[,checkFlags:=NULL]
                message = "Invalid Flags for Shares"
                
            }
        }
        
        write.csv(invalidFlags, destfile, row.names = FALSE)  
        
        on.exit(file.remove(destfile))    
        zipfile <- paste0(destfile, ".zip")
        withCallingHandlers(zip(zipfile, destfile, flags = "-j9X"),
                            warning = function(w){
                                if(grepl("system call failed", w$message)){
                                    stop("The system ran out of memory trying to zip up your data. Consider splitting your request into chunks")
                                }
                            })
        
        on.exit(file.remove(zipfile), add = TRUE)
        body = paste("Standardization stopped because of:",message,
                     " ",
                     "Look attached file for details of invalid Flags",
                     " ",
                     "The file can be modified and uploaded in the Commodity Tree",
                     " ",
                     "=================================================",
                     "Valid Flags for Commodity Tree are the following",
                     " ",
                     "Extraction rates:",
                     "(T,-) = validated up do 2013 - protected",
                     "(E,t) = copied from 2014 onwards - not protected but that do not change during standardization process",
                     "(E,f) = manually changed by the user - protected",
                     " ",
                     "Shares:",
                     "(E,-) = coming from old methodology - NOT protected. These values willbe overwritten",
                     "any run of the module, except the values of oils, which are kept, unless manually changed",
                     "(E,f) = manually changed by the user - protected",
                     "(I,i) = calculated by the module - not protected"
                     ,sep='\n')
        
        if(!CheckDebug()){
            sendmailR::sendmail(from = "sws@fao.org",
                                to = swsContext.userEmail,
                                subject = sprintf("Standardization stopped for invalid flags"),
                                msg = list(strsplit(body,"\n")[[1]], 
                                           sendmailR::mime_part(destfile, 
                                                                name = paste0(basename, FILETYPE)
                                           )
                                )
            )
        }else{
            if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"__0_invalidFlags.csv"))){
                file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"__0_invalidFlags.csv"))
                dir.create(paste0(PARAMS$debugFolder,"/Batch_",batchnumber), showWarnings = FALSE,recursive=TRUE)
                write.csv(invalidFlags, paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"__0_invalidFlags.csv"), row.names = FALSE)  
                
            }
        } 
        
        paste0("Invalid Flags in the commodity Tree.Email sent to ", swsContext.userEmail)
        
    }else{  
        if(!CheckDebug()){
            body = paste("The Commodity Tree has been validated for",
                         "Flags and general validity of figures",
                         " ",
                         "No check has been performed regarding the values of shares by child",
                         "=================================================",
                         "If shares have to be used",
                         "Consistency of shares by child has to be checked"
                         ,sep='\n')
            sendmailR::sendmail(from = "sws@fao.org",
                                to = swsContext.userEmail,
                                subject = sprintf("tree successfully downloaded and Checked"),
                                msg = strsplit(body,"\n")[[1]])
            
        }
        message("tree successfully downloaded and Checked")
        tree
        
    }
}
