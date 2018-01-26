##' Validate Flags of Commodity Tree
##' 
##' This function perform Flag Validation for the commodity Tree
##' Validation. 
##' The possible flags are (decided with Data Team): 
##'
##' Extraction Rates:
##' (T,-) = validated up do 2013 - protected
##' (E,t) = copied from 2014 onwards - not protected but that do not change during standardization process
##' (E,f) = manually changed by the user - protected
##'
##' Shares: 
##' (E,-) = coming from old methodology - NOT protected. These values willbe overwritten
##' at any run of the module, except the values of oils, which are kept, unless manually changed
##' (E,f) = manually changed by the user - protected
##' (I,i) = calculated by the module - not protected
##' 
##' @param tree the commodity tree to check
##'   
##' @return a message with the information of validity of flags
##'   
##' @export
##' 

ValidateTreeFlags = function(tree= NULL){
    ## Data Quality Checks
    if(!exists("swsContext.datasets")){
        stop("No swsContext.datasets object defined.  Thus, you probably ",
             "won't be able to read from the SWS and so this function won't ",
             "work.")
    }
    # Checks for data
    stopifnot(c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", 
                "measuredItemChildCPC", "timePointYears", "Value", "flagObservationStatus", 
                "flagMethod") %in% colnames(tree))
    
    if("5423"%in%tree[,measuredElementSuaFbs]){
        stop("Elements have to be expressed in names: extractionRate, share")
    }
    # 5423 = Extraction Rate [hg/t]
    # 5431 = Share of utilization [%]
    
    tree=tree[,mget(c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", 
                      "measuredItemChildCPC", "timePointYears", "Value", "flagObservationStatus", 
                      "flagMethod"))]
    
    ##create column for check
    ##(this column will be deleted)
    tree[,checkFlags:=paste0("(",flagObservationStatus,",",flagMethod,")")]
    
    
    ##############################################################
    #################### CHECK FLAG VALIDITY  ####################
    ##############################################################
    
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
            message2=paste0("Email sent to ", swsContext.userEmail)
        }else{
            if(file.exists(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"__0_invalidFlags.csv"))){
                file.remove(paste0("debugFile/Batch_",batchnumber,"/B",batchnumber,"__0_invalidFlags.csv"))
                dir.create(paste0(PARAMS$debugFolder,"/Batch_",batchnumber), showWarnings = FALSE,recursive=TRUE)
                write.csv(invalidFlags, paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"__0_invalidFlags.csv"), row.names = FALSE)  
                
            }
            message2=paste0("Csv file saved in: ", paste0(PARAMS$debugFolder,"/Batch_",batchnumber,"/B",batchnumber,"__0_invalidFlags.csv"))
            
        } 
        
        paste0("Invalid Flags in the commodity Tree.",message2)
        
    }else{  # End of case for which flags are invalid
        if(!CheckDebug()){
            body = paste("The Commodity Tree has been validated for Flags",
                         " ",
                         "No check has been performed regarding extraction Rate and share's range",
                         "No check has been performed regarding the values of shares by child",
                         sep='\n')
            sendmailR::sendmail(from = "sws@fao.org",
                                to = swsContext.userEmail,
                                subject = sprintf("tree successfully downloaded and Checked"),
                                msg = strsplit(body,"\n")[[1]])
            
        }
        message("Commodity Tree flags are valid")
        tree
        
    }
}
