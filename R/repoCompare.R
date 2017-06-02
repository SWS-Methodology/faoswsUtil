#' Compare installed packages with those in a remote repository
#' 
#' It's very easy to have one's packages fall out of alignment with the 
#' Statistical Working System's. This function will help detect where the 
#' packages you have installed are different to those on the system or in any
#' other repository.
#' 
#' @param local_packages array. Object returned by functions such as 
#'   \code{installed_packages}
#' @param foreign_packages array. Object returned by functions such as 
#'   \code{available.packages}
#'   
#' @export repoCompare

repoCompare <- function(local_packages = installed.packages(), 
                        foreign_packages = available.packages()){
    
    local_df <- repo_array2df(local_packages)
    foreign_df <- repo_array2df(foreign_packages)
    
    packageDiff(local_df, foreign_df)
    
}

repo_array2df <- function(repo_array){
    df <- as.data.frame(repo_array, stringAsFactors = FALSE, optional = TRUE)
    df <- df[, c("Package", "Version")]
    return(df)
}