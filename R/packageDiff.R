#' Function to compare two sets of packages
#'
#' @param source_pkgs data.frame
#' @param dest_pkgs data.frame.
#' @param exclude_base character. Package priority types to exclude. If you
#'   don't want to exclude any, set this to NULL
#'   
#' @keywords internal

packageDiff <- function(source_pkgs, dest_pkgs, exclude_base = c("base", "recommended")){
    stopifnot(c("Package", "Version") %in% names(source_pkgs))
    stopifnot(c("Package", "Version") %in% names(dest_pkgs))
    stopifnot(is.data.frame(source_pkgs), is.data.frame(dest_pkgs))
    
    if(is.data.table(source_pkgs)){
        source_pkgs <- as.data.frame(source_pkgs)
    }
    
    if(is.data.table(dest_pkgs)){
        dest_pkgs <- as.data.frame(dest_pkgs)
    }
    
    ## Type conversion
    
    source_pkgs <- source_pkgs[, c("Package", "Version")]
    source_pkgs$Package <- as.character(source_pkgs$Package)
    # source_pkgs$origVersion <- source_pkgs$Version
    # source_pkgs$Version <- package_version(source_pkgs$Version)
    row.names(source_pkgs) <- NULL
    
    dest_pkgs <- dest_pkgs[, c("Package", "Version")]
    dest_pkgs$Package <- as.character(dest_pkgs$Package)
    # dest_pkgs$origVersion <- dest_pkgs$Version
    # dest_pkgs$Version <- package_version(dest_pkgs$Version)
    row.names(dest_pkgs) <- NULL
    
    if(!is.null(exclude_base)){
        exclude <- as.data.frame(installed.packages())
        exclude <- exclude[exclude$Priority %in% exclude_base, "Package"]
        
        dest_pkgs <- dest_pkgs[!(dest_pkgs$Package %in% exclude),]
        source_pkgs <- source_pkgs[!(source_pkgs$Package %in% exclude),]
    }
    
    # five categories
    # 1 - new        - Package not installed in destination
    # 2 - updated    - Package older than source
    # 3 - downgraded - Package newer than source
    # 4 - obsolete   - Package not present in source
    # 5 - unchanged  - Package is the same in both
    
    #### NAME COMPARISONS ####
    
    #### new ####
    new_names <- setdiff(source_pkgs$Package, dest_pkgs$Package)
    new <- source_pkgs[source_pkgs$Package %in% new_names, ]
    
    #### obsolete ####
    
    obsolete_names <- setdiff(dest_pkgs$Package, source_pkgs$Package)
    obsolete <- dest_pkgs[dest_pkgs$Package %in% obsolete_names, ]
    
    #### VERSION COMPARISONS ####
    
    source_intersect <- source_pkgs[!(source_pkgs$Package %in% new_names), ]
    names(source_intersect) <- c("Package", "sVersion")
    
    dest_intersect <- dest_pkgs[!(dest_pkgs$Package %in% obsolete_names), ]
    names(dest_intersect) <- c("Package", "dVersion")
    
    pkg_intersect <- merge(source_intersect, dest_intersect)
    
    #### updated ####
    
    updated_names <- pkg_intersect$Package[package_version(pkg_intersect$sVersion) > package_version(pkg_intersect$dVersion)]
    updated <- source_pkgs[source_pkgs$Package %in% updated_names,]
    updated$destVersion <- dest_pkgs[dest_pkgs$Package %in% updated_names, "Version"]
    
    #### downgraded ####
    
    downgraded_names <- pkg_intersect$Package[package_version(pkg_intersect$sVersion) < package_version(pkg_intersect$dVersion)]
    downgraded <- source_pkgs[source_pkgs$Package %in% downgraded_names,]
    downgraded$destVersion <- dest_pkgs[dest_pkgs$Package %in% downgraded_names, "Version"]
    
    #### unchanged ####
    
    unchanged_names <- pkg_intersect$Package[package_version(pkg_intersect$sVersion) == package_version(pkg_intersect$dVersion)]
    unchanged <- source_pkgs[source_pkgs$Package %in% unchanged_names,]
    
    
    #### OUTPUT ####
    
    list(unchanged = unchanged,
         new = new,
         obsolete = obsolete,
         updated = updated,
         downgraded = downgraded)
    
}
