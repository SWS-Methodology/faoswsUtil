##' Function to convert tree in adjacent list format to edge
##' representation.
##'
##' @param tree A data.table object with two columns.  The first column should
##' have the parent nodes and the second column should have a character vector
##' containing all the children nodes separated by commas (i.e. adjacent list
##' format).
##'
##' @return A data.table object containing two columns: parent and children.
##'
##' @export
##' 


adjacent2edge = function(tree){
    
    ## Data Quality Checks
    stopifnot(is(tree, "data.table"))
    stopifnot(sapply(tree, class) == c("character", "character"))
    
    children = strsplit(unlist(tree[, 2, with = FALSE]), ", ")
    data.table(parent = rep(unlist(tree[, 1, with = FALSE]),
                   sapply(children, length)),
               children = unlist(children))
}
