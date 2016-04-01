##' Function to convert tree in adjacent list format to edge
##' representation.
##'
##' @param tree A data.table object with two columns.  The first column should
##' have the parent nodes and the second column should have a character vector
##' containing all the children nodes separated by commas (i.e. adjacent list
##' format).
##' @param sep character. The separator between elements of the second column
##'
##' @return A data.table object containing two columns: parent and children
##' 
##' @examples
##' 
##' library(data.table)
##' children <- matrix(sample(letters, 4 * 3, replace = TRUE), nrow=3)
##' children <- apply(children, 1, paste0, collapse = ", ")
##' adj <- data.table(parent = c("a", "b", "c"), children = children)
##' 
##' adjacent2edge(adj)
##' 
##' @export
##' 


adjacent2edge = function(tree, sep = ", "){
    
    ## Data Quality Checks
    stopifnot(is(tree, "data.table"))
    stopifnot(sapply(tree, class) == c("character", "character"))
    
    children = strsplit(unlist(tree[, 2, with = FALSE]), sep)
    data.table(parent = rep(unlist(tree[, 1, with = FALSE]),
                   sapply(children, length)),
               children = unlist(children))
}
