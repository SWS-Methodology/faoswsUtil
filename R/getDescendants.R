##' Get descendants
##' 
##' This function takes a data.table in a tree like structure and returns, for 
##' each top node, a data.table with all the descendants (children of children 
##' of ...).
##' 
##' @param tree A data.table object.
##' @param parentColname The column name of the parent column of tree.
##' @param childColname The column name of the child column of tree.
##'   
##' @return A data.table object with two columns: the first is the column of all
##'   the top level nodes and the next contains all the descendants of that
##'   node.
##' 
##' @export
##' 

getDescendants = function(tree, parentColname, childColname){
  ## Find the top nodes.
  topNodes = setdiff(tree[[parentColname]], tree[[childColname]])
  allNodes = unique(tree[[parentColname]], tree[[childColname]])
  tree = tree[!is.na(get(childColname)), ]
  tree = tree[!is.na(get(parentColname)), ]
  
  ## Initialize a data.table with the top nodes and their children, then iterate
  ## through the tree.
  out = tree[get(parentColname) %in% topNodes, ]
  out[, level := 1]
  currentTree = tree[!get(parentColname) %in% topNodes, ]
  toMerge = copy(tree)
  setnames(toMerge, c(parentColname, childColname),
           c(childColname, paste0(childColname, ".new")))
  while(nrow(currentTree) > 0){
    newRecords <- merge(out, toMerge, by = childColname,
                        allow.cartesian = TRUE)
    newRecords[, level := level + 1]
    newRecords[, c(childColname) := get(paste0(childColname, ".new"))]
    newRecords[, paste0(childColname, ".new") := NULL]
    setcolorder(newRecords, c(parentColname, childColname, "level"))
    out = unique(rbind(out, newRecords))
    currentTree = currentTree[!get(childColname) %in% out[[childColname]], ]
  }
  out = out[, min(level), by = c(parentColname, childColname)]
  setnames(out, "V1", "level")
  return(out)
}