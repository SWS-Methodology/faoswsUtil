##' Get Commodity Level
##' 
##' This function takes a commodity tree and provides the level of each 
##' commodity within that tree.
##' 
##' @param commodityTree A data.table with parent and child node IDs 
##'   (corresponding to the IDs in nodes) which specify the commodity tree 
##'   structure.
##' @param parentColname The column name of commodityTree which contains the ID 
##'   of the parent node.
##' @param childColname The column name of commodityTree which contains the ID 
##'   of the child node.
##' @param returnMinLevel Logical.  If a node exists at multiple processing 
##'   levels, should the minimum level be returned?  Or, should multiple records
##'   be returned for each level?
##'   
##' @return A data.table with two columns: node (the ID of the commodity code) 
##'   and level.  A level of 0 indicates a top level node which is then 
##'   processed into a level 1 node.  Level 1 nodes are processed into level 2, 
##'   and so on.
##' 
##' @export
##' 

getCommodityLevel = function(commodityTree, parentColname, childColname,
                             returnMinLevel = TRUE){
    
    ## Data Quality Checks
    stopifnot(is(commodityTree, "data.table"))
    stopifnot(c(parentColname, childColname) %in% colnames(commodityTree))
    
    ## Update level by first assigning nodes with no parents to level 0.  Then,
    ## assign a 1 to all nodes which currently have no level (missing) and are
    ## children of a level 0.  Proceed iteratively until no NA's are left.
    levelData = data.table(node = unique(c(commodityTree[[parentColname]],
                                           commodityTree[[childColname]])),
                           level = NA_real_)
    topNodes = setdiff(commodityTree[[parentColname]],
                       commodityTree[[childColname]])
    levelData[node %in% topNodes, level := 0]
    currentLevel = 1
    while(any(is.na(levelData$level))){
        if(currentLevel > 15){
            stop("Currently found a node at a level > 15!  There's probably ",
                 "an issue with your tree, or with getCommodityLevel()")
        }
        identifiedNodes = levelData[!is.na(level), node]
        children = commodityTree[get(parentColname) %in% identifiedNodes,
                                 unique(get(childColname))]
        levelData[node %in% children & is.na(level), level := currentLevel]
        currentLevel = currentLevel + 1
    }
    levelData
}
