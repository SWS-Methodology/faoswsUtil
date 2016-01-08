##' SWS Dimension Aggregation
##' 
##' Aggregate the suppled data.table along the named dimension using the supplied aggregation tree.
##'
##' Aggregates the data in the supplied data.table according to the supplied key
##' tree. The function works from the bottom of the key tree upwards, cascading
##' the aggregation until it reashes the highest level of the tree structure.
##' The level at which the aggregation stops can be controlled using the
##' \code{high_level} parameter. If the \code{agg_flags} parameter is supples it
##' should contain a data.table with a single row containing the flag(s) to be
##' applied to aggregated values; column names should be the same as those
##' supplied in the \code{data} parameter. Any orphans are present as an
##' attribute.
##'
##' @param data A long form data.table with the data to be aggregated - result
##'   of a GetData() call
##' @param keys A vector containing the data.table key column names (i.e. not
##'   the value of flag columns)
##' @param dim_name The name of the dimension to be aggregated
##' @param key_tree The aggregation tree in the form data.table (parent,
##'   children)
##' @param agg_flags A data.table containing the flags to attach to aggregated
##'   values (optional)
##' @param high_level The highest level to which to aggregate to (optional,
##'   default=0 [highest])
##'   
##' @return The aggregated rows of data as a data.table
##'
##' @examples \dontrun{
##' data <- GetData(swsContext.datasets[[1]])
##' keys <- colnames (data)
##' keys <- keys[!(keys %in% c("Value", "flagObservationStatus", "flagMethod"))]
##' key_tree <- GetCodeTree (domain="agriculture", dataset="agriculture", dimension="measuredItemCPC")
##' flags <- data.table (flagObservationStatus=c("I"), flagMethod=c("s"))
##'
##' ag_data <- aggregateDimension (data, "measuredItemCPC", key_tree, agg_flags=flags)
##' 
##' ## Another example
##' 
##' flags <- data.table(myFlag = c("A")) 
##' key_tree <- data.table(
##'   parent = c(rep("a", 3), rep ("a.1", 3), rep("b", 3)),
##'   children = c("a.1", "a.2", "a.3",
##'                "a.1.x", "a.1.y", "a.1.z",
##'                "b.1", "b.2", "b.3")
##' )
##' data <- data.table(
##'   targetDimension = c("a.2", "a.3",
##'                       "a.1.x", "a.1.y", "a.1.z",
##'                       "b.1", "b.2", "b.3", "c.1"),
##'   otherDimension = c(rep("10", 9)),
##'   Value = c(1:9),
##'   myFlag = c(rep("-", 9))
##' )
##' keys <- colnames(data)
##' keys <- keys[!(keys %in% c("Value", "myFlag"))]
##' 
##' ## Do the aggregation
##' aggs <- aggregateDimension(data, keys, "targetDimension",
##'                            key_tree, agg_flags = flags)
##' 
##' aggs
##' str(aggs)
##' attr(aggs, "orphans")
##' }
##'
##' @author  J.Browning, N.A.Connell
##' 
##' @import faosws data.table plyr
##' 
##' @export aggregateDimension
###############################################################################
aggregateDimension = function (data, keys, dim_name, key_tree, agg_flags=NULL, high_level=0)
{
  
  IS_AGG_COL <- "is_aggregate"
  IS_AGG_VAL <- "yes"
  
	orig_col_order <- colnames (data)

## Prepare the key table, stripped of unused entries and with level info
	key_table <- preprocess_key_tree (data, dim_name, key_tree)
	setnames (x = key_table, old = "children", new = dim_name)

## Prepare the flags by adding a join column
	if (!is.null(agg_flags))
	{
		agg_flags[, (IS_AGG_COL) := IS_AGG_VAL]
	}

## Merge the key and the data
	keyed_data <-
		merge (data, key_table, by = dim_name, all.x = TRUE,
			  allow.cartesian = TRUE)
	setkeyv(x = keyed_data, cols = dim_name)

## Work out the range of aggregation levels
	min_level <- high_level
	curr_level <- max (keyed_data$level, na.rm=TRUE)

## This is no longer a global variable - R users do not expect side effects of functions
	orphans <- NULL
	if (curr_level > 0)
	{
		orphans <- keyed_data[is.na(parent)]
		keyed_data <- keyed_data[!is.na(parent)]
	}

## columns <- all keys but the dimension being aggregated
	aggregate_index <- c("parent", keys)
	aggregate_index <- aggregate_index[!aggregate_index %in% dim_name]

## prepare for the first loop
	aggregated_data <- NULL
	pass_data <- keyed_data

## Aggregate the levels, bottom up
	while (curr_level >= min_level)
	{
		aggregated_level <-
			pass_data[pass_data$level == curr_level,
					   list(Value = sum(Value, na.rm = TRUE),
							  is_aggregate = IS_AGG_VAL),
					   by = aggregate_index]

## If we have any, set the flag(s)
		if (!is.null(agg_flags))
		{
			agg_flags[, (IS_AGG_COL) := IS_AGG_VAL]
			aggregated_level <- plyr::join (aggregated_level, agg_flags, by=IS_AGG_COL)
		}
		aggregated_level[, (IS_AGG_COL) := NULL]
## Change the parent column to be the name of the dimension ready for the merge
		setnames(aggregated_level, "parent", dim_name)

## Attach the parent to each aggregation entry
		keyed_data <-
			merge (aggregated_level, key_table, by = dim_name, all.x = TRUE,
				   allow.cartesian = TRUE)
		setkeyv(x = keyed_data, cols = dim_name)

## The aggregates just calculated belong in the next level up
		keyed_data[, level := (curr_level - 1)]

## remove and log orphans - these are missing entries from the hierarchy
		if (curr_level > 0) ## the top level does not have parents
		{
			if (is.null(orphans))
			{
				orphans <- keyed_data[is.na(parent)]
			} else
			{
				orphans <- rbind (orphans, keyed_data[is.na(parent)])
			}
			aggregated_level <- keyed_data[!is.na(parent)]
		} else
		{
			aggregated_level <- keyed_data
		}
		
		if (is.null(aggregated_data))
		{
			aggregated_data <- aggregated_level
		} else
		{
			aggregated_data <- rbind (aggregated_data, aggregated_level)
		}

## Prepare the data for the next level, it consists of everything input to this level
## plus the aggregates from this level
		if (curr_level > 0)
		{
			pass_data <- rbind (pass_data, aggregated_level)
		}
		curr_level <- curr_level - 1
	}

## Transform the output aggregated data to match the incoming raw data
	if (!is.null (aggregated_data))
	{
		aggregated_data[, c("level", "parent") := c(NULL, NULL)]
		setcolorder (aggregated_data, as.vector(orig_col_order))
	}

	setattr(aggregated_data, "orphans", orphans)
	aggregated_data
}

###############################################################################
##' Preprocess key tree
##' 
##' This performs two functions, #1 it removes any key values
##'	not present in the supplied data, #2 it parses the tree and gives each entry
##'	a level number, where level zero is where the parent of the branch has not
##'	parents of its own (i.e. is a root)
##'
##' @rdname preprocess_key_tree
##' @keywords internal
##'
##' @param data The data to be aggregated
##' @param dim_name The name of the dimension in the data to aggregate
##' @param key_table_in The key tree to prepare in the form data.table(parent,children)
##' @return The prepared key tree with an added "level" column
###############################################################################
preprocess_key_tree = function (data, dim_name, key_table_in)
{
  ## Copy the key table supplied so we don't modify the original
  key_table <- copy (key_table_in)
  ## Filter key_table to only have parents of current keys
  key_table <- key_table[, keep := FALSE]
  data_keys <- unique (data[,(dim_name), with=FALSE])
  key_table[children %in% data_keys[[1]], keep := TRUE]
  newSum <- key_table[, sum(keep)]
  oldSum <- 0
  while(newSum > oldSum)
  {
    ## Keep parents of keys which are being kept
    keptKeys <- key_table[(keep), unique(parent)]
    key_table[children %in% keptKeys, keep := TRUE]
    oldSum <- newSum
    newSum <- key_table[, sum(keep)]
  }
  key_table <- key_table[(keep), ]
  key_table[, keep := NULL]
  
  ## Set the tree entry levels (zero == root)
  curr_level <- 0
  key_table[!(parent %in% children), level := curr_level]
  next_generation <- unique (key_table[key_table$level == curr_level]$children)
  while (length (next_generation) > 0)
  {
    curr_level <- curr_level + 1
    key_table[parent %in% next_generation, level := curr_level]
    next_generation <- unique (key_table[key_table$level == curr_level]$children)
  }
  
  key_table
}
