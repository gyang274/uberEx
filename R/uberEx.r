#------------------------------------------------------------------------------#
#------------------------------ uberEx::uberEx.r ------------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#------------------------------------ main ------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------ menu ------------------------------------#
#------------------------------------------------------------------------------#

#' create_menu_cuisine_list
#' @description
#' create a menu with cuisine and number of dishes within each cuisine
#' @param cuisine a character vector of cuisine letter, e.g., c("A", "B", "C")
#' @param ndishes an integer vector of number of dishes within each cuisine,
#'  must have the same length with cuisine, e.g., c(5L, 3L, 4L)
#' @return menu data.table
#' @note
#'  create_menu_cuisine_list(cuisine = c("A", "B", "C"), ndishes = c(5L, 3L, 4L))
create_menu_cuisine_list <- function(cuisine, ndishes) {

  #- check input integrity
  if ( length(cuisine) != length(ndishes) ) {

    stop("create_menu_cuisine_list: cuisine and ndishes should have same length.\n")

  }

  #- handle edge case
  # if ( length(cuisine) == 1L ) {
  #
  #   return( paste0(cuisine, 1L:ndishes) )
  #
  # }

  #- create cusine-dish list
  l = purrr::pmap(
    list(cuisine, ndishes), function(x, y) { paste0(x, 1:y) }
  )

  #- smart "enumeration" by taking into account data-structure
  if ( length(l) > 1L ) {

    n = vapply(l, length, 0L) # in fact, n = ndishes, but this is more generic

    #- number of rows in output
    nrows = prod(n)

    #- x is a vector along with cuisine/ndishes that specifies
    #- how often each cuisine-dish[n] should be change over ..
    x = c(rev(cumprod(rev(n))[1L:(length(n) - 1L)]), 1L)

    for ( i in seq_along(x) ) {

      y = l[[i]]

      if ( i == 1L ) {

        #- rep.int and rep_len are faster simplified versions of rep for two common cases
        l[[i]] = rep.int(y, times = rep.int(x[i], n[i]))

      } else if ( i == length(n) ) {

        l[[i]] = rep.int(y, times = nrows / (x[i] * n[i]))

      } else {

        l[[i]] = rep.int(rep.int(y, times = rep.int(x[i], n[i])), times = nrows / (x[i] * n[i]))

      }

    }

  }

  data.table::setattr(l, "class", c("data.table", "data.frame"))

  data.table::setattr(l, "names", cuisine)

  return(l)

}

#------------------------------------------------------------------------------#
