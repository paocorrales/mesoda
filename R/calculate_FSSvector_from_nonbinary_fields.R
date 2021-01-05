#' Calculate the FSS for non-binary fields
#'
#' Adapted from the SpatialVx. Calculates the value of Fraction Skill Score (FSS)
#' for multiple neighborhood sizes.
#'
#' @param fbind1 A numeric matrix representing the first field.
#' Any values are allowed in the matrix.
#' @param fbind2  A numeric matrix representing the first field.
#' Any values are allowed in the matrix. The matrix needs to have the same dimensions as fbin1.
#' @param nvector A numeric vector containing neighborhood sizes for which the
#' FSS values are to be calculated. Only positive odd values are allowed in the vector.
#' A square neighborhood shape is assumed and the specified value represents the length
#' of square side.
#'
#' @export
calculate_FSSvector_from_nonbinary_fields <- function (fbin1, fbin2, nvector)
{
  if (ncol(fbin1) != ncol(fbin2) || nrow(fbin1) != nrow(fbin2)) {
    stop("The two input binary fields don't have the same dimensions !!!")
  }
  # if (sum(fbin1 == 0) + sum(fbin1 == 1) != length(fbin1) ||
  #     sum(fbin2 == 0) + sum(fbin2 == 1) != length(fbin2)) {
  #   stop("At least one of the two fields contains a non-binary value. Only values 0 and 1 are allowed in the binary input fields.")
  # }
  if (identical(nvector, round(nvector)) == FALSE || length(which(nvector%%2 !=
                                                                  1)) > 0) {
    stop("nvector can only contain odd positive integer values representing the neigberhood size!")

  }

  d = max(ncol(fbin1), nrow(fbin1))
  fbin1enl = SpatialVx:::enlarge_matrix(fbin1, d)
  fsum1enl = SpatialVx::calculate_summed_field(fbin1enl)
  fbin2enl = SpatialVx:::enlarge_matrix(fbin2, d)
  fsum2enl = SpatialVx::calculate_summed_field(fbin2enl)
  return(SpatialVx::calculate_FSSvector_from_enlarged_summed_fields(fsum1enl,
                                                         fsum2enl, nvector, d))
}
