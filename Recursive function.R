# Example recursive function

recursive_sum_function <- function( v ) {

	# Input: v, numeric vector
	# Return sum of all elements in v

	if( exists( "s" ) ) s <- s + v[ length( v ) ] else s <- 0

	if( length( v ) == 1) {
		return( s )
	} else {
		return( s + recursive_sum_function( v[ 1:length( v ) - 1 ] ) )
	}

}

# Call the function

recursive_sum_function(1:1500)
