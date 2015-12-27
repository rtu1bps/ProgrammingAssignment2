##################################################
##
## FILENAME: cachematrix.R
##
## DESCRIPTION:
##    Matrix inversion and caching routines.
##
## FUNCTIONS:
##    makeCacheMatrix() : stores matrix 'x' and it's inverse 'i'.
##    cacheSolve()      : inverts matrix 'x' and calls
##                      : makeCacheMatrix() to store result.
##
## NOTES: N/A
##
## AUTHOR: Jose DelaEspriella
##
## DATE: 20151227
##
##################################################


# function 'makeCacheMatrix' stores matrix 'x' and it's inverse 'i'.
# param {matrix} x - matrix to be inverted.
makeCacheMatrix <- function(x = matrix()) {
	# main()

	# 'i' holds the i of matrix 'x'.
	i <- NULL

	# function 'set' re-sets the allready in memory matrix 'x'
	# param {matrix} y - matrix to be inverted.
	set <- function(y) {

		# * Set free variable 'x' to 'y' the in parent env.
		# * 'x' is NOT an argument of f(set),
		#    therefore 'x' is a free variable and the
		#    scoping rules must be used to ascertain the
		#    value that is to be associated with it.
		# *  R first looks for variable 'x' in the local
		#    enviroment, then in the parent enviroment.
		x <<- y

		# Set 'i' to nullllify the cache in the parent
		#  inviroment so x can be re-inverted by f(setsolve).
		i <<- NULL
	}

	# function 'get' prints matrix 'x'
	get <- function() x

	# function 'setsolve' stores the inverse of matrix 'x'
	# param {matrix} inv - pre-calculated inverted matrix.
	setsolve <- function(inv) i <<- inv

	# function 'getsolve' prints the cached innverted matrix
	#  stored with f(setsolve).
	getsolve <- function() i

	# * From  time  to  time you may create a list of functions that
	#   you  want  to  be  available without having to use a special
	#   syntax.
	# * This allow us to use the functions as obj methods.
	list(set = set,
			 get = get,
			 setsolve = setsolve,
			 getsolve = getsolve)
}


# function 'cacheSolve' inverts matrix 'x' and calls
#  makeCacheMatrix$setSolve(i) to cache the results.
# param {matrix} x - matrix to be inverted.
cacheSolve <- function(x, ...) {
	# main()
  # Return a matrix that is the 'i' of 'x'

	# Set 'i' to the obj 'x$getsolve()'
	i <- x$getsolve()

	# Do we have the 'i' of 'x' cached?
	# if 'i' != null
	if(!is.null(i)) {
		# Print message
		message("getting cached data")
		# Returns cached data and exits function
		return(i)
	}

	# ELSE
	# Retreive the matrix...
	data <- x$get()

	# ...compute the mean...
	i <- solve(data, ...)

	# ...cache the mean...
	x$setsolve(i)

	# ...return 'i'
	i
}
