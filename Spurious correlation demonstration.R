# Demonstration of a spurious correlation (and the need for compositional analysis)

# Generate random numbers
x <- runif( 10000, 0, 1000 )
y <- runif( 10000, 0, 1000 )
z <- runif( 10000, 0, 1000 )

# Compute correlation matrix
cor( data.frame( x, y, z ) )

# Create compositional data
xz <- x / z
yz <- y / z

# Computer correlation
cor( xz, yz )