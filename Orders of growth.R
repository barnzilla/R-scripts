# Bind x to an integer vector
x <- 1:1000

# Different orders of growth
## Constant
plot( rep( 5, length( x ) ) ~ x )

## Log(x)
plot( log( x ) ~ x )

## x Log(x)
plot( x * log( x )  ~ x )

## Linear
plot( x * 10 ~ x )

## Polynomial
plot( x**2 ~ x )
plot( x**3 ~ x )
plot( x**5 ~ x )

## Exponential
plot( 2^x ~ x )