from __future__ import print_function
import time


def factorial( n ):
    return reduce( ( lambda x , y : x * y ) , xrange( 1 , n + 1 ) , 1 )

start = time.clock()
answer = factorial( 1000000 )
end = time.clock()

#print ( answer )
print ( "Time:" , end - start , "sec" )
