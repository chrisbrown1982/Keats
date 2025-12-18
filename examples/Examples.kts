module Examples where

-- natural numbers  
type ( n : *)

-- zero element
assume ( z : n)

-- successor element
assume (s : n -> n)

let id = z : n

-- addition
let add = (\m . \n . ((\f . \x . m f (n f x)) : (n -> n) -> (n -> n)) ) 
 	: ((n -> n) -> (n -> n)) -> ((n -> n) -> (n -> n)) -> ((n -> n) -> (n -> n))

-- church numerals
-- (\f . \x . x )
let test = (\n . n) : (n -> n)
let zero = (\f . ((\x . x) : (n -> n))) : (n -> n) -> (n -> n)

-- let one = (\n . \x . (n x)) : (n -> n) -> (n -> n)
-- let two = (\f . \ n . (f (f n))) : (n -> n) -> (n -> n)

-- some numbers
-- let n0 = zero s z
-- let n1 = one s z
-- let n2 = two s z

-- addition
-- let add = (\m . \n . ((\f . \x . m f (n f x)) : (n -> n) -> (n -> n))) 
-- 	: ((n -> n) -> (n -> n)) -> ((n -> n) -> (n -> n)) -> ((n -> n) -> (n -> n))