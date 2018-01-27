import Data.List
import Data.Ord

type Coords = (Double, Double) -- x,y coordinates

-- polygon check
is_convex xs = all_negative || all_positive
    where xp = xproducts xs
          all_negative = all (<0) xp
          all_positive = all (>=0) xp

-- consecutive edge pairs
xproducts :: [Coords] -> [Double]
xproducts = rs [] . order
    where rs [] (a:[]) = [0] -- single point
          rs [] (a:b:[]) = [0] -- straight line
          rs (a:b:_) (c:[]) = [zx (c,a,b)]
          rs (a:s) (b:c:[]) = (zx (b,c,a)):(rs (a:s++[b]) [c])
          rs s (a:b:c:z) = (zx (a,b,c)):(rs (s++[a]) (b:c:z))

-- return list of coordinates in non intersecting order
order :: [Coords] -> [Coords]
order xs = sortBy (\a b -> compare (ang a) (ang b)) xs
    where ang p = atan2 (snd p - snd c) (fst p - fst c)
          c = center xs

-- find the center of the bouding box or coordinates
center :: [Coords] -> Coords
center (x:xs) = ((lx + sx) / 2, (ly + sy) / 2)
    where lx = foldr (\a b -> max (fst a) b) (fst x) xs
          ly = foldr (\a b -> max (snd a) b) (snd x) xs
          sx = foldr (\a b -> min (fst a) b) (fst x) xs
          sy = foldr (\a b -> min (snd a) b) (snd x) xs

-- z-component cross product for consecutive edge pairs
-- i.e for coordinate points in correct order
zx (a,b,c) = dx1*dy2 - dy1*dx2
    where dx1 = fst b - fst a
          dy1 = snd b - snd a
          dx2 = fst c - fst b
          dy2 = snd c - snd b
 
-- convert degrees to radians 
radians = (*) (pi / 180)
-- convert radians to degrees 
degrees = (*) (180 / pi)