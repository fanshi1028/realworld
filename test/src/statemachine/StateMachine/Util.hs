-- |
module StateMachine.Util where

findByRef :: (Foldable t, Eq a1) => a1 -> t (a1, a2) -> Maybe a2
findByRef ref = foldl' (\acc (ref', u) -> if ref == ref' then Just u else acc) Nothing

findByRef2 :: (Foldable t, Eq a1) => a1 -> t (a2, a1) -> Maybe a2
findByRef2 ref = foldl' (\acc (u, ref') -> if ref == ref' then Just u else acc) Nothing

findByRefAll :: (Foldable t, Eq a1) => a1 -> t (a1, a2) -> [a2]
findByRefAll ref = foldl' (\acc (ref', u) -> if ref == ref' then u : acc else acc) []

findByRef2All :: (Foldable t, Eq a1) => a1 -> t (a2, a1) -> [a2]
findByRef2All ref = foldl' (\acc (u, ref') -> if ref == ref' then u : acc else acc) []

deleteByRef :: (Foldable t, Eq a) => a -> t (a, b) -> [(a, b)]
deleteByRef ref = foldl' (\acc orig@(ref', _) -> if ref == ref' then acc else orig : acc) []

updateByRef :: (Foldable t, Eq a) => a -> ((a, b) -> (a, b)) -> t (a, b) -> [(a, b)]
updateByRef ref f = foldl' (\acc orig@(ref', _) -> if ref == ref' then f orig : acc else orig : acc) []
