module Int2 where

-- import `plus` & `times` on Nats;
-- use these functions where appropriate below.
open import Nat

data Int : Set where
  -- (+ n) represents positive n.
  + : Nat → Int
  -- (- n) represents negative n.
  - : Nat → Int
  -- 0 can be represented as (+ zero) or (- zero).

-- given i, return i + 1.
isuc : Int → Int
isuc (+ n) = + (suc n)
isuc (- zero) = + (suc zero)
isuc (- (suc n)) = - n

-- given i, return i - 1.
ipred : Int → Int
ipred  (+(suc n)) = + n -- for n increase?
ipred  (+ zero) = - zero -- for cases where +/- zero
ipred  (- zero) = - (suc zero) -- defines -zero


-- given i, return -i.
ineg : Int → Int
ineg (- n) = + n
ineg (+ n) = - n

-- given i & j, return i + j.
iplus : Int → Int → Int
iplus (+ n) (+ m) = + (n + m) -- for positive ints
iplus (- n) (- m) = - (n + m) -- for negative ints
iplus (+ (suc m)) (-n) = - n 

-- given i & j, return i - j.
iminus : Int → Int → Int
iminus i j = iplus i (ineg j) -- negation for subtraction

-- given i & j, return i * j.
itimes : Int → Int → Int
itimes (+ m) (+ n) = + (m * n) -- for positive ints
itimes (- m) (- n) = + (m * n) -- for negative ints
itimes (+ m) (- n) = - (m * n) -- for negative ints
itimes (- m) (+ n) = - (m * n) -- for negative ints

