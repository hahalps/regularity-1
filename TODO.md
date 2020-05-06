- determinize NFAe into NFA?
- DFAs?
- union, intersection, negation
- equivalence?
- smart constructors
- **regex matching using derivatives**

  basic idea: given a regex E, compute
  
  `deriv_a(E)` = residual, after matching a
  
  to match a string s:
  
  ```
  s = a1 a2 a3 ... an
  
  deriv_an(... deriv_a2(deriv_a1(E)) ...)
  
    ^^ empty included?
  ```
