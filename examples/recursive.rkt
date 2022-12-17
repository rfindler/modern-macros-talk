#lang recursive-language
 
import Pr, Cn, s, id, z, const;
 
sum = Pr[id_1^1, Cn[s, id_3^3]];
print sum(2, 23); # should be 25

prod = Pr[z, Cn[sum, id_1^3, id_3^3]];
print prod(3, 3); # should be 9
 
fact =
  Cn[Pr[const_1, Cn[prod, Cn[s, id_2^3], id_3^3]],
     id_1^1, id_1^1];
 
print fact(fact(3)); # should be 720

print fact(Cn[s, s](4)); # should be 720
 
check fact(3) = 6;
