-- this won't compile
-- double : ty -> ty
-- double x = x + x

double : Num ty => ty -> ty
double x = x + x
