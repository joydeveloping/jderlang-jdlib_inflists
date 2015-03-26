# Infinite lists module

Module jdlib_inflists is a part of jdlib library.
It operates with infinite lists.
Infinite list has a head and function for generating next element.
When we take first element from infinite list next element becomes its head and so on.

### Creating infinite lists

Infinite lists are created with functions `iterate/3`, `iterate/2`. There is a list of functions for creating particular infinite lists:

- `repeat/2` - infinite number of copies of single term.

```erlang
repeat(a).
[a, a, a, a, a, ...]
```

- `cycle/1` - append result of infinite number of copies of given list (`grundy_series/0`).

```erlang
cycle([a, b, c]).
[a, b, c, a, b, c, a, ...]

grundy_series().
[1, -1, 1, -1, 1, -1, ...]
```

- `seq/2` - sequence with given start element and step (`odds/0`, `evens/0`, `seq/1`, `naturals/0`, `naturals/1`).

```erlang
seq(5, 3).
[5, 8, 11, 14, 17, 21, ...]

evens().
[2, 4, 6, 8, 10, 12, ...]
```

- `geometric_series/2` - geometric series with given base and factor (`power_series/1`).

```erlang
geometric_series(3, 1.5).
[3, 4.5, 6.75, 10.125, 15.1875, ...]

power_series(3).
[1, 3, 9, 27, 81, ...]
```

- `fib/0` - Fibonacci series.

```erlang
fib().
[1, 1, 2, 3, 5, 8, 13, 21, ...]
```

- `harmonic_series/0` - harmonic series (inversed naturals).

```erlang
harmonic_series().
[1.0, 0.5, 0.3333333333333333, 0.25, 0.2, ...]
```

- `anharmonic_series/0` - Leibniz series.

```erlang
anharmonic_series().
[1.0, -0.3333333333333333, 0.2, -0.14285714285714285, ...]
```

- `facts/0`, `inv_facts/0` - factorials and inversed factorials.

```erlang
facts().
[1, 1, 2, 6, 24, 120, 720, ...]

inv_facts().
[1.0, 1.0, 0.5, 0.16666666666666666, ...]
```

### Access to infinite lists parts

Function `head/1` returns head of infinite list,  `tail/1' returns tail of infinite list, `ht/1` returns both of them in one time.
Function `take/2` is needed to get given number of infinite list elements from its beginning.
Function `nth/2` returns element with given number.
Function drop returns infinite list without first elements `drop/2` (`nthtail/2` is the same).

```erlang
N = naturals().
[1, 2, 3, 4, 5, ...]

head(N).
1

tail(N).
[2, 3, 4, 5, 6, ...]

ht(N).
{1, [2, 3, 4, 5, 6, ...]}

take(N, 10).
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

nth(N, 10).
10

drop(N, 10).
[11, 12, 13, 14, 15, ...]
```

### Simple operations on single infinite list

Function `sublist/3` returns part of list from start element number to end element number.
Function `sublist/2` takes part of infinite list from start element number 1 (so it is `take/2`).
Function `split/2` splits infinite list into finite beginning and infinite tail.
Functions `attach_list/2` and `attach/2` attache list or term to the beginning of given infinite list.

```erlang
N = naturals().
[1, 2, 3, 4, 5, ...]

sublist(N, 10, 5).
[10, 11, 12, 13, 14]

split(N, 5).
{[1, 2, 3, 4, 5], [6, 7, 8, 9,...]}

attach(N, 0).
[0, 1, 2, 3, 4, 5, ...]

attach_list(N, [0, 0, 0]).
[0, 0, 0, 1, 2, 3, ...]
```

### Zip and unzip functions

Functions `zip/2` and `zip_3/3` process zipping of two or three lists of terms into single list of tuples.
Functions `unzip/1` and `unzip_3` is reverse functions.

```erlang
N = naturals().
[1, 2, 3, 4, 5, ...]
A = repeat(a).
[a, a, a, a, a, ...]
B = repeat(b).
[b, b, b, b, b, ...]

Z2 = zip(N, A).
[{1, a}, {2, a}, {2, a}, ...]

Z3 = zip_3(N, A, B).
[{1, a, b}, {2, a, b}, {2, a, b}, ...]

unzip(Z2).
{[1, 2, 3, 4, 5, ...], [a, a, a, a, a, ...]}

unzip_3(Z3).
{[1, 2, 3, 4, 5, ...], [a, a, a, a, a, ...], [b, b, b, b, b, ...]}
```

### Higher order functions on infinite lists

Function `zipwith/3` zips two infinite lists with given function.
Function `map/2` applies given function to each element of infinite list.
Function `adj_pairs_map/2` applies function to each pair of adjacent elements of infinite list.
Function `mapfold/3` apply function to each element of infinite list (like `map/2`) but uses additional accumulate parameter.
Functions `is_all/3` and `is_any/3` check are all elements (one any element) with number not greater than given one satisfy some predicate (boolean function).

```erlang
N = naturals().
[1, 2, 3, 4, 5, ...]
E = evens().
[2, 4, 6, 8, 10, ...]

zipwith(N, E, fun(X, Y) -> X + Y end).
[3, 6, 9, 12, 15, ...]

map(N, fun(X) -> X * X end).
[1, 4, 9, 16, 25, 36, ...]

adj_pairs_map(N, fun(X, Y) -> X * Y end).
[2, 6, 12, 20, 30, ...]

mapfold(N, fun(X, A) -> X + A end, 5).
[6, 8, 11, 15, 20, ...]

is_all(N, fun(X) -> X < 10 end, 5).
true

is_any(E, fun(X) -> X rem 2 == 1 end, 100).
false
```

### Mathematical functions

Functions `add/2`, `sub/2`, `neg/1`, `mul/2`, `dvs/2`, `inv/1`, `square/1`, `sqrt/1`, `pow/2` implement componentwise mathematical operations on infinite lists.
Functions `sum/1`, `product/1`, `avg/1` process sublists.
Functions `dirichlet_series/1`, `dirichlet_series/2` calculate Dirichlet series with given infinite list or with list of 1-s by default.

```erlang
N = naturals().
[1, 2, 3, 4, 5, ...]
C = repeat(5).
[5, 5, 5, 5, 5, ...]

add(N, C).
[6, 7, 8, 9, 10, ...]

sub(N, C).
[-4, -3, -2, -1, 0, 1, 2, ...]

neg(N).
[-1, -2, -3, -4, -5, ...]

mul(N, C).
[5, 10, 15, 20, 25, ...]

dvs(N, C).
[0.2, 0.4, 0.6, 0.8, 1.0, ...]

inv(C).
[0.2, 0.2, 0.2, 0.2, 0.2, ...]

square(N).
[1, 4, 9, 16, 25, 36, ...]

sqrt(N).
[1.0, 1.4243135623730951, 1.7320508075688772, 2.0, ...]

pow(N, 3).
[1.0, 8.0, 27.0, 64.0, 125.0, ...]

sum(N).
[1, 3, 6, 10, 15, 21, ...]

product(N).
[1, 2, 6, 24, 120, 720, ...]

avg(N).
[1.0, 1.5, 2.0, 2.5, 3.0, 3.5, ...]

dirichlet_series(3).
[1.0, 0.125, 0.03703703703703703, 0.015625, ...]

dirichlet_series(N, 3).
[1.0, 0.25, 0.11111111111111108, 0.0625, 0.04000000000000001, ...]
```

### Additional operations on infinite lists

Function `sparse/2` takes every (N + 1)-th element of infinite list (`odds/1`, `evens/1` are particular cases).
Function `merge/2` merges two list into one, `unmerge/1` is reverse operation.
Function `sign_alternate/1` multiplies infinite list with Grundy series cycle([1, -1]).

```erlang
N = naturals().
[1, 2, 3, 4, 5, ...]
A = repeat(a).
[a, a, a, a, a, ...]

sparse(N, 5).
[1, 7, 13, 19, 25, ...]

merge(N, A).
[1, a, 2, a, 3, a, ...]

unmerge(N).
{[1, 3, 5, 7, 9, ...], [2, 4, 6, 8, 10, ...]}

sign_alternate(N).
[1, -2, 3, -4, 5, -6, ...]
```

### Taylor series

Functions `taylor_exp/1`, `taylor_lnxp1/1`, `taylor_sin/1`, `taylor_cos/1`, `taylor_arctg/1` returns taylor series of some functions.

```erlang
taylor_exp(0.5).
[1.0, 0.5, 0.125, 0.020833333333333332, 0.0026041666666666665, ...]

taylor_lnxp1(0.5).
[0.5, -0.125, 0.041666666666666664, -0.015625, 0.00625, ...]

taylor_sin(0.5).
[0.5, -0.020833333333333332, 2.6041666666666666e-4, -1.5500992063492063e-6, ...]

taylor_cos(0.5).
[1.0, -0.125, 0.0026041666666666665, -2.170138888888889e-5, ...]
```

