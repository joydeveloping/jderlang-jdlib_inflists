# Infinite lists module

Module jdlib_inflists is a part of jdlib library.
It operates with infinite lists.
Infinite list has a head and function for generating next element.
When we take first element from infinite list next element becomes its head and so on.

### Creating infinite lists

Infinite lists are created with functions `iterate/3`, `iterate/2`. There is a list of functions for creating particular infinite lists:

- `repeat/2` - infinite number of copies of single term.

```erlang
repeat(a)
[a, a, a, a, a, ...]
```

- `cycle/1` - append result of infinite number of copies of given list (`grundy_series/0`).

```erlang
cycle([a, b, c])
[a, b, c, a, b, c, a, ...]

grundy_series()
[1, -1, 1, -1, 1, -1, ...]
```

- `seq/2` - sequence with given start element and step (`odds/0`, `evens/0`, `seq/1`, `naturals/0`, `naturals/1`).

```erlang
seq(5, 3)
[5, 8, 11, 14, 17, 21, ...]

evens()
[2, 4, 6, 8, 10, 12, ...]
```

- `geometric_series/2` - geometric series with given base and factor (`power_series/1`).

```erlang
geometric_series(3, 1.5)
[3, 4.5, 6.75, 10.125, 15.1875, ...]

power_series(3)
[1, 3, 9, 27, 81, ...]
```

- `fib/0` - Fibonacci series.

```erlang
fib()
[1, 1, 2, 3, 5, 8, 13, 21, ...]
```

- `harmonic_series/0` - harmonic series (inversed naturals).

```erlang
harmonic_series()
[1.0, 0.5, 0.3333333333333333, 0.25, 0.2, ...]
```

- `anharmonic_series/0` - Leibniz series.

```erlang
anharmonic_series()
[1.0, -0.3333333333333333, 0.2, -0.14285714285714285, ...]
```

- `facts/0`, `inv_facts/0` - factorials and inversed factorials.

```erlang
facts()
[1, 1, 2, 6, 24, 120, 720, ...]

inv_facts()
[1.0, 1.0, 0.5, 0.16666666666666666, ...]
```

### Access to infinite lists parts

`head/1`, `tail/1', `ht/1`, `take/1`, `nth/1`, `drop/1`, `nthtail/1`

### Simple operations on single infinite list

`sublist/2`, `sublist/3`, `split/2`, `attach_list/2`, `attach/2`

### Zip and unzip functions

`zip/2`, `zip_3/3`, `unzip/1`, `unzip_3`

### Higher order functions on infinite lists

`zipwith/3`, `map/2`, `adj_pairs_map/2`, `mapfold/2`, `is_all/3`, `is_any/3`

### Mathematical functions

`add/2`, `sub/2`, `neg/1`, `mul/2`, `dvs/2`, `inv/1`, `square/1`, `sqrt/1`, `pow/2`, `sum/1`, `product/1`, `avg/1`

### Additional operations on infinite lists

`sparse/2, `odds/1`, `evens/1`, `merge/2`, `unmerge/1`, `sign_alternate/1`

### Taylor series

`taylor_exp/1`, `taylor_lnxp1/1`, `taylor_sin/1`, `taylor_cos/1`, `taylor_arctg/1`

