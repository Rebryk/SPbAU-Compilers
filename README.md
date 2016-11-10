## Types
Just `Int`

## stdin/stdout
Read variable `read(x)` <br>
Write variable `write(x)`

## Operations
* `!!`, `&&`
* `<`, `<=`, `==`, `!=`, `>=`, `>`
* `+`, `-`, `*`, `/`, `%`

## Loops
### If expression
```
if a == b then
  print(1)
else
  print(0)
fi;
```

```
if b != 0 then
  a := a / b
fi;
```

### While Loops
```
read(n);

while n > 0 do
  print(n);
  n := n - 1
od;
```

### For Loops
```
for read(n), n > 0, n := n - 1 do
  print(n)
od;
```

### Repeat until
```
read(n);

repeat
  print(n);
  n := n - 1
until n > 0;
```
