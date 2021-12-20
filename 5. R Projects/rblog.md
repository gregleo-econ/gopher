

# R Blog

## Code Golf Abundant Numbers 11/15/21 @codegolf

The goal in *code golf* is to produce a program that solves a problem with as few characters as possible. Here is a code golf problem where R does pretty well.

### Abundant Numbers

This problem comes from [Here](<https://code.golf/abundant-numbers>](https://code.golf/abundant-numbers).

*An abundant number is a number for which the sum of its proper divisors (divisors not including the number itself) is greater than the number itself. For example **12** is abundant because its proper divisors are **1**, **2**, **3**, **4**, and **6** which add up to **16**.*

*Print all the abundant numbers from **1** to **200** inclusive, each on their own line.*

### Code: (46 Characters)

This code takes advantage of R's matrix functions and operators.

```{r abundant}
a=1:200
t(t(a[((!outer(a,a,"%%"))%*%a)>=2*a]))
```
