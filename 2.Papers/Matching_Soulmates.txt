

# Matching Soulmates

## Citation 

Leo, Greg and Lou, Jian and Van der Linden, Martin and Vorobeychik, Yevgeniy and Wooders, Myrna H., "Matching Soulmates." Journal of Public Economic Theory. *forthcoming*

## Paper

[Matching Soulmates](https://onlinelibrary.wiley.com/doi/10.1111/jpet.12542)

## Matching Soulmates in R

```{r, IMSsetup, echo=FALSE}
#Operator "m %r% f" applies function "f" to matrix "m" by rows. 
`%r%` <- function(m,f){t(apply(m,1,match.fun(f)))}

#Operator "m %rm% i" removes rows and column indicies "i" from matrix "m". 
`%rm%` <- function(m,i){if(length(i)>0){m[-i,-i]}else{m}}  

#Operator applies function "f" across list "l" with sapply.
`%s%` <- function(l,f){sapply(l,match.fun(f))}

#Ensures objects is a matrix.
matrixify <- function(m){as.matrix(m)}

#Returns Hadamard product of matrix m with its transpose.
hadamard <- function(m){m * t(m)}

#Given preference matrix, returns vector of player indicies who are first-order soulmates.
whos_a_soulmate <- function(p){which((p%r%rank%>%hadamard%r%min)==1)}

#Removes first order soulmates from preference matrix p.
remove_soulmates <- function(p){p %rm% whos_a_soulmate(p) %>% matrixify}

#Recursively applies remove_soulmates until there's none left to remove.
ims <- function(p){if(dim(p)[1]==0 || identical(remove_soulmates(p),p)){p}else{ims(remove_soulmates(p))}}

#Create a random roommates preference matrix with "dim" players.
create_preference <- function(dim){
p <- matrix(runif(dim*dim),dim,dim)
diag(p)<- 1
p %r% rank
}
```

In [Matching Soulmates](../2.%20Working%20Papers/MatchingSoulmates.html), my co-authors and I study a a recursive process in matching that
forms coalitions that are mutually most-preferred by members of that coalition. We call this process the iterated matching of soulmates: *IMS*. We show that mechanisms that implement IMS have strong properties among those who are matched by IMS. 

How many people can "usually" be matched by IMS? This depends a lot on the environment and structure of preferences. But what about in totally unstructured environments? 

Here is some R code that takes advantage of R's array-centric functions and some custom operators to count the number of people that can be matched as soulmates in 10000 random 10-person [stable roommates problems](https://en.wikipedia.org/wiki/Stable_roommates_problem). 

The core of this code is contained in the *whos_a_soulmate* function. This function is somewhat non-standard R code. It is inspired by the type of programming normally done in one of R's predecessors [APL](https://tryapl.org). Here is the function: 

```{r, whos, eval=FALSE}
whos_a_soulmate <- function(p){which((p%r%rank%>%hadamard%r%min)==1)}
```

To see how this works, let's first setup an example preference matrix. 

```{r, setupp, eval=TRUE}
p <- matrix(c(3,1,2,1,3,2,2,1,3),3,3,byrow=TRUE)
p
```

Plyer 1 likes 2 best, player 2 likes 1 best, player 3 likes 2 best. Note that 1 and 2 are soulmates. We should find that 1 and 2 are soulmates in the first round of IMS. 

The first step of the function ensures the matrix "p" is a ranking matrix. We apply the "rank" function to the "p" matrix by row using the by-row operator created here. Since "p" is already a ranking matrix, this does not do anything. It is more important once some players have been removed and we need to re-rank. 

```{r, step1, eval=TRUE}
p %r% rank
```

Now we take take [Hadamard product](https://en.wikipedia.org/wiki/Hadamard_product) of the preference matrix with its own transpose by piping it to our "hadamard" function with the built-in R pipe. 

```{r, step2, eval=TRUE}
p %r% rank %>% hadamard
```

Note how off diagonal 1's in this matrix represent positions where both players prefer eachother most. We want to pull these players who have a 1 in their row. To do this, we pipe what we have to the "min" function using our by-row operator. If a 1 is present, that player has a soulmate. 


```{r, step3, eval=TRUE}
p %r% rank %>% hadamard %r% min
```

Note that 1 and 2 have a "1" here, indicating that players 1 and 2 have a soulmate from this group. Select just these players, we compare this vector to 1. The indicies that map to "TRUE" are players who have a soulmate. 


```{r, step4, eval=TRUE}
p %r% rank %>% hadamard %r% min == 1
```

We see now that players 1 and 2 have a soulmate. We can now remove these players by using our remove operator "%rm%". 

```{r, step5, eval=TRUE}
p <- p %rm% whos_a_soulmate(p) %>% as.matrix
p
```

What we get is the remainder of the preference matrix "p" after removing soulmates. We only have player 3 left. Note that player 3 originally ranked being "alone" as third-best. That's why a 3 shows up here. Let's pipe this to rank again to have the player(s) rerank their potential partners.

```{r, step6, eval=TRUE}
p %r% rank
```

Now we can run the process again.

```{r, step7, eval=TRUE}
p <- p %rm% whos_a_soulmate(p)
p
```

Since being alone is the only outcome left, player 3 is a solemate-group-of-one and is removed. This demonstrates how the "IMS" function proceeds. It calls "remove_soulmates". If anyone is removed, "IMS" calls itself. If there is no one to remove or no one further can be removed, the function returns the remaining preference matrix.  

## Code

```{r, IMS, echo=TRUE}
#Operator "m %r% f" applies function "f" to matrix "m" by rows. 
`%r%` <- function(m,f){t(apply(m,1,match.fun(f)))}

#Operator "m %rm% i" removes rows and column indicies "i" from matrix "m". 
`%rm%` <- function(m,i){if(length(i)>0){m[-i,-i]}else{m}}  

#Operator applies function "f" across list "l" with sapply.
`%s%` <- function(l,f){sapply(l,match.fun(f))}

#Returns Hadamard product of matrix m with its transpose.
hadamard <- function(m){m * t(m)}

#Given preference matrix, returns vector of player indicies who are first-order soulmates.
whos_a_soulmate <- function(p){which((p%r%rank%>%hadamard%r%min)==1)}

#Removes first order soulmates from preference matrix p.
remove_soulmates <- function(p){p %rm% whos_a_soulmate(p) %>% as.matrix}

#Recursively applies remove_soulmates until there's none left to remove.
ims <- function(p){if(dim(p)[1]==0 || identical(remove_soulmates(p),p)){p}else{ims(remove_soulmates(p))}}

#Create a random roommates preference matrix with "dim" players.
create_preference <- function(dim){
p <- matrix(runif(dim*dim),dim,dim,byrow=TRUE)
diag(p)<- 1
p %r% rank
}

#Set up parameters. "dim" is the number of players. "n" is the number of random trials. 
dim <- 10
n <- 100

#Create List of "n" Preference Matricies with "dim" players.
preference_list <- lapply(1:n,function(x){create_preference(dim)})

#Apply IMS to the list of preferences then count the number of players remaining.
remaining <- preference_list %s% ims %s% dim 

#Get the number of players removed by IMS.
removed <- dim - remaining[1,]

#Make a table of frequencies of the number of removed players.
removed %>% table/n

```

