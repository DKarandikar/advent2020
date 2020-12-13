This module provides an implementation of the constructive algorithm for $x$ in the \href{https://en.wikipedia.org/wiki/Chinese_remainder_theorem}{Chinese remainder theorem}.
\begin{code}
module CRT ( crt ) where 
\end{code}

\section{Helpers}

The \href{https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm}{extended Euclidean algorithm} finds the coefficients of \href{https://en.wikipedia.org/wiki/B%C3%A9zout's_identity}{Bézout's identity}. For two inputs $a$ and $b$ it finds $x$ and $y$ s.t. 
\begin{equation}
ax + by = gcd(a,b)
\end{equation}

We can implement this as follows:

\begin{code}
extendedEu :: Integer -> Integer -> (Integer, Integer)
extendedEu _ 0 = (1, 0)
extendedEu a b = 
  let 
    (q, r) = quotRem a b
    (s, t) = extendedEu b r
  in 
    (t, s - q * t)
\end{code}

As such, we then trivially have:

\begin{code}
gcd :: Integer -> Integer -> Integer
gcd a b = a*x + b*y
  where (x, y) = extendedEu a b
\end{code}


\section{CRT}
\subsection{Description}

Chinese remainder theorem tells us that for a list of integers $n_{1},...,n_{k}$ and a second list $a_{1},...,a_{k}$, there exists soem $x$ that solves the following:
\begin{align*}
x &= a_{1} (\text{mod } n_{1}) \\
x &= a_{2} (\text{mod } n_{2}) \\
... \\
x &= a_{k} (\text{mod } n_{k})
\end{align*}
And that moreover, all solutions $x$ are congruent modulo $N=\Pi_{i}n_{i}$.

\subsection{Algorithm}

We can generate that $x$ using the following algorithm. First, calculate $N$. Then, for each $i$ we can find $r_{i}$ and $s_{i}$ s.t. 
\begin{equation}
r_{i}n_{i} + \frac{s_{i}N}{n_{i}} = 1
\end{equation}
we do this by using the extended Euclidean algoirthm on $n_{i}$ and $N/n_{i}$. In which case $s_{i}$ is the second element of the output of \texttt{extendedEu}.

We then have a solution $x$ given by 
\begin{equation}
x = \sum_{i=1}^{k}\frac{a_{i}s_{i}N}{n_{i}}
\end{equation}

And we can find the minimal solution by taking mod $N$.

\subsection{Implementation}
We can implement that as follows.

First, a helper to calculate each ith element of the above sum.
\begin{code}
getElement:: Integer -> (Integer, Integer) -> Integer
getElement bigN (a, n) = (a*s*bigN) `div` n
  where s = snd $ extendedEu n (bigN `div` n)
\end{code}

Then the real thing:

\begin{code}
crt:: [(Integer ,Integer)] -> Integer
crt cs = sum (map (getElement n) cs) `mod` n
  where n = product $ map snd cs
\end{code}