# Automated Timed Temporal Verification

In this work, we study temporal verification of compositional real-time systems, modeled using mutable variables and timed processes. Instead of explicitly manipulating clock variables, several compositional timed behavioral patterns, such as delay, timeout, deadline, are introduced to capture quantitative timing constraints. The idea is to dynamically create clocks and solve constraints on the clocks.
We propose a novel solution for timed verification via a compositional Hoare-style forward verifier and a term rewriting system (TRS) on Timed Effects (TimEffs). We formally define a core language 𝜆t , generalizing the timed processes. Secondly, to capture real-time specifications, we introduce TimEffs, a new effects logic, that extends Kleene Algebra with dependent values and arithmetic constraints. Thirdly, the forward verifier infers temporal behaviors of given 𝜆t programs, expressed in TimEffs. Lastly, we present a purely algebraic TRS, to efficiently prove language inclusions between TimEffs. To demonstrate the feasibility of our proposals, we prototype the verification system; prove its correctness; report on case studies and the experimental results.



## Online demo


### To Compile:

```
git clone https://github.com/songyahui/EFFECTS.git
git checkout timed_effects
cd EFFECTS
chmod 755 clean 
chmod 755 compile 
./compile
```

### Dependencies:

```
opam switch create 4.10.2
eval $(opam env)
sudo apt-get install menhir
sudo apt-get install z3
```

### Examples:

Entailments Checking 

```
./trs src/effect/ex1.ee src/effect/output.txt 
```

Program Verification

```
./verify src/program/coffee.c
```

### To Clean:

``` 
./clean
```

### Benchmark:


# 

