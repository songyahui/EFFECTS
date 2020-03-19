# Tvide: an Automated Temporal Verification tool of Integrated Dependent Effects

Existing approaches to temporal verification have either sacrificed compositionality in favor of achieving automation or vice-versa. To exploit the best of both worlds, we present Tvide, a new solution to ensure temporal properties via a Hoare-style verifier and a term rewriting system (t.r.s) on Integrated Dependent Effects. The first contribution is the novel effects capable of integrating value-dependent finite and infinite traces into a single disjunctive type, resulting in more concise and expressive specifications. As a second contribution, by avoiding the complex translation into deterministic automata, our purely algebraic t.r.s efficiently checks the language inclusion, on the mixed inductive (finite traces) and coinductive (infinite traces) definitions. Lastly, we build a prototype system and show the feasibility of our method.

## Online demo

The easiest way to try the code is to use the [web UI](http://loris-5.d2.comp.nus.edu.sg/Effect/index.html?ex=send&type=c&options=sess) written
by [Yahui Song](https://www.comp.nus.edu.sg/~yahuis/).

### To Compile:

```
git clone https://github.com/songyahui/EFFECTS.git
cd EFFECTS
chmod 755 clean 
chmod 755 compile 
./compile
```

### Dependencies:

```
opam switch create 4.07.1
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
./verify src/program/send.c src/program/output.txt
```

### To Clean:

``` 
./clean
```

### Benchmark:

We provide a [Miroc-Benchmark](http://loris-5.d2.comp.nus.edu.sg/Effect/BenchMark.zip) for experiemnts on checking inclusions among regular expressions.

[Arduino]https://create.arduino.cc/projecthub/projects/tags/control

# 


#### ===========This is for myself=========




git ls-files | xargs wc -l

sudo rm -r EFFECTS/

mkdir EFFECTS

sudo cp -r  ~yahui/EFFECTS/* /home/project/public_html/Effect/cgi-bin/EFFECTS

sudo chown yahui:yahui -R /home/project/public_html/Effect/cgi-bin/EFFECTS

cd EFFECTS

chmod 755 clean

chmod 755 compile

./compile

sudo cp trs ../

sudo cp verify ../

cd ..

sudo chown www-data:www-data trs 

sudo chown www-data:www-data verify

yahui:repo

----------------------

cd ../src/

sudo cp ../cgi-bin/EFFECTS/src/effect/* effect/

sudo chown www-data:www-data -R effect/

sudo cp ../cgi-bin/EFFECTS/src/program/* program/

sudo chown www-data:www-data -R program/


