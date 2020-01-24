# DEMO Flatform
http://loris-5.d2.comp.nus.edu.sg/Effect/index.html?ex=send&type=c&options=sess

# EFFECTS
Automated verification tool for temporal properties

# To Compile:

chmod 755 clean 

chmod 755 compile 

./clean

./compile

git ls-files | xargs wc -l

# Example:

./trs src/effect/ex1.ee src/effect/output.txt > src/effect/output.txt 2>&1

./verify src/program/send.c src/program/output.txt

# Server:

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










