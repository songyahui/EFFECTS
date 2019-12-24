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

# Server:

sudo rm -r EFFECTS/

mkdir EFFECTS

sudo cp -r  ~yahui/EFFECTS/* /home/project/public_html/Effect/cgi-bin/EFFECTS

sudo chown yahui:yahui -R /home/project/public_html/Effect/cgi-bin/EFFECTS

cd EFFECTS

./compile

sudo cp trs ../

sudo cp verify ../

cd ..

sudo chown www-data:www-data trs 

sudo chown www-data:www-data verify










