# EFFECTS
Automated verification tool for temporal properties

# To Compile:

chmod 755 clean 

chmod 755 compile 

./clean

./compile

# Example:

./trs src/effect/ex1.ee src/effect/output.txt > src/effect/output.txt 2>&1

# Server:

rm -f /home/project/public_html/Effect/cgi-bin/code

sudo rm -r code

sudo cp -r  ~yahui/hg/EFFECTS/* /home/project/public_html/Effect/cgi-bin/EFFECTS

chown yahui:yahui /


