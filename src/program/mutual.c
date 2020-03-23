#include "primitives.c"

int is_odd (int n);

int is_even(int n)
    /*
    require n>=0 /\ emp
    ensure TRUE /\ (ODD.EVEN)^*._*
    */
    /*
    require n>=0 /\ emp
    ensure TRUE /\ (ODD.EVEN)^(n/2).ODD?
    */
{
    if (n == 0) {
        return 1;
    }
    else {
        event ("ODD");
        return is_odd(n-1);
    }
}
    

int is_odd(int n)
    /*
    require n>=0 /\ emp
    ensure TRUE /\ (EVEN.ODD)^*._*
    */

    /*
    require n>=0 /\ emp
    ensure TRUE /\ (EVEN.ODD)^(n/2).EVEN?
    */
{
    if (n == 0) {
        return 0;
    }
    else{
        event ("EVEN");
        return is_even(n-1);
    }      
}

int main () {
    is_odd (4);
}


//TRUE /\ E.((O.E)^*).(_^*) |- TRUE /\ ((E.O)^*).(_^*)
//
    