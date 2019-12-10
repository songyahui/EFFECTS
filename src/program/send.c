void send (int n) 
    /*
    require TRUE/\Emp
    ensure TRUE/\Emp
    */

{

    if (eq(n, 0)) { 
        event ("Done");
    }
    else {
        event ("Send"); 
        send (minus(n, 1));
    }
} 