void delay (int n) 
    /*
    require (n>=0)/\/\emp
    ensure (n>=0)/\(Delay^n)
    */
{
    if (n == 0) { 
        return;
    }
    else {
        event ("Delay"); 
        delay (n - 1);
    }
} 
 

void turnRed ()
    /*
    require TRUE/\emp
    ensure TRUE /\ Red
    */
{
    event ("Red"); 
    printf("X Ave. - RED; Y Blvd. - RED\n");
}


void turnGreen ()
    /*
    require TRUE/\emp
    ensure TRUE /\ Red
    */
{
    event ("Green"); 
    printf("X Ave. - RED; Y Blvd. - GREEN\n");
}


void turnYellow ()
    /*
    require TRUE/\emp
    ensure TRUE /\ Red
    */
{
    event ("Yellow"); 
    printf("X Ave. - RED; Y Blvd. - YELLOW\n");
}