package pkg.hupp;

import Bar;

public class Foo                         // OK14
{
    public void print()                  // OK08
    { 
	print();                         // test08
	Bar.instance().foo();            // test09
    }

    public void gurka()                  // OK07 OK15
    {
	Bar barbar;                       // test10 OK10 OK12
	barbar = new Bar();               // test11
	barbar .                          // test12
	    toString();                   // Will bug out because of testcomment

	barbar . 	    toString().toString().toString().toString().toString().toString().toString().toString().toString();

    }
}



