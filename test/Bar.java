class Bar                                // OK01 OK11
{
    public String toString() 
    { 
	return "";
    }

    public static Bar instance()         // OK02
    {
	return null;
    }

    public void foo() { }                // OK03 OK04 OK06 OK09
    
    public void foobar() { }

    public void foozarg() { }

    public void afoo() { }

    public void zoofoo() { }

    public void print()              	 
    { 
	foo();                        	 // test06
    }
    
    class Bar_Inn
    {
	public void print() { }          // OK05 if inner classes tagged.

	public void gurka() 
	{ 
	    Bar.                  	 // test01
		instance().       	 // test02
		foo();            	 // test03
	    foo();                	 // test04  
	    print();              	 // test05
	} 
    } 
}
 
class Bar_Par
{
    public void urk() { }
    String dumt;
    //    dumt.g
}
