package pkg.hupp;

public class Test
{
    public static void main(String[] args)
    {
	foo_ =                           // test13 
	    new Foo();                   // test14
	foo_.
	    gurka();                     // test15
    }

    public void print() 
    { 
    }

    public Foo getFoo() 
    { 
	return null;
    }

    private static Foo foo_ = null;      // OK13
}
