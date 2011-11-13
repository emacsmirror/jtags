/*
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.sf.jtags;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Foo extends java.io.IOException implements Serializable {

    private static Logger TLOG = Logger.getLogger(Foo.class.getName());

    private final String Final = null;

    private static String[] Static = null;

    private final static String FinalStatic = null;

    private File file = null;

    private String name = "This string has the word import in it.";

    private static java.lang.Double number[] = null;

    private long xxx,yyy,zzz;

    private java.io.File[] files1, files2;

    public Foo(java.lang.String name) {
        super(name);
        this.name = name;

        // This comment does not end with a period
        if (name == null) {
            this.name = "Foo";
        }

        try {
            System.out.println("Foo" + xxx + yyy + zzz);
        } catch (Exception e) {
            System.out.println("FooFoo: " + e.getMessage() + files1[0].getName() + files2[0].getAbsoluteFile());
        }
    }

    public static Foo gurka(String apa,
                            Double banan) {
        // This comment contains import java.io.*;
        int index = 0;
        index = apa.indexOf(':');
        java.lang.String newName = String.valueOf(index);
        number[0] = banan;
        Foo newFoo = new Foo(newName.toLowerCase());
        return newFoo;
    }

    public java.lang.String getName() {
        // This comment contains code: name.toUpperCase().trim()
        String result = name;
        String code = "This string contains code: String.valueOf(1)";
        result += " " + Class.class.getName().toUpperCase().toLowerCase();
        return result;
    }

    public Bar getBar() {
        // This comment does end with a period.
        getName().toUpperCase().trim();
        return new Bar(name, this);
    }

    public String toString() {
        IOException ioe = new IOException(this.toString());
        return ((Exception) super.getCause()).toString();
    }
}

class Bar {

    private static Logger TLOG = Logger.getLogger(Bar.class.getName());

    protected String gurkburk = null;

    private net.sf.jtags.Foo foo = null;

    private List<String> theList;

    public Bar(String name, Foo poFoo) {
        this.foo = poFoo;
        TLOG.log(Level.INFO, "Name=" + name);
        TLOG.log(Level.INFO, "Gurkburk=" + gurkburk);
    }

    public void setGurkburk(final String gurkburk) {
        this.gurkburk = gurkburk;
        TLOG.log(Level.INFO, "Gurkburk=" + gurkburk);
    }

    public Foo getFoo() {
        TLOG.log(Level.WARNING, "Returning foo");
        TLOG.log(Level.INFO, "Gurkburk=" + gurkburk);
        return foo.gurka("Apa!", new Double(Double.MAX_VALUE));
    }

    public Foo getFooo() {
        for (int row = 0; row < 10; row++) {
            for (int col = 0; col < 5; col++) {
                System.out.println("Row = " + row + ", col = " + col);
            }
        }
        return getFoo();
    }

    public IOException getException() throws IOException {
        throw getIOException();
    }

    public IOException getIOException() {
        return new IOException("Error!");
    }

    private void longCallList(Foo foo) {
        foo.getBar().getFoo().getBar().getFooo().getBar().getFoo().getBar().getFooo();
    }

    /**
     * Returns a list of strings.
     *
     * @param in Input list.
     * @return Output list.
     */
    public List<String> getStringList(List<String> in) {
        java.util.List<String> myList = in;
        Map<Integer, String> myMap = new HashMap<Integer, String>();
        Iterator<String> iter = in.iterator();
        theList = in;
        System.out.println(myMap.size());

        for (String myString : in) {
            System.out.println("myString = " + myString);
            myString.toLowerCase();
        }

        for (String theString : theList) {
            theString.split(";");
        }

        for (Integer key : myMap.keySet()) {
            myMap.get(key).toUpperCase();
        }

        return myList;
    }

    public String lastInClass() {
        Bar bar = null;
        return foo.getBar().getFoo().getBar().lastInClass();
    }
}

class Axe extends net.sf.jtags.Bar {

    private Thread t = null;

    private Bar bar = null;

    public Axe(final net.sf.jtags.Foo poFoo) {
        super("Axe", poFoo);
        System.out.println("Gurkburk=" + gurkburk + poFoo.getName().trim());
        t = new Thread();
        t.run();
    }

    public void setBarArray(Bar[] bar, Foo[]foo, Axe axe[]) {
        int aaa, bbb, ccc;
        this.bar = bar[0];
        setBar(bar[0]).getFoo().getName().toLowerCase();
        foo[0] = foo[0];
        axe[0] = axe[0];
        aaa = 0;
        bbb = 0;
        ccc = 0;
    }

    public net.sf.jtags.Bar setBar(Bar bar) {
        this.bar = bar;
        return bar;
    }

    public void lastInFile() {
        Thread t = new Thread();
        t.run();
        lastInFile();
    }
}
