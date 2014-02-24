/**
 * MyRepeater repeats a function n times
 */
public class MyRepeater extends Repeater{
    int n;
    /**
     * @param n is how many times this repeater should repeat functions
     * Note: n >= 0
     */
    MyRepeater(int n) {
        if (n < 0) {
            throw new RuntimeException("n must be >= 0, given: " + n);
        }
        this.n = n;
    }
    /**
     * @param f is the function to be repeated
     * @return the function repeated this.n times
     */
    public <X> Function<X> repeat(final Function<X> f) {
        if(this.n == 0) {
            return new Function<X>() {
                @Override
                public X apply(X x) {
                    return x;
                }     
            };
        }
        else {
            return new Function<X>() {
                @Override
                public X apply(X x) {
                    return f.apply(new MyRepeater(n - 1).repeat(f).apply(x));
                }
            };
        }
    }
}
