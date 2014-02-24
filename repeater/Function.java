/**
 * @param <X> is both the domain and co-domain of the function
 */
public interface Function<X> {
    /**
     * @param x is the input to the function
     * @return the function applied to x
     */
    public X apply(X x);
}
