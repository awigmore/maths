/**
 * A repeater is a class that, given a Function f,
 * outputs a function in which f is duplicated some number of times.
 * How many times depends on the specific subclass and instance of Repeater
 * @param <X>
 */
@SuppressWarnings("rawtypes")
public abstract class Repeater{
    /** @return a function repeated */
    public abstract <X> Function<X> repeat(final Function<X> f);
    /** @return how many times this repeater repeats */
    int repeatCount(){
        return this.repeat(new Add(1)).apply(0);
    }
    @SuppressWarnings("unchecked")
    public final Function apply(Function f) {
        return this.repeat(f);
    }
    /**
     * @return a repeater that repeats one more time
     */
    static Repeater successor(final Repeater r1) {
        return new Repeater() {
            @Override
            public <X> Function<X> repeat(final Function<X> f) {
               return new Function<X>() {
                @Override
                    public X apply(X x) {
                        return f.apply(r1.repeat(f).apply(x));
                    } 
               };
            }  
        };
    }
    public Repeater successor() {
        return Repeater.successor(this);
    }
    /**
     * @param r1 is a repeater
     * @param r2 is a repeater
     * @return a repeater that repeats how many times r1 repeats +
     * how many times r2 repeats
     */
    static Repeater plus(final Repeater r1, final Repeater r2) {
        return new Repeater() {
            @Override
            public <X> Function<X> repeat(final Function<X> f) {
               return new Function<X>() {
                @Override
                    public X apply(X x) {
                        return r1.repeat(f).apply(r2.repeat(f).apply(x));
                    } 
               };
            }  
        };
    }
    Repeater plus(Repeater r2) {
        return Repeater.plus(this, r2);
    }
    /**
     * @param r1 is a repeater
     * @param r2 is a repeater
     * @return a repeater that repeats how many times r1 repeats *
     * how many times r2 repeats
     */
    static Repeater times(final Repeater r1, final Repeater r2) {
        return new Repeater() {
            @Override
            public <X> Function<X> repeat(final Function<X> f) {
               return new Function<X>() {
                @Override
                    public X apply(X x) {
                        Function<X> f1 = r1.repeat(f);
                        return r2.repeat(f1).apply(x);
                    } 
               };
            }  
        };
    }
    Repeater times(Repeater r2) {
        return Repeater.times(this, r2);
    }
    /**
     * @param r1 is a repeater
     * @param r2 is a repeater
     * @return a repeater that repeats how many times r1 repeats ^
     * how many times r2 repeats
     */
    static Repeater exp(final Repeater r1, final Repeater r2) {
        final Function<Function> f1 = new Function<Function>() {
            @Override
            public Function apply(Function f) {
                return r1.apply(f);
            };
        };
        return new Repeater() {
            @SuppressWarnings("unchecked")
            @Override
            public <X> Function<X> repeat(final Function<X> f) {
                return r2.repeat(f1).apply(f);
            }
        };
    }
    Repeater exp(Repeater r2) {
        return Repeater.exp(this, r2);
    }
}
