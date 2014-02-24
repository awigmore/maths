/**
 * Add adds n to any number
 */
public class Add implements Function<Integer>{
    int n;
    Add(int n) {
        this.n = n;
    }
    public Integer apply(Integer i) {
        return i + n;
    }
}