public class RepeaterTest {
    public static void main(String args[]) {
        Repeater threeTimes = new MyRepeater(3);
        test(threeTimes.repeat(new Add(2)).apply(7) == 13);
        test(threeTimes.repeatCount() == 3);
        Repeater twoTimes = new MyRepeater(2);
        test(twoTimes.successor().repeat(new Add(2)).apply(7) == 13);
        test(threeTimes.plus(twoTimes).repeatCount() == 5);
        test(threeTimes.times(twoTimes).repeatCount() == 6);
        test(threeTimes.exp(twoTimes).repeatCount() == 9);
    }
    public static void test(Boolean b) {
        if (!b) {
            System.out.println("fail");
        }
    }
}
