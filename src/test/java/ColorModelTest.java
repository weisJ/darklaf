import com.weis.darklaf.color.DarkColorModel;
import com.weis.darklaf.color.DarkColorModelCMYK;
import com.weis.darklaf.color.DarkColorModelHSB;
import com.weis.darklaf.color.DarkColorModelHSL;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.util.Arrays;

public class ColorModelTest {

    public static void main(final String[] args) {
        var color = new Color(10, 20, 30);

        var m1 = new DarkColorModel();
        var m2 = new DarkColorModelHSB();
        var m3 = new DarkColorModelHSL();
        var m4 = new DarkColorModelCMYK();
//        test(m1);
        test(m2);
//        test(m3);
//        test(m4);
    }

    private static void test(@NotNull final DarkColorModel model) {
        System.out.println("Testing " + model.toString());
        for (int r = model.getMinimum(0); r < model.getMaximum(0); r++) {
            for (int g = model.getMinimum(1); g < model.getMaximum(1); g++) {
                for (int b = model.getMinimum(2); b < model.getMaximum(2); b++) {
                    var c = new int[]{r, g, b};
                    var interm = model.getColorFromValues(c);
                    var nc = model.getValuesFromColor(interm);
                    if (!Arrays.equals(c, nc)) {
                        throw new RuntimeException(
                                "Not equals " + Arrays.toString(c) + " : " + interm + " : " + Arrays.toString(nc));
                    }
                }
            }
        }
    }
}
