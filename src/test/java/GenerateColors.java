import com.weis.darklaf.color.DarkColorModelHSL;

import java.awt.*;

/**
 * Usd to generate the color swatches for ColorChooser.
 */
public final class GenerateColors {

    public static void main(final String[] args) {
        int cols = 30;
        int rows = 20;
        System.out.println("{");
        int r;
        int g;
        int b;
        for (int i = 1; i < rows + 1; i++) {
            r = g = b = (int) ((i - 1) * 255.0 / (rows - 1));
            System.out.println(r + "," + g + "," + b + ",");
            for (int j = 0; j < cols - 1; j++) {
                var c = colorFromPos(j, i, rows + 1, cols);
                System.out.println(c.getRed() + "," + c.getGreen() + "," + c.getBlue() + ",");
            }
        }
        System.out.println("};");
    }

    private static Color colorFromPos(final int x, final int y, final int height, final int width) {
        int h, s, l;

        l = (int) (100 * y / (double) height);
        s = 100;
        h = (int) (360 * ((double) x / width));
        return new DarkColorModelHSL().getColorFromValues(new int[]{h, s, l});
    }
}
