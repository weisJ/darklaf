import com.github.weisj.darklaf.color.DarkColorModelHSL;

import java.awt.*;

/**
 * Used to generate the color swatches for ColorChooser.
 */
public final class GenerateColors {

    public static void main(final String[] args) {
        int cols = 30;
        int rows = 15;
        StringBuilder builder = new StringBuilder("{");
        int r;
        int g;
        int b;
        for (int i = 1; i < rows + 1; i++) {
            r = g = b = (int) ((i - 1) * 255.0 / (rows - 1));
            builder.append(r).append(",")
                   .append(g).append(",")
                   .append(b).append(",");
            for (int j = 0; j < cols - 1; j++) {
                Color c = colorFromPos(j, i, rows + 1, cols);
                builder.append(c.getRed()).append(",")
                       .append(c.getGreen()).append(",")
                       .append(c.getBlue()).append(",");
            }
        }
        builder.append("};");
        System.out.println(builder);
    }

    private static Color colorFromPos(final int x, final int y, final int height, final int width) {
        int h, s, l;

        l = (int) (100 * y / (double) height);
        s = 100;
        h = (int) (360 * ((double) x / width));
        return new DarkColorModelHSL().getColorFromValues(new int[]{h, s, l});
    }
}
