import com.github.weisj.darklaf.LafManager;

import javax.swing.*;
import java.awt.*;

public final class ColorChooserDemo {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install();
            JColorChooser.showDialog(null, "Color Chooser with transparency",
                                     Color.RED);
        });
    }
}
