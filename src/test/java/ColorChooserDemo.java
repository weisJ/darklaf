import com.weis.darklaf.LafManager;

import javax.swing.*;
import java.awt.*;

public final class ColorChooserDemo {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.loadLaf(LafManager.Theme.Dark);
            JColorChooser.showDialog(null, "Color Chooser without transparency",
                                     Color.RED, false);
        });
    }
}
